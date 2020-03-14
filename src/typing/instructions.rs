use std::iter::FromIterator;


use crate::syntax::instructions::*;


use super::*;
use nom::lib::std::collections::{HashMap};

use ena::unify::{UnificationTable, UnifyKey, EqUnifyValue, InPlace};


use nom::lib::std::convert::TryFrom;

rule!(LoadRule: Load => ParamFuncType, load_rule);

fn load_rule(syntax: &Load, _rule: &LoadRule, _context: &Context) -> WrappedResult<ParamFuncType> {
    let size = if let Some((size, _sx)) = syntax.storage_size {
        size / 8
    } else {
        syntax.valtype.size()
    };
    if 1u32 << syntax.memarg.align > size {
        None?
    } else {
        Ok(ParamFuncType { parameters: vec![Param::Const(ValType::I32)], results: vec![Param::Const(syntax.valtype)] })
    }
}

rule!(StoreRule: Store => ParamFuncType, store_rule);

fn store_rule(
    syntax: &Store,
    _rule: &StoreRule,
    _context: &Context,
) -> WrappedResult<ParamFuncType> {
    let size = if let Some(size) = syntax.storage_size {
        size / 8
    } else {
        syntax.valtype.size()
    };
    if 1u32 << syntax.memarg.align > size {
        None?
    } else {
        Ok(ParamFuncType { parameters: vec![Param::Const(ValType::I32), Param::Const(syntax.valtype)], results: vec![] })
    }
}

// TODO memory grow, size

rule!(
    ConstRule: Const => ParamFuncType,
    |syntax: &Const, _, _| Ok(ParamFuncType { parameters: vec![], results: vec![Param::Const(syntax.valtype())] })
);

rule!(InstrRule: Instr => ParamFuncType, instr_rule);

fn instr_rule(syntax: &Instr, _rule: &InstrRule, context: &Context) -> WrappedResult<ParamFuncType> {
    Ok(match syntax {
        Instr::Const(s) => ConstRule {}.check(s, context)?,
        Instr::Load(s) => LoadRule {}.check(s, context)?,
        Instr::Store(s) => StoreRule {}.check(s, context)?,
        Instr::Block(s) => BlockRule {}.check(s, context)?,
        _ => unimplemented!()
    })
}

rule!(ExprRule { parameters: Vec<ValType>, results: Vec<ValType>, is_const: bool }: Expr => ParamFuncType, expr_rule);

fn expr_rule(syntax: &Expr, rule: &ExprRule, context: &Context) -> WrappedResult<ParamFuncType> {
    check_instruction_seq(rule.parameters.clone(), rule.results.clone(), &syntax.instrs, &context, rule.is_const)
}

rule!(NopRule: Nop => FuncType, |_, _, _| Ok(Default::default()));

rule!(UnreachableRule: Unreachable => ParamFuncType, |_, _, _| Ok(ParamFuncType { parameters: vec![Param::Star], results: vec![Param::Star] }));

rule!(BlockRule: Block => ParamFuncType, block_rule);

fn block_rule(syntax: &Block, rule: &BlockRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.instrs, &inner_context, false)
}

rule!(LoopRule: Loop => ParamFuncType, loop_rule);

fn loop_rule(syntax: &Loop, rule: &LoopRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.instrs, &inner_context, false)
}

rule!(IfElseRule: IfElse => ParamFuncType, if_else_rule);

fn if_else_rule(syntax: &IfElse, rule: &IfElseRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.if_instrs, &inner_context, false)?;
    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.else_instrs, &inner_context, false)
}

rule!(BrRule: Br => ParamFuncType, br_rule);

fn br_rule(syntax: &Br, rule: &BrRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let n = context.labels.len();
    let i = syntax.labelidx as usize;
    if i >= n {
        Err(NoneError)?
    }
    let mut parameters = vec![Param::Star].into_iter().chain(
        Vec::from_iter(context.labels[n - i - 1].map(|v| Param::Const(v)))
    ).collect();

    Ok(ParamFuncType { parameters, results: vec![Param::Star] })
}

// FIXME implement const constraint
fn check_instruction_seq(parameters: Vec<ValType>, results: Vec<ValType>, instrs: &Vec<Instr>, context: &Context, _is_const: bool) -> WrappedResult<ParamFuncType> {
    let mut param_stack = ParamStack::from(parameters.clone());

    for instr in instrs.iter() {
        let ft = InstrRule {}.check(instr, &context)?;
        param_stack = param_stack.merge(ParamStack::from(ft))?;
    }

    let ft = FuncType { parameters: results.clone(), results: vec![] };

    param_stack = param_stack.merge(ParamStack::from(ft.clone()))?;

    if param_stack.stack.is_empty() || param_stack.stack.len() == 1 && param_stack.stack[0] == StackSymbol::ZZ {
        Ok(ParamFuncType {
            parameters: parameters.into_iter().map(|v| Param::Const(v)).collect(),
            results: results.into_iter().map(|v| Param::Const(v)).collect(),
        })
    } else {
        Err(NoneError)?
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Param {
    Const(ValType),
    T(u32),
    Star,
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParamFuncType {
    parameters: Vec<Param>,
    results: Vec<Param>,
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct UnifyValType(u32);

impl UnifyKey for UnifyValType {
    type Value = Option<ValType>;
    fn index(&self) -> u32 { self.0 }
    fn from_index(u: u32) -> Self { UnifyValType(u) }
    fn tag() -> &'static str { "UnifyParam" }
}

impl EqUnifyValue for ValType {}

#[derive(Clone, Debug, Default)]
pub struct ParamStack {
    stack: Vec<StackSymbol<UnifyValType>>,
    utable: UnificationTable<InPlace<UnifyValType>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StackSymbol<T: Sized> {
    Pos(T),
    Neg(T),
    ZZ,
}

impl From<Vec<ValType>> for ParamStack {
    fn from(vec: Vec<ValType>) -> Self {
        Self::from( ParamFuncType { parameters: vec![], results: vec.into_iter().map(|p| Param::Const(p)).collect() })
    }
}

impl From<Vec<Param>> for ParamStack {
    fn from(vec: Vec<Param>) -> Self {
        Self::from( ParamFuncType { parameters: vec![], results: vec })
    }
}

impl From<FuncType> for ParamStack {
    fn from(ft: FuncType) -> Self {
        Self::from(ParamFuncType {
            parameters: ft.parameters.into_iter().map(|p| Param::Const(p)).collect(),
            results: ft.results.into_iter().map(|p| Param::Const(p)).collect(),
        })
    }
}

impl From<ParamFuncType> for ParamStack {
    fn from(pft: ParamFuncType) -> Self {
        let mut param_stack: ParamStack = Default::default();
        let mut remapping: HashMap<u32, UnifyValType> = Default::default();
        let mut elems = pft.parameters.iter().rev().map(|i| (i, false)).chain(
            pft.results.iter().map(|i| (i, true))
        );

        while let Some((elem, d)) = elems.next() {
            let symbol = match elem {
                Param::Const(v) => {
                    let k = param_stack.utable.new_key(Some(*v));
                    if d { StackSymbol::Pos(k) } else { StackSymbol::Neg(k) }
                },
                Param::T(v) => {
                    let k = remapping.get(&v).map_or_else(|| param_stack.utable.new_key(None), |v| *v);
                    remapping.insert(*v, k);
                    if d { StackSymbol::Pos(k) } else { StackSymbol::Neg(k) }
                },
                Param::Star => {
                    if let Some((Param::Star, true)) = elems.next() {
                        StackSymbol::ZZ
                    } else {
                        panic!("The only stack polymorphic functions supported are of the form [*, p0 .. pn] -> [*, r0 .. rm]")
                    }
                }
            };
            param_stack.stack.push(symbol);
        }
        param_stack
    }
}

impl TryFrom<ParamStack> for Vec<Param> {
    type Error = NoneError;

    fn try_from(mut value: ParamStack) -> Result<Self, Self::Error> {
        value.stack.clone().into_iter().map(|symbol| {
            if let StackSymbol::Pos(k) = symbol {
                if let Some(v) = value.utable.probe_value(k) {
                    Ok(Param::Const(v))
                } else {
                    Ok(Param::T(k.0))
                }
            } else {
                Err(NoneError)
            }
        }).collect()
    }
}

impl ParamStack {
    pub fn merge(mut self, mut other: ParamStack) -> Result<Self, NoneError> {
        let mut params_iter = other.stack.iter();

        while let Some(param_symbol) = params_iter.next() {
            match (param_symbol, self.stack.last()) {
                (StackSymbol::Neg(param_k), Some(StackSymbol::Pos(self_k))) => {
                    let p_val = other.utable.probe_value(*param_k);
                    self.utable.unify_var_value(*self_k, p_val).or_else(|_err| Err(NoneError))?;
                    let self_val = self.utable.probe_value(*self_k);
                    other.utable.unify_var_value(*param_k, self_val).or_else(|_err| Err(NoneError))?;
                    self.stack.pop();
                },
                (StackSymbol::Neg(param_k), Some(StackSymbol::ZZ)) => {},
                (StackSymbol::Pos(param_k), _) => {
                    let p_val = other.utable.probe_value(*param_k);
                    self.stack.push(StackSymbol::Pos(*param_k));
                    self.utable.new_key(p_val);
                }
                (StackSymbol::ZZ, _) => {
                    self.stack.clear();
                    self.stack.push(StackSymbol::ZZ);
                    self.utable = Default::default();
                }
                _ => {
                    println!("Premature end of stack");
                    Err(NoneError)?
                }

            }
        }
        Ok(self)
    }
}

//rule!(InstructionSeqRule { start_stack: Vec<ValType>, is_const: bool }: Vec<Instr> => FuncType, instruction_seq_rule);
//
//fn instruction_seq_rule(
//    syntax: &Vec<Instr>,
//    rule: &InstructionSeqRule,
//    context: &Context,
//) -> WrappedResult<FuncType> {
//
//
//    let end_stack = check_instruction_seq(syntax, rule.start_stack.clone(), rule.is_const)?;
//    Ok(FuncType { parameters: rule.start_stack.clone(), results: end_stack, })
//}

// fn check_expr(instr: &Vec<Instr>, is_const: bool, context: &Context, ) -> WrappedResult<Option<ValType>> {
//     let seq = InstructionSeqRule { start_stack: vec![], is_const, }.check(instr, &context)?;
//     if seq.results.len() <= 1 {
//         Ok(seq.results.get(0).cloned())
//     } else {
//         None?
//     }
// }

//fn check_instruction_seq(ft: FuncType, instr: &Vec<Instr>, is_const: bool) -> Option<Vec<ValType>> {
//    //let mut param_stack = ParamStack::from
//    for i in instr {
//        match i {
//            Instr::Const(Const::I32(x)) => stack.push(ValType::I32),
//            Instr::Const(Const::I64(x)) => stack.push(ValType::I64),
//            Instr::Const(Const::F32(x)) => stack.push(ValType::F32),
//            Instr::Const(Const::F64(x)) => stack.push(ValType::F64),
//            _ => unimplemented!()
//        }
//    }
//    Some(stack)
//}

fn check_block_context(label: &Option<ValType>, context: &Context) -> Option<Context> {
    let mut labels = context.labels.clone();
    labels.push_back(label.clone());
    Some(Context {
        types: context.types.clone(),
        funcs: context.funcs.clone(),
        tables: context.tables.clone(),
        mems: context.mems.clone(),
        globals: context.globals.clone(),
        labels,
        locals: context.locals.clone(),
        ret: context.ret.clone(),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_block_rule() {
        let t1 = BlockRule {}
            .check(&Block { result: None, instrs: vec![], }, &Context::empty())
            .unwrap();

        assert_eq!(t1, ParamFuncType { parameters: vec![], results: vec![] });

        let t2 = BlockRule {}
            .check(
                &Block { result: Some(ValType::I64), instrs: vec![Instr::Const(Const::I64(256))] },
                &Context::empty(),
            )
            .unwrap();

        assert_eq!(t2, ParamFuncType { parameters: vec![], results: vec![Param::Const(ValType::I64)] });
    }

    #[test]
    fn test_stack_merge() {
        {
            let stack = ParamStack::from(vec![Param::Const(ValType::I32), Param::Const(ValType::I64)]);
            let ft = ParamFuncType {
                parameters: vec![Param::Const(ValType::I32), Param::Const(ValType::I64)],
                results: vec![],
            };
            let other = ParamStack::from(ft);
            let merged = stack.merge(other).unwrap();

            assert_eq!(Vec::try_from(merged).unwrap(), vec![]);
        }

        {
            let stack = ParamStack::from(vec![Param::T(0), Param::Const(ValType::I64)]);
            let ft = ParamFuncType {
                parameters: vec![Param::Const(ValType::I32), Param::Const(ValType::I64)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(ParamStack::from(ft)).unwrap()).unwrap(), vec![]);
        }

        {
            let stack = ParamStack::from(vec![Param::T(0), Param::Const(ValType::I32)]);
            let ft = ParamFuncType {
                parameters: vec![Param::Const(ValType::I32)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(ParamStack::from(ft)).unwrap()).unwrap(), vec![Param::T(0)]);
        }

        {
            let stack = ParamStack::from(vec![Param::T(0), Param::T(0)]);
            let ft = ParamFuncType {
                parameters: vec![Param::Const(ValType::I32)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(ParamStack::from(ft)).unwrap()).unwrap(), vec![Param::Const(ValType::I32)]);
        }
    }
}

