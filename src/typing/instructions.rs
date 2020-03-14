use std::iter::FromIterator;


use crate::syntax::instructions::*;


use super::*;
use nom::lib::std::collections::{HashMap};

use ena::unify::{UnificationTable, UnifyKey, EqUnifyValue, InPlace};


use nom::lib::std::convert::TryFrom;

rule!(LoadRule: Load => FuncType, load_rule);

fn load_rule(syntax: &Load, _rule: &LoadRule, _context: &Context) -> WrappedResult<FuncType> {
    let size = if let Some((size, _sx)) = syntax.storage_size {
        size / 8
    } else {
        syntax.valtype.size()
    };
    if 1u32 << syntax.memarg.align > size {
        None?
    } else {
        Ok(FuncType { parameters: vec![ValType::I32], results: vec![syntax.valtype] })
    }
}

rule!(StoreRule: Store => FuncType, store_rule);

fn store_rule(
    syntax: &Store,
    _rule: &StoreRule,
    _context: &Context,
) -> WrappedResult<FuncType> {
    let size = if let Some(size) = syntax.storage_size {
        size / 8
    } else {
        syntax.valtype.size()
    };
    if 1u32 << syntax.memarg.align > size {
        None?
    } else {
        Ok(FuncType { parameters: vec![ValType::I32, syntax.valtype], results: vec![] })
    }
}

// TODO memory grow, size

rule!(
    ConstRule: Const => FuncType,
    |syntax: &Const, _, _| Ok(FuncType { parameters: vec![], results: vec![syntax.valtype()] })
);


rule!(InstrRule: Instr => FuncType, instr_rule);

fn instr_rule(syntax: &Instr, _rule: &InstrRule, context: &Context) -> WrappedResult<FuncType> {
    Ok(match syntax {
        Instr::Const(s) => ConstRule {}.check(s, context)?,
        Instr::Load(s) => LoadRule {}.check(s, context)?,
        Instr::Store(s) => StoreRule {}.check(s, context)?,
        Instr::Block(s) => BlockRule {}.check(s, context)?,
        _ => unimplemented!()
    })
}

rule!(ExprRule { parameters: Vec<ValType>, results: Vec<ValType>, is_const: bool }: Expr => FuncType, expr_rule);

fn expr_rule(syntax: &Expr, rule: &ExprRule, context: &Context) -> WrappedResult<FuncType> {
    check_instruction_seq(rule.parameters.clone(), rule.results.clone(), &syntax.instrs, &context, rule.is_const)
}

rule!(BlockRule: Block => FuncType, block_rule);

fn block_rule(syntax: &Block, rule: &BlockRule, context: &Context) -> WrappedResult<FuncType> {
    let block_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.instrs, &block_context, false)
}

// FIXME implement const constraint
fn check_instruction_seq(parameters: Vec<ValType>, results: Vec<ValType>, instrs: &Vec<Instr>, context: &Context, _is_const: bool) -> WrappedResult<FuncType> {
    let mut param_stack = ParamStack::from(parameters);

    for instr in instrs.iter() {
        let ft = InstrRule {}.check(instr, &context)?;
        param_stack = param_stack.merge(ParamStack::from(ft))?;
    }

    let ft = FuncType { parameters: results, results: vec![] };

    param_stack = param_stack.merge(ParamStack::from(ft.clone()))?;

    if param_stack.stack.is_empty() {
        Ok(ft)
    } else {
        Err(NoneError)?
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Param {
    Const(ValType),
    T(u32)
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

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Diff {
    Add,
    Rem
}

#[derive(Clone, Debug, Default)]
pub struct ParamStack {
    stack: Vec<(UnifyValType, Diff)>,
    utable: UnificationTable<InPlace<UnifyValType>>,
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
        let elems = pft.parameters.iter().rev().map(|i| (i, Diff::Rem)).chain(
            pft.results.iter().map(|i| (i, Diff::Add))
        );

        for (elem, d) in elems {
            let k = match elem {
                Param::Const(v) => param_stack.utable.new_key(Some(*v)),
                Param::T(v) => {
                    let k = remapping.get(&v).map_or_else(|| param_stack.utable.new_key(None), |v| *v);
                    remapping.insert(*v, k);
                    k
                },
            };
            param_stack.stack.push((k, d));
        }
        param_stack
    }
}

impl TryFrom<ParamStack> for Vec<Param> {
    type Error = NoneError;

    fn try_from(mut value: ParamStack) -> Result<Self, Self::Error> {
        value.stack.clone().into_iter().map(|(k, d)| {
            if d != Diff::Add {
                return Err(NoneError);
            }
            if let Some(v) = value.utable.probe_value(k) {
                Ok(Param::Const(v))
            } else {
                Ok(Param::T(k.0))
            }
        }).collect()
    }
}

impl ParamStack {
    pub fn merge(mut self, mut other: ParamStack) -> Result<Self, NoneError> {
        let mut params_iter = other.stack.iter();

        while let Some((param_k, d)) = params_iter.next() {
            let p_val = other.utable.probe_value(*param_k);
            match d {
                Diff::Rem => {
                    let (self_k, d1) = self.stack.pop()?;
                    if d1 != Diff::Add {
                        return Err(NoneError)
                    }
                    self.utable.unify_var_value(self_k, p_val).or_else(|_err| Err(NoneError))?;
                    let self_val = self.utable.probe_value(self_k);
                    other.utable.unify_var_value(*param_k, self_val).or_else(|_err| Err(NoneError))?;
                }
                Diff::Add => {
                    self.stack.push((*param_k, Diff::Add));
                    if p_val == None {
                        self.utable.new_key(p_val);
                    }
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

        assert_eq!(t1, FuncType { parameters: vec![], results: vec![] });

        let t2 = BlockRule {}
            .check(
                &Block { result: Some(ValType::I64), instrs: vec![Instr::Const(Const::I64(256))] },
                &Context::empty(),
            )
            .unwrap();

        assert_eq!(t2, FuncType { parameters: vec![], results: vec![ValType::I64] });
    }

    #[test]
    fn test_stack_merge() {
        {
            let stack = ParamStack::from(vec![Param::Const(ValType::I32), Param::Const(ValType::I64)]);
            let ft = ParamFuncType {
                parameters: vec![Param::Const(ValType::I32), Param::Const(ValType::I64)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(ParamStack::from(ft)).unwrap()).unwrap(), vec![]);
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






//        {
//            let stack: Stack = Default::default();
//            let parameters = VecDeque::from(vec![]);
//            let results = VecDeque::from(vec![VpValType::The(ValType::I64)]);
//
//            assert_eq!(
//                stack.merge(SpFuncType::VpFuncType(VpFuncType { parameters, results })),
//                Ok(Stack::from(vec![SpValType::One(VpValType::The(ValType::I64))]))
//            );
//        }
//
//        {
//            let stack: Stack = Stack::from(vec![SpValType::One(VpValType::The(ValType::I32))]);
//            let parameters = VecDeque::from(vec![VpValType::The(ValType::I32)]);
//            let results = VecDeque::from(vec![VpValType::The(ValType::I64)]);
//
//            assert_eq!(
//                stack.merge(SpFuncType::VpFuncType(VpFuncType { parameters, results })),
//                Ok(Stack::from(vec![SpValType::One(VpValType::The(ValType::I64))]))
//            );
//
//        }
//
//        {
//            let stack: Stack = Stack::from(vec![SpValType::One(VpValType::The(ValType::I32))]);
//            let parameters = VecDeque::from(vec![VpValType::T]);
//            let results = VecDeque::from(vec![VpValType::The(ValType::I64)]);
//
//            assert_eq!(
//                stack.merge(SpFuncType::VpFuncType(VpFuncType { parameters, results })),
//                Ok(Stack::from(vec![SpValType::One(VpValType::The(ValType::I64))]))
//            );
//
//        }
////        let stack = Stack { inner: vec![SpValType::One(VpValType::The(ValType::I32))] };
//        let parameters = VecDeque::from(vec![VpValType::The(ValType::I32)])
//
//        stack.merge(SpFuncType::VpFuncType(VpFuncType { parameters, results }))
    }
}

