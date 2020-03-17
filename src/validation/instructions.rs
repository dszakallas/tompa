use std::iter::FromIterator;
use crate::syntax::instructions::*;
use super::*;
use std::collections::{HashMap};
use ena::unify::{UnificationTable, UnifyKey, EqUnifyValue, InPlace};
use std::convert::TryFrom;

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
        Ok(ParamFuncType { parameters: vec![ParamType::Const(ValType::I32)], results: vec![ParamType::Const(syntax.valtype)] })
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
        Ok(ParamFuncType { parameters: vec![ParamType::Const(ValType::I32), ParamType::Const(syntax.valtype)], results: vec![] })
    }
}

// TODO memory grow, size

rule!(
    ConstRule: Const => ParamFuncType,
    |syntax: &Const, _, _| Ok(ParamFuncType { parameters: vec![], results: vec![ParamType::Const(syntax.valtype())] })
);

rule!(InstrRule: Instr => ParamFuncType, instr_rule);

fn instr_rule(syntax: &Instr, _rule: &InstrRule, context: &Context) -> WrappedResult<ParamFuncType> {
    Ok(match syntax {
        Instr::Const(s) => ConstRule {}.check(s, context)?,
        Instr::Load(s) => LoadRule {}.check(s, context)?,
        Instr::Store(s) => StoreRule {}.check(s, context)?,
        Instr::Nop(s) => NopRule {}.check(s, context)?,
        Instr::Unreachable(s) => UnreachableRule {}.check(s, context)?,
        Instr::Block(s) => BlockRule {}.check(s, context)?,
        Instr::Loop(s) => LoopRule {}.check(s, context)?,
        Instr::IfElse(s) => IfElseRule {}.check(s, context)?,
    })
}

rule!(NopRule: Nop => ParamFuncType, |_, _, _| Ok(Default::default()));

rule!(
    UnreachableRule: Unreachable => ParamFuncType,
    |_, _, _| Ok(ParamFuncType { parameters: vec![ParamType::Star], results: vec![ParamType::Star] })
);

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

fn br_rule(syntax: &Br, _rule: &BrRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let label_val = check_label(syntax.labelidx, context)?;
    let parameters = vec![ParamType::Star].into_iter()
        .chain(label_val.into_iter().map(|v| ParamType::Const(v)))
        .collect();

    Ok(ParamFuncType { parameters, results: vec![ParamType::Star] })
}

rule!(BrIfRule: BrIf => ParamFuncType, br_if_rule);

fn br_if_rule(syntax: &BrIf, _rule: &BrIfRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let label_val = check_label(syntax.labelidx, context)?;
    let br_r = label_val.map(|v| ParamType::Const(v));
    let parameters = br_r.iter().cloned()
        .chain(vec![ParamType::Const(ValType::I32)].into_iter())
        .collect();
    let results = Vec::from_iter(br_r.into_iter());

    Ok(ParamFuncType { parameters, results })
}

rule!(BrTableRule: BrTable => ParamFuncType, br_table_rule);

fn br_table_rule(syntax: &BrTable, rule: &BrTableRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let n = syntax.labelidxs.len();
    if n == 0 {
        return Err(NoneError)?
    }
    let label_val = check_label(syntax.labelidxs[n - 1], context)?;
    for idx in &syntax.labelidxs[..n-1] {
        let other_label_val = check_label(*idx, context)?;
        if label_val != other_label_val {
            return Err(NoneError)?
        }
    }
    let br_r = label_val.map(|v| ParamType::Const(v));
    let parameters = vec![ParamType::Star].into_iter()
        .chain(br_r.into_iter())
        .chain(vec![ParamType::Const(ValType::I32)])
        .collect();
    Ok(ParamFuncType { parameters, results: vec![ParamType::Star] })
}

rule!(ReturnRule: Return => ParamFuncType, return_rule);

fn return_rule(syntax: &Return, rule: &ReturnRule, context: &Context) -> WrappedResult<ParamFuncType> {
    if let Some(ret) = context.ret {
        let parameters = vec![ParamType::Star].into_iter()
            .chain(ret.into_iter().map(|v| ParamType::Const(v)))
            .collect();
        return Ok(ParamFuncType { parameters, results: vec![ParamType::Star] })
    };
    Err(NoneError)?

}

rule!(CallRule: Call => ParamFuncType, call_rule);

fn call_rule(syntax: &Call, rule: &CallRule, context: &Context) -> WrappedResult<ParamFuncType> {
    let n = context.funcs.len();
    let i = syntax.fidx as usize;
    if i >= n {
        return Err(NoneError)?
    }
    Ok(ParamFuncType::from(context.funcs[i].clone()))
}

rule!(CallIndirectRule: CallIndirect => ParamFuncType, call_indirect_rule);

fn call_indirect_rule(syntax: &CallIndirect, rule: &CallIndirectRule, context: &Context) -> WrappedResult<ParamFuncType> {
    if context.tables.len() == 0 {
        return Err(NoneError)?
    }

    let n = context.types.len();
    let i = syntax.tidx as usize;
    if i >= n {
        return Err(NoneError)?
    }

    let mut res = ParamFuncType::from(context.types[i].clone());
    res.parameters.push(ParamType::Const(ValType::I32));

    Ok(res)
}

fn check_label(labelidx: LabelIdx, context: &Context) -> WrappedResult<Option<ValType>> {
    let n = context.labels.len();
    let i = labelidx as usize;
    if i >= n {
        Err(NoneError)?
    }
    Ok(context.labels[n - i - 1])
}

// FIXME implement const constraint
pub(in crate::validation) fn check_instruction_seq(parameters: Vec<ValType>, results: Vec<ValType>, instrs: &Vec<Instr>, context: &Context, _is_const: bool) -> WrappedResult<ParamFuncType> {
    let mut param_stack = OpStack::from(parameters.clone());

    for instr in instrs.iter() {
        let ft = InstrRule {}.check(instr, &context)?;
        param_stack = param_stack.merge(OpStack::from(ft))?;
    }

    let ft = FuncType { parameters: results.clone(), results: vec![] };

    param_stack = param_stack.merge(OpStack::from(ft.clone()))?;

    if param_stack.stack.is_empty() || param_stack.stack.len() == 1 && param_stack.stack[0] == StackSymbol::ZZ {
        Ok(ParamFuncType {
            parameters: parameters.into_iter().map(|v| ParamType::Const(v)).collect(),
            results: results.into_iter().map(|v| ParamType::Const(v)).collect(),
        })
    } else {
        Err(NoneError)?
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParamType {
    Const(ValType),
    T(u32),
    Star,
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParamFuncType {
    parameters: Vec<ParamType>,
    results: Vec<ParamType>,
}

impl From<FuncType> for ParamFuncType {
    fn from(ft: FuncType) -> Self {
        let mut pft: ParamFuncType = Default::default();
        for vt in ft.parameters.iter() {
            pft.parameters.push(ParamType::Const(*vt))
        }
        for vt in ft.results.iter() {
            pft.results.push(ParamType::Const(*vt))
        }
        pft
    }
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
pub struct OpStack {
    stack: Vec<StackSymbol<UnifyValType>>,
    utable: UnificationTable<InPlace<UnifyValType>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StackSymbol<T: Sized> {
    Pos(T),
    Neg(T),
    ZZ,
}

impl From<Vec<ValType>> for OpStack {
    fn from(vec: Vec<ValType>) -> Self {
        Self::from( ParamFuncType { parameters: vec![], results: vec.into_iter().map(|p| ParamType::Const(p)).collect() })
    }
}

impl From<Vec<ParamType>> for OpStack {
    fn from(vec: Vec<ParamType>) -> Self {
        Self::from( ParamFuncType { parameters: vec![], results: vec })
    }
}

impl From<FuncType> for OpStack {
    fn from(ft: FuncType) -> Self {
        Self::from(ParamFuncType {
            parameters: ft.parameters.into_iter().map(|p| ParamType::Const(p)).collect(),
            results: ft.results.into_iter().map(|p| ParamType::Const(p)).collect(),
        })
    }
}

impl From<ParamFuncType> for OpStack {
    fn from(pft: ParamFuncType) -> Self {
        let mut param_stack: OpStack = Default::default();
        let mut remapping: HashMap<u32, UnifyValType> = Default::default();
        let mut elems = pft.parameters.iter().rev().map(|i| (i, false)).chain(
            pft.results.iter().map(|i| (i, true))
        );

        while let Some((elem, d)) = elems.next() {
            let symbol = match elem {
                ParamType::Const(v) => {
                    let k = param_stack.utable.new_key(Some(*v));
                    if d { StackSymbol::Pos(k) } else { StackSymbol::Neg(k) }
                },
                ParamType::T(v) => {
                    let k = remapping.get(&v).map_or_else(|| param_stack.utable.new_key(None), |v| *v);
                    remapping.insert(*v, k);
                    if d { StackSymbol::Pos(k) } else { StackSymbol::Neg(k) }
                },
                ParamType::Star => {
                    if let Some((ParamType::Star, true)) = elems.next() {
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

impl TryFrom<OpStack> for Vec<ParamType> {
    type Error = NoneError;

    fn try_from(mut value: OpStack) -> Result<Self, Self::Error> {
        value.stack.clone().into_iter().map(|symbol| {
            if let StackSymbol::Pos(k) = symbol {
                if let Some(v) = value.utable.probe_value(k) {
                    Ok(ParamType::Const(v))
                } else {
                    Ok(ParamType::T(k.0))
                }
            } else {
                Err(NoneError)
            }
        }).collect()
    }
}

impl OpStack {
    pub fn merge(mut self, mut other: OpStack) -> Result<Self, NoneError> {
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
            .check(&Block { result: None, instrs: vec![], }, &Default::default())
            .unwrap();

        assert_eq!(t1, ParamFuncType { parameters: vec![], results: vec![] });

        let t2 = BlockRule {}
            .check(
                &Block { result: Some(ValType::I64), instrs: vec![Instr::Const(Const::I64(256))] },
                &Default::default(),
            )
            .unwrap();

        assert_eq!(t2, ParamFuncType { parameters: vec![], results: vec![ParamType::Const(ValType::I64)] });
    }

    #[test]
    fn test_stack_merge() {
        {
            let stack = OpStack::from(vec![ParamType::Const(ValType::I32), ParamType::Const(ValType::I64)]);
            let ft = ParamFuncType {
                parameters: vec![ParamType::Const(ValType::I32), ParamType::Const(ValType::I64)],
                results: vec![],
            };
            let other = OpStack::from(ft);
            let merged = stack.merge(other).unwrap();

            assert_eq!(Vec::try_from(merged).unwrap(), vec![]);
        }

        {
            let stack = OpStack::from(vec![ParamType::T(0), ParamType::Const(ValType::I64)]);
            let ft = ParamFuncType {
                parameters: vec![ParamType::Const(ValType::I32), ParamType::Const(ValType::I64)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(OpStack::from(ft)).unwrap()).unwrap(), vec![]);
        }

        {
            let stack = OpStack::from(vec![ParamType::T(0), ParamType::Const(ValType::I32)]);
            let ft = ParamFuncType {
                parameters: vec![ParamType::Const(ValType::I32)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(OpStack::from(ft)).unwrap()).unwrap(), vec![ParamType::T(0)]);
        }

        {
            let stack = OpStack::from(vec![ParamType::T(0), ParamType::T(0)]);
            let ft = ParamFuncType {
                parameters: vec![ParamType::Const(ValType::I32)],
                results: vec![],
            };

            assert_eq!(Vec::try_from(stack.merge(OpStack::from(ft)).unwrap()).unwrap(), vec![ParamType::Const(ValType::I32)]);
        }
    }
}

