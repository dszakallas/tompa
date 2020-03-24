use std::iter::FromIterator;
use crate::syntax::*;
use std::collections::{HashMap};
use ena::unify::{UnificationTable, UnifyKey, EqUnifyValue, InPlace};
use std::convert::TryFrom;

use phf::phf_map;
use super::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Load {
    pub valtype: ValType,
    pub storage_size: Option<(u32, Sx)>
}

macro_rules! def_mem_load_instr_rule_multi {
    ($($instr:ty: $valtype:expr, $storage_size:expr, $key: expr);*) => {
        $(
            static MEM_LOAD_TYPE_INFO: phf::Map<&str, Load> = phf_map! {
                $key => Load { valtype: $valtype, storage_size: $storage_size }
            };
        )*
        $(
            def_rule!(Context |- $instr => ParamFuncType, |syntax:&$instr, _, context: &Context| -> WrappedResult<ParamFuncType> {
                check_load(&syntax.memarg, &MEM_LOAD_TYPE_INFO[$key], context)
            });
        )*
    }
}

def_mem_load_instr_rule_multi! {
    lsinstr!(i32load, Keyword::I32Load, Load, ValType::I32, None, 4);
lsinstr!(i64load, Keyword::I64Load, Load, ValType::I64, None, 8);
lsinstr!(f32load, Keyword::F32Load, Load, ValType::F32, None, 4);
lsinstr!(f64load, Keyword::F64Load, Load, ValType::F64, None, 8);
lsinstr!(i32load8_s, Keyword::I32Load8S, Load, ValType::I32, Some((8, Sx::S)), 1);
lsinstr!(i32load8_u, Keyword::I32Load8U, Load, ValType::I32, Some((8, Sx::U)), 1);
lsinstr!(i32load16_s, Keyword::I32Load16S, Load, ValType::I32, Some((16, Sx::S)), 2);
lsinstr!(i32load16_u, Keyword::I32Load16U, Load, ValType::I32, Some((16, Sx::U)), 2);
lsinstr!(i64load8_s, Keyword::I64Load8S, Load, ValType::I64, Some((8, Sx::S)), 1);
lsinstr!(i64load8_u, Keyword::I64Load8U, Load, ValType::I64, Some((8, Sx::U)), 1);
lsinstr!(i64load16_s, Keyword::I64Load16S, Load, ValType::I64, Some((16, Sx::S)), 2);
lsinstr!(i64load16_u, Keyword::I64Load16U, Load, ValType::I64, Some((16, Sx::U)), 2);
lsinstr!(i64load32_s, Keyword::I64Load32S, Load, ValType::I64, Some((32, Sx::S)), 4);
lsinstr!(i64load32_u, Keyword::I64Load32U, Load, ValType::I64, Some((32, Sx::U)), 4);
lsinstr!(i32store, Keyword::I32Store, Store, ValType::I32, None, 4);
lsinstr!(i64store, Keyword::I64Store, Store, ValType::I64, None, 8);
lsinstr!(f32store, Keyword::F32Store, Store, ValType::F32, None, 4);
lsinstr!(f64store, Keyword::F64Store, Store, ValType::F64, None, 8);
lsinstr!(i32store8, Keyword::I32Store8, Store, ValType::I32, Some(8), 1);
lsinstr!(i32store16, Keyword::I32Store16, Store, ValType::I32, Some(16), 2);
lsinstr!(i64store8, Keyword::I64Store8, Store, ValType::I64, Some(8), 1);
lsinstr!(i64store16, Keyword::I64Store16, Store, ValType::I64, Some(16), 2);
lsinstr!(i64store32, Keyword::I64Store32, Store, ValType::I64, Some(32), 4);
    I32Load: "i32.load" -> Load { valtype: ValType }
    I32Load: ValType::I32, None, "i32.load"
}


// def_rule!(Context |- Load => ParamFuncType, load_rule);
//
fn check_load(arg: &Memarg, type_info: &Load, _context: &Context) -> WrappedResult<ParamFuncType> {
    let size = if let Some((size, _sx)) = type_info.storage_size {
        size / 8
    } else {
        type_info.valtype.size()
    };
    if 1u32 << arg.align > size {
        None?
    } else {
        Ok(ParamFuncType { parameters: vec![ParamType::Const(ValType::I32)], results: vec![ParamType::Const(type_info.valtype)] })
    }
}
//
// def_rule!(StoreRule: Store => ParamFuncType, store_rule);
//
// fn store_rule(
//     syntax: &Store,
//     _rule: &StoreRule,
//     _context: &Context,
// ) -> WrappedResult<ParamFuncType> {
//     let size = if let Some(size) = syntax.storage_size {
//         size / 8
//     } else {
//         syntax.valtype.size()
//     };
//     if 1u32 << syntax.memarg.align > size {
//         None?
//     } else {
//         Ok(ParamFuncType { parameters: vec![ParamType::Const(ValType::I32), ParamType::Const(syntax.valtype)], results: vec![] })
//     }
// }

// TODO memory grow, size

def_rule!(
    Context |- Const => ParamFuncType,
    |syntax: &Const, _, _| Ok(ParamFuncType { parameters: vec![], results: vec![ParamType::Const(syntax.valtype())] })
);
//
// def_rule!(UnopRule: Unop => ParamFuncType, unop_rule);
//
// fn unop_rule(syntax: &Unop, _rule: &UnopRule, context: &Context) -> WrappedResult<ParamFuncType> {
//     Ok(ParamFuncType { parameters: vec![valtype], results: vec![valtype] })
// }

def_rule!(Context |- Instr => ParamFuncType, instr_rule);

fn instr_rule(syntax: &Instr, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    Ok(match syntax {
        Instr::Const(s) => s.check(&(), context)?,
        //Instr::Load(s) => s.check(&(), context)?,
        //Instr::Store(s) => s.check(&(), context)?,
        Instr::Nop(s) => s.check(&(), context)?,
        Instr::Unreachable(s) => s.check(&(), context)?,
        Instr::Block(s) => s.check(&(), context)?,
        Instr::Loop(s) => s.check(&(), context)?,
        Instr::IfElse(s) => s.check(&(), context)?,
        _ => unimplemented!(),
    })
}

def_rule!(Context |- Nop => ParamFuncType, |_, _, _| Ok(Default::default()));

def_rule!(
    Context |- Unreachable => ParamFuncType,
    |_, _, _| Ok(ParamFuncType { parameters: vec![ParamType::Star], results: vec![ParamType::Star] })
);

def_rule!(Context |- Block => ParamFuncType, block_rule);

fn block_rule(syntax: &Block, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.instrs, &inner_context, false)
}

def_rule!(Context |- Loop => ParamFuncType, loop_rule);

fn loop_rule(syntax: &Loop, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.instrs, &inner_context, false)
}

def_rule!(Context |- IfElse => ParamFuncType, if_else_rule);

fn if_else_rule(syntax: &IfElse, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let inner_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax))?;

    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.if_instrs, &inner_context, false)?;
    check_instruction_seq(vec![], Vec::from_iter(syntax.result), &syntax.else_instrs, &inner_context, false)
}

def_rule!(Context |- Br => ParamFuncType, br_rule);

fn br_rule(syntax: &Br, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let label_val = check_label(syntax.labelidx, context)?;
    let parameters = vec![ParamType::Star].into_iter()
        .chain(label_val.into_iter().map(|v| ParamType::Const(v)))
        .collect();

    Ok(ParamFuncType { parameters, results: vec![ParamType::Star] })
}

def_rule!(Context |- BrIf => ParamFuncType, br_if_rule);

fn br_if_rule(syntax: &BrIf, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let label_val = check_label(syntax.labelidx, context)?;
    let br_r = label_val.map(|v| ParamType::Const(v));
    let parameters = br_r.iter().cloned()
        .chain(vec![ParamType::Const(ValType::I32)].into_iter())
        .collect();
    let results = Vec::from_iter(br_r.into_iter());

    Ok(ParamFuncType { parameters, results })
}

def_rule!(Context |- BrTable => ParamFuncType, br_table_rule);

fn br_table_rule(syntax: &BrTable, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
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

def_rule!(Context |- Return => ParamFuncType, return_rule);

fn return_rule(_syntax: &Return, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    if let Some(ret) = context.ret {
        let parameters = vec![ParamType::Star].into_iter()
            .chain(ret.into_iter().map(|v| ParamType::Const(v)))
            .collect();
        return Ok(ParamFuncType { parameters, results: vec![ParamType::Star] })
    };
    Err(NoneError)?

}

def_rule!(Context |- Call => ParamFuncType, call_rule);

fn call_rule(syntax: &Call, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
    let n = context.funcs.len();
    let i = syntax.fidx as usize;
    if i >= n {
        return Err(NoneError)?
    }
    Ok(ParamFuncType::from(context.funcs[i].clone()))
}

def_rule!(Context |- CallIndirect => ParamFuncType, call_indirect_rule);

fn call_indirect_rule(syntax: &CallIndirect, _param: &(), context: &Context) -> WrappedResult<ParamFuncType> {
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
        let ft = instr.check(&(), &context)?;
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
                (StackSymbol::Neg(_param_k), Some(StackSymbol::ZZ)) => {},
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

    use crate::syntax::ValType::*;

    #[test]
    fn test_block_rule() {
        let t1 = Block { result: None, instrs: vec![] }
            .check(&(), &Default::default()).unwrap();

        assert_eq!(t1, ParamFuncType { parameters: vec![], results: vec![] });

        let t2 = Block { result: Some(I64), instrs: vec![Instr::Const(Const::I64(256))] }
            .check(&(), &Default::default()).unwrap();

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

