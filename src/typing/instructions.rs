use std::iter::FromIterator;
use std::ops::Add;

use crate::syntax::instructions::*;
use crate::syntax::types::*;

use super::*;

mod numeric {
    use super::*;

    #[cfg(test)]
    mod test {
        use super::*;
    }

    rule!(
        ConstRule: Const => FuncType,
        |syntax: &Const, _, _| Ok(match syntax {
                Const::I32(_) => FuncType { parameters: vec![], results: vec![ValType::I32] },
                Const::I64(_) => FuncType { parameters: vec![], results: vec![ValType::I64] },
                Const::F32(_) => FuncType { parameters: vec![], results: vec![ValType::F32] },
                Const::F64(_) => FuncType { parameters: vec![], results: vec![ValType::F64] }
            }
        )
    );
}

mod parametric {
    use super::*;
}

mod variable {
    use super::*;
}

mod memory {
    use super::*;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_block_rule() {
        let t1 = BlockRule {}
            .check(
                &Block {
                    result: None,
                    instrs: vec![],
                },
                &Context::empty(),
            )
            .unwrap();

        assert_eq!(
            t1,
            FuncType {
                parameters: vec![],
                results: vec![]
            }
        );

        let t2 = BlockRule {}
            .check(
                &Block {
                    result: Some(ValType::I64),
                    instrs: vec![Instr::Const(Const::I64(256))],
                },
                &Context::empty(),
            )
            .unwrap();

        assert_eq!(
            t2,
            FuncType {
                parameters: vec![],
                results: vec![ValType::I64]
            }
        );
    }
}

rule!(ExprRule { is_const: bool }: Expr => Option<ValType>, expr_rule);

fn expr_rule(syntax: &Expr, rule: &ExprRule, context: &Context) -> WrappedResult<Option<ValType>> {
    let res = check_expr(&syntax.instr, rule.is_const, context)?;
    Ok(res)
}

rule!(BlockRule: Block => FuncType, block_rule);

fn block_rule(syntax: &Block, rule: &BlockRule, context: &Context) -> WrappedResult<FuncType> {
    let block_context =
        check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;
    let result = check_expr(&syntax.instrs, false, context)?;

    if result == syntax.result {
        Ok(FuncType {
            parameters: vec![],
            results: Vec::from_iter(result.into_iter()),
        })
    } else {
        None?
    }
}

rule!(InstructionSeqRule { start_stack: Vec<ValType>, is_const: bool }: Vec<Instr> => FuncType, instruction_seq_rule);

fn instruction_seq_rule(
    syntax: &Vec<Instr>,
    rule: &InstructionSeqRule,
    context: &Context,
) -> WrappedResult<FuncType> {
    let end_stack = check_instruction_seq(syntax, rule.start_stack.clone(), rule.is_const)?;
    Ok(FuncType {
        parameters: rule.start_stack.clone(),
        results: end_stack,
    })
}

fn check_expr(
    instr: &Vec<Instr>,
    is_const: bool,
    context: &Context,
) -> WrappedResult<Option<ValType>> {
    let seq = InstructionSeqRule {
        start_stack: vec![],
        is_const,
    }
    .check(instr, &context)?;
    if seq.results.len() <= 1 {
        Ok(seq.results.get(0).cloned())
    } else {
        None?
    }
}

fn check_instruction_seq(
    instr: &Vec<Instr>,
    start_stack: Vec<ValType>,
    is_const: bool,
) -> Option<Vec<ValType>> {
    let mut stack = start_stack;
    for i in instr {
        match i {
            Instr::Const(Const::I32(x)) => stack.push(ValType::I32),
            Instr::Const(Const::I64(x)) => stack.push(ValType::I64),
            Instr::Const(Const::F32(x)) => stack.push(ValType::F32),
            Instr::Const(Const::F64(x)) => stack.push(ValType::F64),
            _ => unimplemented!()
        }
    }
    Some(stack)
}

fn check_block_context(label: &Option<ValType>, context: &Context) -> Option<Context> {
    Some(Context {
        types: context.types.clone(),
        funcs: context.funcs.clone(),
        tables: context.tables.clone(),
        mems: context.mems.clone(),
        globals: context.globals.clone(),
        labels: context.labels.clone().add(im_rc::vector![label.clone()]),
        locals: context.locals.clone(),
        ret: context.ret.clone(),
    })
}

mod expressions {
    use super::*;
}
