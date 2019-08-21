use super::*;

use crate::syntax::types::*;
use crate::syntax::instructions::*;
use std::collections::HashSet;

use std::ops::Add;
use std::iter::FromIterator;

mod numeric {
    use super::*;

    #[cfg(test)]
    mod tests {
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
mod tests {
    use super::*;

    #[test]
    fn test_block_rule() {
        let t1 = BlockRule {}.check(
            &Block { result: None, instr: vec![] },
            &Context::empty(),
        ).unwrap();

        assert_eq!(t1, FuncType { parameters: vec![], results: vec![] });

        let t2 = BlockRule {}.check(
            &Block { result: Some(ValType::I64), instr: vec![Instruction::Const(Const::I64(256))] },
            &Context::empty(),
        ).unwrap();

        assert_eq!(t2, FuncType { parameters: vec![], results: vec![ValType::I64] });
    }
}

fn expr_rule(syntax: &Expr, rule: ExprRule, context: &Context) -> Result<Option<ValType>, TypeError> {
    check_expr(&syntax.instr, rule.is_const, context, || type_error!(syntax, rule))
}

rule!(ExprRule { is_const: bool }: Expr => Option<ValType>, expr_rule);


fn block_rule(syntax: &Block, rule: BlockRule, context: &Context) -> Result<FuncType, TypeError> {
    let block_context = check_block_context(&syntax.result, context).ok_or_else(|| type_error!(syntax, rule))?;
    let result = check_expr(&syntax.instr, false, context, || type_error!(syntax, rule))?;

    if result == syntax.result {
        Ok(FuncType { parameters: vec![], results: Vec::from_iter(result.into_iter()) })
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(BlockRule: Block => FuncType, block_rule);

fn instruction_seq_rule(syntax: &Vec<Instruction>, rule: InstructionSeqRule, context: &Context) -> Result<FuncType, TypeError> {
    let end_stack = check_instruction_seq(syntax, rule.start_stack.clone(), rule.is_const).ok_or_else(|| type_error!(syntax, rule))?;
    Ok(FuncType { parameters: rule.start_stack, results: end_stack })
}

rule!(InstructionSeqRule { start_stack: Vec<ValType>, is_const: bool }: Vec<Instruction> => FuncType, instruction_seq_rule);

fn check_expr<F: Fn() -> TypeError>(instr: &Vec<Instruction>, is_const: bool, context: &Context, err: F) -> Result<Option<ValType>, TypeError> {
    let seq = InstructionSeqRule { start_stack: vec![], is_const }.check(instr, &context)?;

    if seq.results.len() <= 1 {
        Ok(seq.results.get(0).cloned())
    } else {
        Err(err())
    }
}

fn check_instruction_seq(instr: &Vec<Instruction>, start_stack: Vec<ValType>, is_const: bool) -> Option<Vec<ValType>> {
    let mut stack = start_stack;
    for i in instr {
        match i {
            Instruction::Const(Const::I32(x)) => stack.push(ValType::I32),
            Instruction::Const(Const::I64(x)) => stack.push(ValType::I64),
            Instruction::Const(Const::F32(x)) => stack.push(ValType::F32),
            Instruction::Const(Const::F64(x)) => stack.push(ValType::F64),
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

