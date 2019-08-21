use super::*;
use crate::typing::types::*;
use super::instructions::*;
use crate::syntax::types::*;
use crate::syntax::instructions::*;
use crate::syntax::modules::*;
use im_rc;

use std::ops::Add;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_rule() {
        let t1 = FunctionRule {}.check(
            &Function { type_idx: 0, locals: vec![], body: Expr { instr: vec![Instruction::Const(Const::I64(256))] } },
            &Context {
                types: im_rc::vector![FuncType { parameters: vec![], results: vec![ValType::I64] }],
                funcs: im_rc::vector![],
                tables: im_rc::vector![],
                mems: im_rc::vector![],
                globals: im_rc::vector![],
                labels: im_rc::vector![],
                locals: im_rc::vector![],
                ret: None,
            },
        ).unwrap();

        assert_eq!(t1, FuncType { parameters: vec![], results: vec![ValType::I64] });

        FunctionRule {}.check(
            &Function { type_idx: 0, locals: vec![], body: Expr { instr: vec![] } },
            &Context {
                types: im_rc::vector![FuncType { parameters: vec![], results: vec![ValType::I64] }],
                funcs: im_rc::vector![],
                tables: im_rc::vector![],
                mems: im_rc::vector![],
                globals: im_rc::vector![],
                labels: im_rc::vector![],
                locals: im_rc::vector![],
                ret: None,
            },
        ).unwrap_err();
    }

    #[test]
    fn test_table_rule() {
        TableRule {}.check(
            &Table { tpe: TableType {
                limits: Limits { min: 1 << 8, max: None },
                elemtype: FuncRef {}
            } },
            &Context::empty()
        ).unwrap();
    }

    #[test]
    fn test_mem_rule() {
        MemRule {}.check(
            &Mem { tpe: MemType {
                limits: Limits { min: 1 << 8, max: None }
            } },
            &Context::empty()
        ).unwrap();
    }
}

fn check_function_context(tpe_idx: &TypeIdx, locals: &Vec<ValType>, context: &Context) -> Option<(FuncType, Context)> {
    let tpe = context.types.get(*tpe_idx as usize)?;

    let fn_labels = context.labels.clone().add(im_rc::vector![tpe.results.get(0).cloned()]);
    let fn_ret = tpe.results.get(0).cloned();

    let fn_locals = context.locals.clone()
        .add(im_rc::Vector::from(&tpe.parameters))
        .add(im_rc::Vector::from(locals));

    Some((tpe.clone(), Context {
        types: context.types.clone(),
        funcs: context.funcs.clone(),
        tables: context.tables.clone(),
        mems: context.mems.clone(),
        globals: context.globals.clone(),
        labels: fn_labels,
        locals: fn_locals,
        ret: fn_ret }))
}

fn function_rule(syntax: &Function, rule: FunctionRule, context: &Context) -> Result<FuncType, TypeError> {
    let (tpe, fn_context) = check_function_context(&syntax.type_idx, &syntax.locals, context).ok_or_else(|| type_error!(syntax, rule))?;
    let result = ExprRule { is_const: false }.check(&syntax.body, &fn_context)?;

    if tpe.results.get(0) == result.as_ref() {
        Ok(tpe)
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(FunctionRule: Function => FuncType, function_rule);

fn table_rule(syntax: &Table, rule: TableRule, context: &Context) -> Result<TableType, TypeError> {
    TableTypeRule {}.check(&syntax.tpe, &context)?;
    Ok(syntax.tpe.clone())
}

rule!(TableRule: Table => TableType, table_rule);

fn mem_rule(syntax: &Mem, rule: MemRule, context: &Context) -> Result<MemType, TypeError> {
    MemTypeRule {}.check(&syntax.tpe, &context)?;
    Ok(syntax.tpe.clone())
}

rule!(MemRule: Mem => MemType, mem_rule);

fn global_rule(syntax: &Global, rule: GlobalRule, context: &Context) -> Result<GlobalType, TypeError> {
    GlobalTypeRule {}.check(&syntax.tpe, &context)?;
    let result = ExprRule { is_const: true }.check(&syntax.expr, &context)?;

    if result == Some(syntax.tpe.valtype) {
        Ok(syntax.tpe.clone())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(GlobalRule: Global => GlobalType, global_rule);

macro_rules! derive_extern_rule {
    ($rulename:ident: $syntax:ty => $tpe:ident, $prop:ident) => {
        rule!($rulename: $syntax => $tpe, |syntax: &$syntax, rule: $rulename, context: &Context| {
            let tpe = context.$prop.get(syntax.0 as usize).ok_or_else(|| type_error!(syntax, rule))?;
            Ok($tpe(tpe.clone()))
        });
    };
}

derive_extern_rule!(ExternFuncRule: ExternFunc => ExternFuncType, funcs);
derive_extern_rule!(ExternTableRule: ExternTable => ExternTableType, tables);
derive_extern_rule!(ExternMemRule: ExternMem => ExternMemType, mems);
derive_extern_rule!(ExternGlobalRule: ExternGlobal => ExternGlobalType, globals);

fn export_desc_rule(syntax: &ExportDesc, rule: ExportDescRule, context: &Context) -> Result<ExternType, TypeError> {
    match syntax {
        ExportDesc::Func(e) => Ok(ExternType::Func(ExternFuncRule {}.check(e, context)?)),
        ExportDesc::Table(e) => Ok(ExternType::Table(ExternTableRule {}.check(e, context)?)),
        ExportDesc::Mem(e) => Ok(ExternType::Mem(ExternMemRule {}.check(e, context)?)),
        ExportDesc::Global(e) => Ok(ExternType::Global(ExternGlobalRule {}.check(e, context)?))
    }
}

rule!(ExportDescRule: ExportDesc => ExternType, export_desc_rule);
