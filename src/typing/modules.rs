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
            &Table {
                tpe: TableType {
                    limits: Limits { min: 1 << 8, max: None },
                    elemtype: FuncRef {}
                }
            },
            &Context::empty()
        ).unwrap();
    }

    #[test]
    fn test_mem_rule() {
        MemRule {}.check(
            &Mem {
                tpe: MemType { limits: Limits { min: 1 << 8, max: None } }
            },
            &Context::empty()
        ).unwrap();
    }

    #[test]
    fn test_global_rule() {
        GlobalRule {}.check(
            &Global {
                tpe: GlobalType { mut_: Mut::Var, valtype: ValType::I32  },
                expr: Expr { instr: vec![Instruction::Const(Const::I64(256))] }
            },
            &Context::empty()
        ).unwrap_err();

        GlobalRule {}.check(
            &Global {
                tpe: GlobalType { mut_: Mut::Var, valtype: ValType::I32  },
                expr: Expr { instr: vec![Instruction::Const(Const::I32(256))] }
            },
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
        ret: fn_ret
    }))
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

fn element_segment_rule(syntax: &ElementSegment, rule: ElementSegmentRule, context: &Context) -> Result<(), TypeError> {
    let k = context.tables.get(syntax.table as usize).ok_or_else(|| type_error!(syntax, rule))?;

    TableTypeRule {}.check(k, context)?;

    for f in &syntax.init {
        context.funcs.get(*f as usize).ok_or_else(|| type_error!(syntax, rule))?;
    }

    let res = ExprRule { is_const: true }.check(&syntax.offset, context)?;

    if res == Some(ValType::I32) {
        Ok(())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(ElementSegmentRule: ElementSegment => (), element_segment_rule);

fn data_segment_rule(syntax: &DataSegment, rule: DataSegmentRule, context: &Context) -> Result<(), TypeError> {
    let k = context.mems.get(syntax.data as usize).ok_or_else(|| type_error!(syntax, rule))?;

    MemTypeRule {}.check(k, context)?;

    let res = ExprRule { is_const: true }.check(&syntax.offset, context)?;

    if res == Some(ValType::I32) {
        Ok(())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(DataSegmentRule: DataSegment => (), data_segment_rule);

fn start_rule(syntax: &Start, rule: StartRule, context: &Context) -> Result<(), TypeError> {
    let k = context.funcs.get(syntax.0 as usize).ok_or_else(|| type_error!(syntax, rule))?;

    if *k == (FuncType { parameters: vec![], results: vec![] }) {
        Ok(())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(StartRule: Start => (), start_rule);

macro_rules! derive_export_rule {
    ($rulename:ident: $syntax:ty => $tpe:ident, $prop:ident) => {
        rule!($rulename: $syntax => $tpe, |syntax: &$syntax, rule: $rulename, context: &Context| {
            let tpe = context.$prop.get(syntax.0 as usize).ok_or_else(|| type_error!(syntax, rule))?;
            Ok($tpe(tpe.clone()))
        });
    };
}

derive_export_rule!(ExportFuncRule: ExternFunc => ExternFuncType, funcs);
derive_export_rule!(ExportTableRule: ExternTable => ExternTableType, tables);
derive_export_rule!(ExportMemRule: ExternMem => ExternMemType, mems);
derive_export_rule!(ExportGlobalRule: ExternGlobal => ExternGlobalType, globals);

fn export_rule(syntax: &Export, rule: ExportRule, context: &Context) -> Result<ExternType, TypeError> {
    match &syntax.expr {
        ExportDesc::Func(e) => Ok(ExternType::Func(ExportFuncRule {}.check(e, context)?)),
        ExportDesc::Table(e) => Ok(ExternType::Table(ExportTableRule {}.check(e, context)?)),
        ExportDesc::Mem(e) => Ok(ExternType::Mem(ExportMemRule {}.check(e, context)?)),
        ExportDesc::Global(e) => Ok(ExternType::Global(ExportGlobalRule {}.check(e, context)?))
    }
}

rule!(ExportRule: Export => ExternType, export_rule);

fn import_func_rule(syntax: &ExternFunc, rule: ImportFuncRule, context: &Context) -> Result<ExternFuncType, TypeError> {
    let tpe = context.types.get(syntax.0 as usize).ok_or_else(|| type_error!(syntax, rule))?;
    Ok(ExternFuncType(tpe.clone()))
}

rule!(ImportFuncRule: ExternFunc => ExternFuncType, import_func_rule);

macro_rules! derive_import_rule {
    ($rulename:ident: $syntax:ty => $tpe:ident, $inner:tt) => {
        rule!($rulename: $syntax => $tpe, |syntax: &$syntax, rule: $rulename, context: &Context| {
            $inner {}.check(syntax, context)?;
            Ok(syntax.clone())
        });
    };
}

derive_import_rule!(ImportTableRule: ExternTableType => ExternTableType, ExternTableTypeRule);
derive_import_rule!(ImportMemRule: ExternMemType => ExternMemType, ExternMemTypeRule);
derive_import_rule!(ImportGlobalRule: ExternGlobalType => ExternGlobalType, ExternGlobalTypeRule);

fn import_rule(syntax: &Import, rule: ImportRule, context: &Context) -> Result<ExternType, TypeError> {
    match &syntax.expr {
        ImportDesc::Func(e) => Ok(ExternType::Func(ImportFuncRule {}.check(e, context)?)),
        ImportDesc::Table(e) => Ok(ExternType::Table(ImportTableRule {}.check(e, context)?)),
        ImportDesc::Mem(e) => Ok(ExternType::Mem(ImportMemRule {}.check(e, context)?)),
        ImportDesc::Global(e) => Ok(ExternType::Global(ImportGlobalRule {}.check(e, context)?))
    }
}

rule!(ImportRule: Import => ExternType, import_rule);

fn module_rule(syntax: &Module, rule: ModuleRule, _context: &Context) -> Result<(Vec<ExternType>, Vec<ExternType>),TypeError> {
    let mut ctx = Context::empty();

    let mut global_ctx = ctx.clone();

    for tpe in &syntax.types {
        FuncTypeRule {}.check(tpe, &ctx)?;
    }
    ctx.types = im_rc::Vector::from(&syntax.types);

    let mut its = vec![];
    for import in &syntax.imports {
        let it = ImportRule {}.check(import, &ctx)?;

        match &it {
            ExternType::Func(ExternFuncType(ft)) => ctx.types.push_back(ft.clone()),
            ExternType::Mem(ExternMemType(mt)) => ctx.mems.push_back(*mt),
            ExternType::Table(ExternTableType(tt)) => ctx.tables.push_back(*tt),
            ExternType::Global(ExternGlobalType(gt)) => { ctx.globals.push_back(*gt); global_ctx.globals.push_back(*gt) }
        }
        its.push(it);
    }

    for table in &syntax.tables {
        ctx.tables.push_back(TableRule {}.check(table, &ctx)?);
    }

    for mem in &syntax.mems {
        ctx.mems.push_back(MemRule {}.check(mem, &ctx)?);
    }

    for global in &syntax.globals {
        ctx.globals.push_back(GlobalRule {}.check(global, &global_ctx)?);
    }

    let mut ets = vec![];
    for export in &syntax.exports {
        ets.push(ExportRule {}.check(export, &ctx)?);
    }

    // start, elem, data

    Err(type_error!(syntax, rule))
}

rule!(ModuleRule: Module => (Vec<ExternType>, Vec<ExternType>), module_rule);