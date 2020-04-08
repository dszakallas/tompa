use std::collections::HashSet;
use std::ops::{Add};

use im_rc;

use crate::ast::*;
use crate::validation::types::*;

use super::*;

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast;

    #[test]
    fn test_function_rule() {
        let t1 = FunctionRule {}
            .check(
                &Function {
                    type_idx: 0,
                    locals: vec![],
                    body: vec![Instr::Const(Const::I64(256))],
                },
                &Context {
                    types: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![ValType::I64]
                    }],
                    funcs: im_rc::vector![],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap();

        assert_eq!(
            t1,
            FuncType {
                parameters: vec![],
                results: vec![ValType::I64]
            }
        );

        FunctionRule {}
            .check(
                &Function {
                    type_idx: 0,
                    locals: vec![],
                    body: vec![],
                },
                &Context {
                    types: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![ValType::I64]
                    }],
                    funcs: im_rc::vector![],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap_err();
    }

    #[test]
    fn test_table_rule() {
        TableRule {}
            .check(
                &Table {
                    tpe: TableType {
                        limits: Limits {
                            min: 1 << 8,
                            max: None,
                        },
                        elemtype: FuncRef {},
                    },
                },
                &Default::default(),
            )
            .unwrap();
    }

    #[test]
    fn test_mem_rule() {
        MemRule {}
            .check(
                &Mem {
                    tpe: MemType {
                        limits: Limits {
                            min: 1 << 8,
                            max: None,
                        },
                    },
                },
                &Default::default(),
            )
            .unwrap();
    }

    #[test]
    fn test_global_rule() {
        GlobalRule {}
            .check(
                &Global {
                    tpe: GlobalType { mut_: Mut::Var, valtype: ValType::I32 },
                    expr: vec![Instr::Const(Const::I64(256))],
                },
                &Default::default(),
            )
            .unwrap_err();

        GlobalRule {}
            .check(
                &Global {
                    tpe: GlobalType { mut_: Mut::Var, valtype: ValType::I32 },
                    expr: vec![Instr::Const(Const::I32(256))],
                },
                &Default::default(),
            )
            .unwrap();
    }

    #[test]
    fn test_element_segment_rule() {
        ElementSegmentRule {}
            .check(
                &ElementSegment {
                    table: 0,
                    offset: vec![Instr::Const(Const::I32(256))],
                    init: vec![0],
                },
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![ValType::I32]
                    }],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap_err();
        ElementSegmentRule {}
            .check(
                &ElementSegment {
                    table: 0,
                    offset: vec![Instr::Const(Const::I32(256))],
                    init: vec![0],
                },
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![],
                    tables: im_rc::vector![TableType {
                        limits: Limits { min: 0, max: None },
                        elemtype: FuncRef {}
                    }],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap_err();

        ElementSegmentRule {}
            .check(
                &ElementSegment {
                    table: 0,
                    offset: vec![Instr::Const(Const::I32(256))],
                    init: vec![0],
                },
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![ValType::I32]
                    }],
                    tables: im_rc::vector![TableType {
                        limits: Limits { min: 0, max: None },
                        elemtype: FuncRef {}
                    }],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap();
    }

    #[test]
    fn test_data_segment_rule() {
        DataSegmentRule {}
            .check(
                &DataSegment {
                    data: 0,
                    offset: vec![Instr::Const(Const::I32(256))],
                    init: vec![0],
                },
                &Default::default(),
            )
            .unwrap_err();

        DataSegmentRule {}
            .check(
                &DataSegment {
                    data: 0,
                    offset: vec![Instr::Const(Const::I32(256))],
                    init: vec![0],
                },
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![MemType {
                        limits: Limits { min: 0, max: None }
                    }],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap();
    }

    #[test]
    fn test_start_rule() {
        StartRule {}
            .check(
                &Start(0),
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![ValType::I32]
                    }],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap_err();

        StartRule {}
            .check(
                &Start(0),
                &Context {
                    types: im_rc::vector![],
                    funcs: im_rc::vector![FuncType {
                        parameters: vec![],
                        results: vec![]
                    }],
                    tables: im_rc::vector![],
                    mems: im_rc::vector![],
                    globals: im_rc::vector![],
                    labels: im_rc::vector![],
                    locals: im_rc::vector![],
                    ret: None,
                },
            )
            .unwrap();
    }

    #[test]
    fn test_export_rule() {
        let ctx = Context {
            types: Default::default(),
            funcs: im_rc::vector![FuncType {
                parameters: vec![],
                results: vec![]
            }],
            tables: Default::default(),
            mems: Default::default(),
            globals: Default::default(),
            labels: Default::default(),
            locals: Default::default(),
            ret: None,
        };

        ExportRule {}
            .check(
                &Export {
                    name: "export_1".to_owned(),
                    expr: ExportDesc::Func(ExternFunc(0)),
                },
                &ctx,
            )
            .unwrap();

        ExportRule {}
            .check(
                &Export {
                    name: "export_2".to_owned(),
                    expr: ExportDesc::Global(ExternGlobal(0)),
                },
                &ctx,
            )
            .unwrap_err();
    }
}

fn check_function_context(
    tpe_idx: &TypeIdx,
    locals: &Vec<ValType>,
    context: &Context,
) -> Option<(FuncType, Context)> {
    let tpe = context.types.get(*tpe_idx as usize)?;

    let fn_labels = context
        .labels
        .clone()
        .add(im_rc::vector![tpe.results.get(0).cloned()]);
    let fn_ret = tpe.results.get(0).cloned();

    let fn_locals = context
        .locals
        .clone()
        .add(im_rc::Vector::from(&tpe.parameters))
        .add(im_rc::Vector::from(locals));

    Some((
        tpe.clone(),
        Context {
            types: context.types.clone(),
            funcs: context.funcs.clone(),
            tables: context.tables.clone(),
            mems: context.mems.clone(),
            globals: context.globals.clone(),
            labels: fn_labels,
            locals: fn_locals,
            ret: Some(fn_ret),
        },
    ))
}

rule!(FunctionRule: Function => FuncType, function_rule);

use crate::validation::instructions::check_instruction_seq;

fn function_rule(
    syntax: &Function,
    _rule: &FunctionRule,
    context: &Context,
) -> WrappedResult<FuncType> {
    let (tpe, fn_context) = check_function_context(&syntax.type_idx, &syntax.locals, context)?;
    check_instruction_seq(tpe.parameters.clone(), tpe.results.clone(), &syntax.body, &fn_context, true)?;
    Ok(tpe)
}

rule!(TableRule: Table => TableType, table_rule);

fn table_rule(syntax: &Table, _rule: &TableRule, context: &Context) -> WrappedResult<TableType> {
    TableTypeRule {}.check(&syntax.tpe, &context)?;
    Ok(syntax.tpe.clone())
}

rule!(MemRule: Mem => MemType, mem_rule);

fn mem_rule(syntax: &Mem, _rule: &MemRule, context: &Context) -> WrappedResult<MemType> {
    MemTypeRule {}.check(&syntax.tpe, &context)?;
    Ok(syntax.tpe.clone())
}

rule!(GlobalRule: Global => GlobalType, global_rule);

fn global_rule(syntax: &Global, _rule: &GlobalRule, context: &Context) -> WrappedResult<GlobalType> {
    GlobalTypeRule {}.check(&syntax.tpe, &context)?;
    check_instruction_seq(vec![], vec![syntax.tpe.valtype], &syntax.expr, context, true)?;
    Ok(syntax.tpe)
}

rule!(ElementSegmentRule: ElementSegment => (), element_segment_rule);

fn element_segment_rule(
    syntax: &ElementSegment,
    _rule: &ElementSegmentRule,
    context: &Context,
) -> WrappedResult<()> {
    let k = context.tables.get(syntax.table as usize)?;

    TableTypeRule {}.check(k, context)?;

    for f in &syntax.init {
        context.funcs.get(*f as usize)?;
    }

    check_instruction_seq(vec![], vec![ValType::I32], &syntax.offset, context, true)?;
    Ok(())
}

rule!(DataSegmentRule: DataSegment => (), data_segment_rule);

fn data_segment_rule(
    syntax: &DataSegment,
    _rule: &DataSegmentRule,
    context: &Context,
) -> WrappedResult<()> {
    let k = context.mems.get(syntax.data as usize)?;

    MemTypeRule {}.check(k, context)?;

    check_instruction_seq(vec![], vec![ValType::I32], &syntax.offset, context, true)?;
    Ok(())
}

rule!(StartRule: Start => (), start_rule);

fn start_rule(syntax: &Start, _rule: &StartRule, context: &Context) -> WrappedResult<()> {
    let k = context.funcs.get(syntax.0 as usize)?;

    if *k
        == (FuncType {
            parameters: vec![],
            results: vec![],
        })
    {
        Ok(())
    } else {
        None?
    }
}

macro_rules! derive_export_rule {
    ($rulename:ident: $syntax:ty => $tpe:ident, $prop:ident) => {
        rule!($rulename: $syntax => $tpe, |syntax: &$syntax, _rule: &$rulename, context: &Context| {
            let tpe = context.$prop.get(syntax.0 as usize)?;
            Ok($tpe(tpe.clone()))
        });
    };
}

derive_export_rule!(ExportFuncRule: ExternFunc => ExternFuncType, funcs);
derive_export_rule!(ExportTableRule: ExternTable => ExternTableType, tables);
derive_export_rule!(ExportMemRule: ExternMem => ExternMemType, mems);
derive_export_rule!(ExportGlobalRule: ExternGlobal => ExternGlobalType, globals);

rule!(ExportRule: Export => ExternType, export_rule);

fn export_rule(syntax: &Export, _rule: &ExportRule, context: &Context) -> WrappedResult<ExternType> {
    match &syntax.expr {
        ExportDesc::Func(e) => Ok(ExternType::Func(ExportFuncRule {}.check(e, context)?)),
        ExportDesc::Table(e) => Ok(ExternType::Table(ExportTableRule {}.check(e, context)?)),
        ExportDesc::Mem(e) => Ok(ExternType::Mem(ExportMemRule {}.check(e, context)?)),
        ExportDesc::Global(e) => Ok(ExternType::Global(ExportGlobalRule {}.check(e, context)?)),
    }
}

rule!(ImportFuncRule: ExternFunc => ExternFuncType, import_func_rule);

fn import_func_rule(
    syntax: &ExternFunc,
    _rule: &ImportFuncRule,
    context: &Context,
) -> WrappedResult<ExternFuncType> {
    let tpe = context.types.get(syntax.0 as usize)?;
    Ok(ExternFuncType(tpe.clone()))
}

macro_rules! derive_import_rule {
    ($rulename:ident: $syntax:ty => $tpe:ident, $inner:tt) => {
        rule!($rulename: $syntax => $tpe, |syntax: &$syntax, _rule: &$rulename, context: &Context| {
            $inner {}.check(syntax, context)?;
            Ok(syntax.clone())
        });
    };
}

derive_import_rule!(ImportTableRule: ExternTableType => ExternTableType, ExternTableTypeRule);
derive_import_rule!(ImportMemRule: ExternMemType => ExternMemType, ExternMemTypeRule);
derive_import_rule!(ImportGlobalRule: ExternGlobalType => ExternGlobalType, ExternGlobalTypeRule);

rule!(ImportRule: Import => ExternType, import_rule);

fn import_rule(syntax: &Import, _rule: &ImportRule, context: &Context) -> WrappedResult<ExternType> {
    match &syntax.expr {
        ImportDesc::Func(e) => Ok(ExternType::Func(ImportFuncRule {}.check(e, context)?)),
        ImportDesc::Table(e) => Ok(ExternType::Table(ImportTableRule {}.check(e, context)?)),
        ImportDesc::Mem(e) => Ok(ExternType::Mem(ImportMemRule {}.check(e, context)?)),
        ImportDesc::Global(e) => Ok(ExternType::Global(ImportGlobalRule {}.check(e, context)?)),
    }
}

rule!(ModuleRule: Module => (Vec<ExternType>, Vec<ExternType>), module_rule);

fn module_rule(
    syntax: &Module,
    _rule: &ModuleRule,
    _context: &Context,
) -> WrappedResult<(Vec<ExternType>, Vec<ExternType>)> {
    let mut ctx: Context = Default::default();

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
            ExternType::Global(ExternGlobalType(gt)) => {
                ctx.globals.push_back(*gt);
                global_ctx.globals.push_back(*gt)
            }
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
        ctx.globals
            .push_back(GlobalRule {}.check(global, &global_ctx)?);
    }

    let mut ets = vec![];
    let mut et_names = HashSet::new();
    for export in &syntax.exports {
        if !et_names.contains(&export.name) {
            et_names.insert(&export.name);
        } else {
            None?
        }
        ets.push(ExportRule {}.check(export, &ctx)?);
    }

    &syntax
        .start
        .map_or(Ok(()), |start| StartRule {}.check(&start, &ctx))?;

    for elem in &syntax.elem {
        ElementSegmentRule {}.check(elem, &ctx)?;
    }

    for data in &syntax.data {
        DataSegmentRule {}.check(data, &ctx)?;
    }

    Ok((its, ets))
}
