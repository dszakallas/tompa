use super::*;
use crate::syntax::types::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_limits_bound_rule() {
        LimitsBoundRule { bound: 1 << 16 }
            .check(&Limits { min: 1, max: None }, &Context::empty())
            .unwrap();

        LimitsBoundRule { bound: 1 << 16 }
            .check(&Limits { min: 1 << 32, max: None }, &Context::empty())
            .unwrap_err();

        LimitsBoundRule { bound: 1 << 32 }
            .check(&Limits { min: 1 << 32, max: Some(1 << 16) }, &Context::empty())
            .unwrap_err();

        LimitsBoundRule { bound: 1 << 16 }
            .check(&Limits { min: 1 << 16, max: Some(1 << 32) }, &Context::empty())
            .unwrap_err();
    }

    #[test]
    fn test_func_type_rule() {
        FuncTypeRule {}
            .check(&FuncType { parameters: vec![], results: vec![] }, &Context::empty())
            .unwrap();

        FuncTypeRule {}
            .check(&FuncType {
                parameters: vec![ValType::I32, ValType::F32],
                results: vec![ValType::F64],
            }, &Context::empty())
            .unwrap();

        FuncTypeRule {}
            .check(&FuncType {
                parameters: vec![ValType::I32, ValType::F32],
                results: vec![ValType::F64, ValType::F64],
            }, &Context::empty())
            .unwrap_err();
    }


    #[test]
    fn test_table_type_rule() {
        TableTypeRule {}
            .check(&TableType {
                limits: Limits { min: 1 << 32, max: Some(1 << 32) },
                elemtype: FuncRef {},
            }, &Context::empty())
            .unwrap();

        TableTypeRule {}
            .check(&TableType {
                limits: Limits { min: 1 << 33, max: Some(1 << 32) },
                elemtype: FuncRef {},
            }, &Context::empty())
            .unwrap_err();
    }

    #[test]
    fn test_memory_type_rule() {
        MemTypeRule {}
            .check(&MemType {
                limits: Limits { min: 1 << 16, max: Some(1 << 16) },
            }, &Context::empty())
            .unwrap();
        MemTypeRule {}
            .check(&MemType {
                limits: Limits { min: 1 << 17, max: Some(1 << 17) },
            }, &Context::empty())
            .unwrap_err();
    }

    #[test]
    fn test_global_type_rule() {
        GlobalTypeRule {}
            .check(&GlobalType {
                mut_: Mut::Const,
                valtype: ValType::I32,
            }, &Context::empty())
            .unwrap();

        GlobalTypeRule {}
            .check(&GlobalType {
                mut_: Mut::Const,
                valtype: ValType::F64,
            }, &Context::empty())
            .unwrap();
    }
}

fn limits_bound_rule(syntax: &Limits, rule: LimitsBoundRule, _context: &Context) -> Result<(), TypeError> {
    if syntax.min <= rule.bound && syntax.max.map_or(true, |max| syntax.min <= max && max <= rule.bound) {
        Ok(())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(LimitsBoundRule { bound: u64 }: Limits => (), limits_bound_rule);

fn func_type_rule(syntax: &FuncType, rule: FuncTypeRule, context: &Context) -> Result<(), TypeError> {
    if syntax.results.len() <= 1 {
        Ok(())
    } else {
        Err(type_error!(syntax, rule))
    }
}

rule!(FuncTypeRule: FuncType => (), func_type_rule);

fn table_type_rule(syntax: &TableType, rule: TableTypeRule, context: &Context) -> Result<(), TypeError> {
    LimitsBoundRule { bound: 1 << 32 }.check(&syntax.limits, context)?;
    Ok(())
}

rule!(TableTypeRule: TableType => (), table_type_rule);

fn mem_type_rule(syntax: &MemType, rule: MemTypeRule, context: &Context) -> Result<(), TypeError> {
    LimitsBoundRule { bound: 1 << 16 }.check(&syntax.limits, context)?;
    Ok(())
}

rule!(MemTypeRule: MemType => (), mem_type_rule);

rule!(GlobalTypeRule: GlobalType => (), |_, _ ,_| Ok(()));

macro_rules! derive_extern_type_rule {
    ($rulename:ident($inner:ident): $syntax:ident => ()) => {
        rule!($rulename {}: $syntax => (), |syntax: &$syntax, _, context: &Context| $inner {}.check(&syntax.0, context));
    };
}

derive_extern_type_rule!(ExternFuncTypeRule(FuncTypeRule): ExternFuncType => ());
derive_extern_type_rule!(ExternTableTypeRule(TableTypeRule): ExternTableType => ());
derive_extern_type_rule!(ExternGlobalTypeRule(GlobalTypeRule): ExternGlobalType => ());
derive_extern_type_rule!(ExternMemTypeRule(MemTypeRule): ExternMemType => ());

