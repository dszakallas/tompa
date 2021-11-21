use super::*;

#[cfg(test)]
mod test {
    use super::*;

    use crate::ast::ValType::*;

    #[test]
    fn test_limits_rule() {
        Limits { min: 1, max: None }
            .check(&LimitsBound { bound: 1 << 16}, &())
            .unwrap();

        Limits { min: 1 << 16, max: None }
            .check(&LimitsBound { bound: 1 << 8 }, &())
            .unwrap_err();

        Limits { min: 1 << 16, max: Some(1 << 8) }
            .check(&LimitsBound { bound: 1 << 16 }, &())
            .unwrap_err();

        Limits { min: 1 << 8, max: Some(1 << 16) }
            .check(&LimitsBound { bound: 1 << 8 }, &())
            .unwrap_err();
    }

    #[test]
    fn test_func_type_rule() {
        FuncType { parameters: vec![], results: vec![] }
            .check(&(), &()).unwrap();

        FuncType { parameters: vec![I32, F32], results: vec![F64] }
            .check(&(), &()).unwrap();

        FuncType { parameters: vec![I32, F32], results: vec![F64, F64] }
            .check(&(), &()).unwrap_err();
    }

    #[test]
    fn test_table_type_rule() {
        &TableType {
            limits: Limits { min: 1 << 16, max: Some(1 << 16) },
            elemtype: FuncRef {},
        }.check(&(), &()).unwrap();

        &TableType {
            limits: Limits { min: 1 << 17, max: Some(1 << 16) },
            elemtype: FuncRef {},
        }.check(&(), &()).unwrap_err();

    }

    #[test]
    fn test_memory_type_rule() {
        &MemType { limits: Limits { min: 1 << 16, max: Some(1 << 16) } }
            .check(&(), &()).unwrap();

        &MemType { limits: Limits { min: 1 << 17, max: Some(1 << 17) } }
            .check(&(), &()).unwrap_err();
    }

    #[test]
    fn test_global_type_rule() {
        &GlobalType { mut_: Mut::Const, valtype: ValType::I32 }
            .check(&(), &()).unwrap();
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct LimitsBound { pub bound: u32 }

#[inline]
fn limits_rule(syntax: &Limits, params: &LimitsBound, _ctx: &()) -> WrappedResult<()> {
    if syntax.min <= params.bound && syntax.max.map_or(true, |max| syntax.min <= max && max <= params.bound) {
        Ok(())
    } else {
        Err(TypeError)
    }
}

def_rule!(Limits(LimitsBound) => (), limits_rule);

#[inline]
fn func_type_rule(syntax: &FuncType, _params: &(), _ctx: &()) -> WrappedResult<()> {
    if syntax.results.len() <= 1 { Ok(()) } else { Err(TypeError) }
}

def_rule!(FuncType => (), func_type_rule);

#[inline]
fn table_type_rule(syntax: &TableType, _params: &(), ctx: &()) -> WrappedResult<()> {
    syntax.limits.check(&LimitsBound { bound: 1 << 16 }, ctx)
}

def_rule!(TableType => (), table_type_rule);

#[inline]
fn mem_type_rule(syntax: &MemType, _params: &(), ctx: &()) -> WrappedResult<()> {
    syntax.limits.check(&LimitsBound { bound: 1 << 16 }, ctx)
}

def_rule!(MemType => (), mem_type_rule);

def_rule!(GlobalType => (), |_, _ ,_| Ok(()));

macro_rules! def_extern_type_rule {
    ($syntax:ident) => {
        def_rule!($syntax => (), |syntax: &$syntax, _, context: &()| {
            syntax.0.check(&(), context)
        });
    };
}

def_extern_type_rule!(ExternFuncType);
def_extern_type_rule!(ExternTableType);
def_extern_type_rule!(ExternGlobalType);
def_extern_type_rule!(ExternMemType);
