#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FuncRef {}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValType { I32, F32, I64, F64 }

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sx { U, S }

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncType {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TableType {
    pub limits: Limits,
    pub elemtype: FuncRef,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct MemType {
    pub limits: Limits,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Limits {
    pub min: u64,
    pub max: Option<u64>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mut { Var, Const }

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct GlobalType {
    pub mut_: Mut,
    pub valtype: ValType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExternFuncType(pub FuncType);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternMemType(pub MemType);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternTableType(pub TableType);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternGlobalType(pub GlobalType);

#[derive(Clone, Debug, PartialEq)]
pub enum ExternType {
    Func(ExternFuncType),
    Mem(ExternMemType),
    Table(ExternTableType),
    Global(ExternGlobalType),
}

pub type TypeIdx = u32;

pub type FuncIdx = u32;

pub type TableIdx = u32;

pub type MemIdx = u32;

pub type GlobalIdx = u32;

pub type Name = String;