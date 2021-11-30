#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FuncRef {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ValType {
    I32,
    F32,
    I64,
    F64,
}

impl ValType {
    pub fn size(&self) -> u32 {
        match &self {
            ValType::I32 => 4,
            ValType::F32 => 4,
            ValType::I64 => 8,
            ValType::F64 => 8,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ResultType {
    pub valtype: Option<ValType>
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Sx {
    U,
    S,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Memarg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct FuncType {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TableType {
    pub limits: Limits,
    pub elemtype: FuncRef,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct MemType {
    pub limits: Limits,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mut {
    Var,
    Const,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GlobalType {
    pub mut_: Mut,
    pub valtype: ValType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternFuncType(pub FuncType);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ExternMemType(pub MemType);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ExternTableType(pub TableType);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ExternGlobalType(pub GlobalType);

#[derive(Clone, Debug, PartialEq, Eq)]
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

pub type LabelIdx = u32;

pub type LocalIdx = u32;

pub type GlobalIdx = u32;

pub type Name = String;
