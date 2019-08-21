use super::types::*;
use super::instructions::*;

#[derive(Clone, Debug)]
pub struct Function {
    pub type_idx: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub struct Table {
    pub tpe: TableType
}

#[derive(Clone, Debug)]
pub struct Mem {
    pub tpe: MemType
}

#[derive(Clone, Debug)]
pub struct Global {
    pub tpe: GlobalType,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Export {
    pub name: Name,
    pub expr: ExportDesc,
}

#[derive(Copy, Clone, Debug)]
pub struct ExternFunc(pub FuncIdx);

#[derive(Copy, Clone, Debug)]
pub struct ExternTable(pub TableIdx);

#[derive(Copy, Clone, Debug)]
pub struct ExternMem(pub MemIdx);

#[derive(Copy, Clone, Debug)]
pub struct ExternGlobal(pub GlobalIdx);


#[derive(Copy, Clone, Debug)]
pub enum ExportDesc {
    Func(ExternFunc),
    Table(ExternTable),
    Mem(ExternMem),
    Global(ExternGlobal)
}


