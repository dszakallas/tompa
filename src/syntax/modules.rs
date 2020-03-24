use super::instructions::*;
use super::types::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub type_idx: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Table {
    pub tpe: TableType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mem {
    pub tpe: MemType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Global {
    pub tpe: GlobalType,
    pub expr: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Export {
    pub name: Name,
    pub expr: ExportDesc,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternFunc(pub FuncIdx);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternTable(pub TableIdx);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternMem(pub MemIdx);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExternGlobal(pub GlobalIdx);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExportDesc {
    Func(ExternFunc),
    Table(ExternTable),
    Mem(ExternMem),
    Global(ExternGlobal),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportDesc {
    Func(ExternFunc),
    Table(ExternTableType),
    Mem(ExternMemType),
    Global(ExternGlobalType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub expr: ImportDesc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElementSegment {
    pub table: TableIdx,
    pub offset: Vec<Instruction>,
    pub init: Vec<FuncIdx>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataSegment {
    pub data: MemIdx,
    pub offset: Vec<Instruction>,
    pub init: Vec<u8>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Start(pub FuncIdx);

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Function>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elem: Vec<ElementSegment>,
    pub data: Vec<DataSegment>,
    pub start: Option<Start>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}
