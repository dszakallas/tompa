use super::types::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub enum BinOp {}

pub enum TestOp {}

pub enum RelOp {}

pub enum CvtOp {}

pub struct Drop {}

pub struct Select {}

pub struct LocalGet(u32);

pub struct LocalSet(u32);

pub struct LocalTee(u32);

pub struct GlobalGet(u32);

pub struct GlobalSet(u32);

pub enum BitWidth {
    _8,
    _16,
    _32,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Load {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Store {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub struct MemorySize {}

pub struct MemoryGrow {}

pub struct Nop {}

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    BlockInstr(BlockInstr),
    Const(Const)
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockInstr {
    Block(Block),
    Loop(Loop),
    IfElse(IfElse)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub result: Option<ValType>,
    pub instrs: Vec<Instr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub result: Option<ValType>,
    pub instrs: Vec<Instr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfElse {
    pub result: Option<ValType>,
    pub if_instrs: Vec<Instr>,
    pub else_instrs: Vec<Instr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub instr: Vec<Instr>,
}
