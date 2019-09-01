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

//#[derive(Clone, Debug)]
//pub struct Load {
//    pub t: ValType,
//    pub memarg: MemArg,
//    pub n: Option<(ValType, Sx)>,
//}
//
//#[derive(Clone, Debug)]
//pub struct Store {
//    pub t: ValType,
//    pub memarg: MemArg,
//    pub n: Option<ValType>,
//}

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
pub enum Instruction {
    Const(Const),
    //    Load(Load),
    //    Store(Store)
}

#[derive(Clone, Debug)]
pub struct Block {
    pub result: Option<ValType>,
    pub instr: Vec<Instruction>,
}

//#[derive(Clone, Debug)]
//pub struct IfElse {
//    pub label: Option<LabelIdx>,
//    pub result: Option<ValType>,
//    pub if_instr: Vec<Instruction>,
//    pub else_instr: Vec<Instruction>,
//}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub instr: Vec<Instruction>,
}
