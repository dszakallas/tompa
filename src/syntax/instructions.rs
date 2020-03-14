use super::types::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Load {
   pub valtype: ValType,
   pub storage_size: Option<(u32, Sx)>,
   pub memarg: MemArg
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Store {
    pub valtype: ValType,
    pub storage_size: Option<u32>,
    pub memarg: MemArg
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Const {
    pub fn valtype(&self) -> ValType {
        match &self {
            Const::I32(_) => ValType::I32,
            Const::F32(_) => ValType::F32,
            Const::I64(_) => ValType::I64,
            Const::F64(_) => ValType::F64,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Unreachable {}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Nop {}


//#[derive(Copy, Clone, Debug, PartialEq)]
//pub struct Unop {
//    pub type_: UnopType
//    pub
//}

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    Const(Const),
    Load(Load),
    Store(Store),
    Block(Block),
    Loop(Loop),
    IfElse(IfElse),
}

//#[derive(Clone, Debug, PartialEq)]
//pub enum BlockInstr {
//    Block(Block),
//    Loop(Loop),
//    IfElse(IfElse)
//}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Block {
    pub result: Option<ValType>,
    pub instrs: Vec<Instr>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Loop {
    pub result: Option<ValType>,
    pub instrs: Vec<Instr>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IfElse {
    pub result: Option<ValType>,
    pub if_instrs: Vec<Instr>,
    pub else_instrs: Vec<Instr>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Br {
    pub labelidx: LabelIdx
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct BrIf {
    pub labelidx: LabelIdx
}

#[derive(Clone, Debug, PartialEq)]
pub struct BrTable {
    pub labelidxs: Vec<LabelIdx>,
}


#[derive(Clone, Debug, PartialEq, Default)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}
