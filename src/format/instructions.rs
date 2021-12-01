use crate::ast::{FuncIdx, GlobalIdx, Instruction, LabelIdx, LocalIdx, Memarg, ResultType, TypeIdx};
use phf::phf_map;


#[derive(Clone, Debug)]
pub enum InstrParseType {
    NoArg(fn() -> Instruction),
    Block(fn(ResultType, Vec<Instruction>) -> Instruction),
    Loop(fn(ResultType, Vec<Instruction>) -> Instruction),
    If(fn(ResultType, Vec<Instruction>, Vec<Instruction>) -> Instruction),
    LocalIdx(fn(LocalIdx) -> Instruction),
    GlobalIdx(fn(GlobalIdx) -> Instruction),
    LabelIdx(fn(LabelIdx) -> Instruction),
    LabelIdxN(fn(Vec<LabelIdx>) -> Instruction),
    FuncIdx(fn(FuncIdx) -> Instruction),
    TypeIdx(fn(TypeIdx) -> Instruction),
    MemLs(u32, fn(Memarg) -> Instruction),
    ConstI32(fn(u32) -> Instruction),
    ConstI64(fn(u64) -> Instruction),
    ConstF32(fn(f32) -> Instruction),
    ConstF64(fn(f64) -> Instruction),
}

macro_rules! def_instruction_parser_helpers {
    ($($id:ident; ($($pname:ident: $pty:ty),*); $_text:expr; $opcode:expr; $parse_tpe:ident($($arg:expr),*); $_rule:expr;)*) => {
        pub(crate) static INSTR_PARSERS: phf::Map<u8, InstrParseType> = phf_map! {
            $(
                $opcode => InstrParseType::$parse_tpe($($arg,)* |$($pname: $pty),*| crate::ast::Instruction::$id(crate::ast::$id { $($pname),* }))
            ),*
        };
    }
}

instruction_defs_cps!(def_instruction_parser_helpers());
