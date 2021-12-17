use nom::combinator::{map, opt};
use nom::error::{ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::{preceded, terminated};
use nom::{IResult, bytes::complete::take};

use super::BinaryInput;
use super::types::resulttype;
use super::values::byte;
use crate::ast::{Block, If, Instruction, Loop, Memarg, ResultType, LocalIdx, GlobalIdx};
use crate::format::binary::values::{uxx, vec_, ixx, fxx};

use phf::phf_map;

macro_rules! def_instruction_bin_cps { ($cb:ident($($args:tt)*)) => { $cb!{
$($args)*
/*
Keep tabulated with column -t -s ";" -o ";" table > table_organized

Name             ; Parameters                                                                           ; Opcode;            ;
*/
Unreachable      ; ()                                                                                   ; 0x00u8; NoArg()    ;
Nop              ; ()                                                                                   ; 0x01u8; NoArg()    ;
Block            ; (result: ResultType, instrs: Vec<Instruction>)                                       ; 0x02u8; Block()    ;
Loop             ; (result: ResultType, instrs: Vec<Instruction>)                                       ; 0x03u8; Loop()     ;
If               ; (result: ResultType, if_instrs: Vec<Instruction>, else_instrs: Vec<Instruction>)     ; 0x04u8; If()       ;
Br               ; ()                                                                                   ; 0x0cu8; NoArg()    ;
BrIf             ; ()                                                                                   ; 0x0du8; NoArg()    ;
BrTable          ; ()                                                                                   ; 0x0eu8; NoArg()    ;
Return           ; ()                                                                                   ; 0x0fu8; NoArg()    ;
Call             ; ()                                                                                   ; 0x10u8; NoArg()    ;
CallIndirect     ; ()                                                                                   ; 0x11u8; NoArg()    ;
Drop             ; ()                                                                                   ; 0x1au8; NoArg()    ;
Select           ; ()                                                                                   ; 0x1bu8; NoArg()    ;
LocalGet         ; (localidx: LocalIdx)                                                                 ; 0x20u8; LocalIdx() ;
LocalSet         ; (localidx: LocalIdx)                                                                 ; 0x21u8; LocalIdx() ;
LocalTee         ; (localidx: LocalIdx)                                                                 ; 0x22u8; LocalIdx() ;
GlobalGet        ; (globalidx: GlobalIdx)                                                               ; 0x23u8; GlobalIdx();
GlobalSet        ; (globalidx: GlobalIdx)                                                               ; 0x24u8; GlobalIdx();
I32Load          ; (memarg: Memarg)                                                                     ; 0x28u8; MemLs()    ;
I64Load          ; (memarg: Memarg)                                                                     ; 0x29u8; MemLs()    ;
F32Load          ; (memarg: Memarg)                                                                     ; 0x2au8; MemLs()    ;
F64Load          ; (memarg: Memarg)                                                                     ; 0x2bu8; MemLs()    ;
I32Load8S        ; (memarg: Memarg)                                                                     ; 0x2cu8; MemLs()    ;
I32Load8U        ; (memarg: Memarg)                                                                     ; 0x2du8; MemLs()    ;
I32Load16S       ; (memarg: Memarg)                                                                     ; 0x2eu8; MemLs()    ;
I32Load16U       ; (memarg: Memarg)                                                                     ; 0x2fu8; MemLs()    ;
I64Load8S        ; (memarg: Memarg)                                                                     ; 0x30u8; MemLs()    ;
I64Load8U        ; (memarg: Memarg)                                                                     ; 0x31u8; MemLs()    ;
I64Load16S       ; (memarg: Memarg)                                                                     ; 0x32u8; MemLs()    ;
I64Load16U       ; (memarg: Memarg)                                                                     ; 0x33u8; MemLs()    ;
I64Load32S       ; (memarg: Memarg)                                                                     ; 0x34u8; MemLs()    ;
I64Load32U       ; (memarg: Memarg)                                                                     ; 0x35u8; MemLs()    ;
I32Store         ; (memarg: Memarg)                                                                     ; 0x36u8; MemLs()    ;
I64Store         ; (memarg: Memarg)                                                                     ; 0x37u8; MemLs()    ;
F32Store         ; (memarg: Memarg)                                                                     ; 0x38u8; MemLs()    ;
F64Store         ; (memarg: Memarg)                                                                     ; 0x39u8; MemLs()    ;
I32Store8        ; (memarg: Memarg)                                                                     ; 0x3au8; MemLs()    ;
I32Store16       ; (memarg: Memarg)                                                                     ; 0x3bu8; MemLs()    ;
I64Store8        ; (memarg: Memarg)                                                                     ; 0x3cu8; MemLs()    ;
I64Store16       ; (memarg: Memarg)                                                                     ; 0x3du8; MemLs()    ;
I64Store32       ; (memarg: Memarg)                                                                     ; 0x3eu8; MemLs()    ;
MemorySize       ; ()                                                                                   ; 0x3fu8; MemGrow()  ;
MemoryGrow       ; ()                                                                                   ; 0x40u8; MemGrow()  ;
I32Const         ; (param: u32)                                                                         ; 0x41u8; ConstI32() ;
I64Const         ; (param: u64)                                                                         ; 0x42u8; ConstI64() ;
F32Const         ; (param: f32)                                                                         ; 0x43u8; ConstF32() ;
F64Const         ; (param: f64)                                                                         ; 0x44u8; ConstF64() ;
I32Eqz           ; ()                                                                                   ; 0x45u8; NoArg()    ;
I32Eq            ; ()                                                                                   ; 0x46u8; NoArg()    ;
I32Ne            ; ()                                                                                   ; 0x47u8; NoArg()    ;
I32LtS           ; ()                                                                                   ; 0x48u8; NoArg()    ;
I32LtU           ; ()                                                                                   ; 0x49u8; NoArg()    ;
I32GtS           ; ()                                                                                   ; 0x4au8; NoArg()    ;
I32GtU           ; ()                                                                                   ; 0x4bu8; NoArg()    ;
I32LeS           ; ()                                                                                   ; 0x4cu8; NoArg()    ;
I32LeU           ; ()                                                                                   ; 0x4du8; NoArg()    ;
I32GeS           ; ()                                                                                   ; 0x4eu8; NoArg()    ;
I32GeU           ; ()                                                                                   ; 0x4fu8; NoArg()    ;
I64Eqz           ; ()                                                                                   ; 0x50u8; NoArg()    ;
I64Eq            ; ()                                                                                   ; 0x51u8; NoArg()    ;
I64Ne            ; ()                                                                                   ; 0x52u8; NoArg()    ;
I64LtS           ; ()                                                                                   ; 0x53u8; NoArg()    ;
I64LtU           ; ()                                                                                   ; 0x54u8; NoArg()    ;
I64GtS           ; ()                                                                                   ; 0x55u8; NoArg()    ;
I64GtU           ; ()                                                                                   ; 0x56u8; NoArg()    ;
I64LeS           ; ()                                                                                   ; 0x57u8; NoArg()    ;
I64LeU           ; ()                                                                                   ; 0x58u8; NoArg()    ;
I64GeS           ; ()                                                                                   ; 0x59u8; NoArg()    ;
I64GeU           ; ()                                                                                   ; 0x5au8; NoArg()    ;
F32Eq            ; ()                                                                                   ; 0x5bu8; NoArg()    ;
F32Ne            ; ()                                                                                   ; 0x5cu8; NoArg()    ;
F32Lt            ; ()                                                                                   ; 0x5du8; NoArg()    ;
F32Gt            ; ()                                                                                   ; 0x5eu8; NoArg()    ;
F32Le            ; ()                                                                                   ; 0x5fu8; NoArg()    ;
F32Ge            ; ()                                                                                   ; 0x60u8; NoArg()    ;
F64Eq            ; ()                                                                                   ; 0x61u8; NoArg()    ;
F64Ne            ; ()                                                                                   ; 0x62u8; NoArg()    ;
F64Lt            ; ()                                                                                   ; 0x63u8; NoArg()    ;
F64Gt            ; ()                                                                                   ; 0x64u8; NoArg()    ;
F64Le            ; ()                                                                                   ; 0x65u8; NoArg()    ;
F64Ge            ; ()                                                                                   ; 0x66u8; NoArg()    ;
I32Clz           ; ()                                                                                   ; 0x67u8; NoArg()    ;
I32Ctz           ; ()                                                                                   ; 0x68u8; NoArg()    ;
I32Popcnt        ; ()                                                                                   ; 0x69u8; NoArg()    ;
I32Add           ; ()                                                                                   ; 0x6au8; NoArg()    ;
I32Sub           ; ()                                                                                   ; 0x6bu8; NoArg()    ;
I32Mul           ; ()                                                                                   ; 0x6cu8; NoArg()    ;
I32DivS          ; ()                                                                                   ; 0x6du8; NoArg()    ;
I32DivU          ; ()                                                                                   ; 0x6eu8; NoArg()    ;
I32RemS          ; ()                                                                                   ; 0x6fu8; NoArg()    ;
I32RemU          ; ()                                                                                   ; 0x70u8; NoArg()    ;
I32And           ; ()                                                                                   ; 0x71u8; NoArg()    ;
I32Or            ; ()                                                                                   ; 0x72u8; NoArg()    ;
I32Xor           ; ()                                                                                   ; 0x73u8; NoArg()    ;
I32Shl           ; ()                                                                                   ; 0x74u8; NoArg()    ;
I32ShrS          ; ()                                                                                   ; 0x75u8; NoArg()    ;
I32ShrU          ; ()                                                                                   ; 0x76u8; NoArg()    ;
I32Rotl          ; ()                                                                                   ; 0x77u8; NoArg()    ;
I32Rotr          ; ()                                                                                   ; 0x78u8; NoArg()    ;
I64Clz           ; ()                                                                                   ; 0x79u8; NoArg()    ;
I64Ctz           ; ()                                                                                   ; 0x7au8; NoArg()    ;
I64Popcnt        ; ()                                                                                   ; 0x7bu8; NoArg()    ;
I64Add           ; ()                                                                                   ; 0x7cu8; NoArg()    ;
I64Sub           ; ()                                                                                   ; 0x7du8; NoArg()    ;
I64Mul           ; ()                                                                                   ; 0x7eu8; NoArg()    ;
I64DivS          ; ()                                                                                   ; 0x7fu8; NoArg()    ;
I64DivU          ; ()                                                                                   ; 0x80u8; NoArg()    ;
I64RemS          ; ()                                                                                   ; 0x81u8; NoArg()    ;
I64RemU          ; ()                                                                                   ; 0x82u8; NoArg()    ;
I64And           ; ()                                                                                   ; 0x83u8; NoArg()    ;
I64Or            ; ()                                                                                   ; 0x84u8; NoArg()    ;
I64Xor           ; ()                                                                                   ; 0x85u8; NoArg()    ;
I64Shl           ; ()                                                                                   ; 0x86u8; NoArg()    ;
I64ShrS          ; ()                                                                                   ; 0x87u8; NoArg()    ;
I64ShrU          ; ()                                                                                   ; 0x88u8; NoArg()    ;
I64Rotl          ; ()                                                                                   ; 0x89u8; NoArg()    ;
I64Rotr          ; ()                                                                                   ; 0x8au8; NoArg()    ;
F32Abs           ; ()                                                                                   ; 0x8bu8; NoArg()    ;
F32Neg           ; ()                                                                                   ; 0x8cu8; NoArg()    ;
F32Ceil          ; ()                                                                                   ; 0x8du8; NoArg()    ;
F32Floor         ; ()                                                                                   ; 0x8eu8; NoArg()    ;
F32Trunc         ; ()                                                                                   ; 0x8fu8; NoArg()    ;
F32Nearest       ; ()                                                                                   ; 0x90u8; NoArg()    ;
F32Sqrt          ; ()                                                                                   ; 0x91u8; NoArg()    ;
F32Add           ; ()                                                                                   ; 0x92u8; NoArg()    ;
F32Sub           ; ()                                                                                   ; 0x93u8; NoArg()    ;
F32Mul           ; ()                                                                                   ; 0x94u8; NoArg()    ;
F32Div           ; ()                                                                                   ; 0x95u8; NoArg()    ;
F32Min           ; ()                                                                                   ; 0x96u8; NoArg()    ;
F32Max           ; ()                                                                                   ; 0x97u8; NoArg()    ;
F32Copysign      ; ()                                                                                   ; 0x98u8; NoArg()    ;
F64Abs           ; ()                                                                                   ; 0x99u8; NoArg()    ;
F64Neg           ; ()                                                                                   ; 0x9au8; NoArg()    ;
F64Ceil          ; ()                                                                                   ; 0x9bu8; NoArg()    ;
F64Floor         ; ()                                                                                   ; 0x9cu8; NoArg()    ;
F64Trunc         ; ()                                                                                   ; 0x9du8; NoArg()    ;
F64Nearest       ; ()                                                                                   ; 0x9eu8; NoArg()    ;
F64Sqrt          ; ()                                                                                   ; 0x9fu8; NoArg()    ;
F64Add           ; ()                                                                                   ; 0xa0u8; NoArg()    ;
F64Sub           ; ()                                                                                   ; 0xa1u8; NoArg()    ;
F64Mul           ; ()                                                                                   ; 0xa2u8; NoArg()    ;
F64Div           ; ()                                                                                   ; 0xa3u8; NoArg()    ;
F64Min           ; ()                                                                                   ; 0xa4u8; NoArg()    ;
F64Max           ; ()                                                                                   ; 0xa5u8; NoArg()    ;
F64Copysign      ; ()                                                                                   ; 0xa6u8; NoArg()    ;
I32WrapI64       ; ()                                                                                   ; 0xa7u8; NoArg()    ;
I32TruncF32S     ; ()                                                                                   ; 0xa8u8; NoArg()    ;
I32TruncF32U     ; ()                                                                                   ; 0xa9u8; NoArg()    ;
I32TruncF64S     ; ()                                                                                   ; 0xaau8; NoArg()    ;
I32TruncF64U     ; ()                                                                                   ; 0xabu8; NoArg()    ;
I64ExtendI32S    ; ()                                                                                   ; 0xacu8; NoArg()    ;
I64ExtendI32U    ; ()                                                                                   ; 0xadu8; NoArg()    ;
I64TruncF32S     ; ()                                                                                   ; 0xaeu8; NoArg()    ;
I64TruncF32U     ; ()                                                                                   ; 0xafu8; NoArg()    ;
I64TruncF64S     ; ()                                                                                   ; 0xb0u8; NoArg()    ;
I64TruncF64U     ; ()                                                                                   ; 0xb1u8; NoArg()    ;
F32ConvertI32S   ; ()                                                                                   ; 0xb2u8; NoArg()    ;
F32ConvertI32U   ; ()                                                                                   ; 0xb3u8; NoArg()    ;
F32ConvertI64S   ; ()                                                                                   ; 0xb4u8; NoArg()    ;
F32ConvertI64U   ; ()                                                                                   ; 0xb5u8; NoArg()    ;
F32DemoteF64     ; ()                                                                                   ; 0xb6u8; NoArg()    ;
F64ConvertI32S   ; ()                                                                                   ; 0xb7u8; NoArg()    ;
F64ConvertI32U   ; ()                                                                                   ; 0xb8u8; NoArg()    ;
F64ConvertI64S   ; ()                                                                                   ; 0xb9u8; NoArg()    ;
F64ConvertI64U   ; ()                                                                                   ; 0xbau8; NoArg()    ;
F64PromoteF32    ; ()                                                                                   ; 0xbbu8; NoArg()    ;
I32ReinterpretF32; ()                                                                                   ; 0xbcu8; NoArg()    ;
I64ReinterpretF64; ()                                                                                   ; 0xbdu8; NoArg()    ;
F32ReinterpretI32; ()                                                                                   ; 0xbeu8; NoArg()    ;
F64ReinterpretI64; ()                                                                                   ; 0xbfu8; NoArg()    ;
}};
}


#[derive(Clone, Debug)]
pub enum InstrParseType {
    NoArg(fn() -> Instruction),
    Block(fn(ResultType, Vec<Instruction>) -> Instruction),
    Loop(fn(ResultType, Vec<Instruction>) -> Instruction),
    If(fn(ResultType, Vec<Instruction>, Vec<Instruction>) -> Instruction),
    LocalIdx(fn(LocalIdx) -> Instruction),
    GlobalIdx(fn(GlobalIdx) -> Instruction),
    // LabelIdx(fn(LabelIdx) -> Instruction),
    // LabelIdxN(fn(Vec<LabelIdx>) -> Instruction),
    // FuncIdx(fn(FuncIdx) -> Instruction),
    // TypeIdx(fn(TypeIdx) -> Instruction),
    MemLs(fn(Memarg) -> Instruction),
    MemGrow(fn() -> Instruction),
    ConstI32(fn(u32) -> Instruction),
    ConstI64(fn(u64) -> Instruction),
    ConstF32(fn(f32) -> Instruction),
    ConstF64(fn(f64) -> Instruction),
}

macro_rules! def_instruction_bin_parser_phf {
    ($($id:ident; ($($pname:ident: $pty:ty),*); $opcode:expr; $parse_tpe:ident($($arg:expr),*);)*) => {
        pub(crate) static INSTR_PARSERS: phf::Map<u8, InstrParseType> = phf_map! {
            $(
                $opcode => InstrParseType::$parse_tpe($($arg,)* |$($pname: $pty),*| crate::ast::Instruction::$id(crate::ast::$id { $($pname),* }))
            ),*
        };
    }
}

def_instruction_bin_cps!(def_instruction_bin_parser_phf());

fn block<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Block, I::Error> {
    let (i, rt) = resulttype(i)?;
    let (i, instrs) = many0(instr)(i)?;
    let (i, _) = byte(0x0B)(i)?;
    Ok((i, Block { result: rt, instrs }))
}

fn loop_<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Loop, I::Error> {
    let (i, rt) = resulttype(i)?;
    let (i, instrs) = many0(instr)(i)?;
    let (i, _) = byte(0x0B)(i)?;
    Ok((i, Loop { result: rt, instrs }))
}

fn if_<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, If, I::Error> {
    let (i, rt) = resulttype(i)?;
    let (i, if_instrs) = many0(instr)(i)?;
    let (i, else_instrs) = map(opt(preceded(byte(0x05), many0(instr))), |e| e.unwrap_or_default())(i)?;
    let (i, _) = byte(0x0B)(i)?;
    Ok((i, If { result: rt, if_instrs, else_instrs }))
}

fn memarg<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Memarg, I::Error> {
    let (i, align) = uxx::<u32, I>(i)?;
    let (i, offset) = uxx::<u32, I>(i)?;
    Ok((i, Memarg { offset, align }))
} 

fn memgrow<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, (), I::Error> {
    let (i, _) = byte(0x00)(i)?;
    Ok((i, ()))
} 

pub fn instr<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Instruction, I::Error> {
    let (i, ib) = take(1u8)(i)?;
    let b = ib.as_bytes()[0];
    match INSTR_PARSERS.get(&b) {
        Some(InstrParseType::Block(_)) => map(block, |v| Instruction::Block(v))(i),
        Some(InstrParseType::Loop(_)) => map(loop_, |v| Instruction::Loop(v))(i),
        Some(InstrParseType::If(_)) => map(if_, |v| Instruction::If(v))(i),
        Some(InstrParseType::NoArg(constr)) => Ok((i, constr())),
        Some(InstrParseType::LocalIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        Some(InstrParseType::GlobalIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        // Some(InstrParseType::LabelIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        // Some(InstrParseType::LabelIdxN(constr)) => map(pair(vec_, uxx::<u32>), constr)(i),
        // Some(InstrParseType::FuncIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        // Some(InstrParseType::TypeIdx(constr)) => {
        //     let (ok_i, (typeidx, locals)) = typeuse(ctx)(i.clone())?;
        //     for l in locals.iter() {
        //         if let Some(_) = l {
        //             // id ctx most be empty
        //             return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)));
        //         }
        //     };
        //     Ok((ok_i, constr(typeidx)))
        // }
        Some(InstrParseType::MemLs(constr)) => map(memarg, constr)(i),
        Some(InstrParseType::MemGrow(constr)) => map(memgrow, |_| constr())(i),
        Some(InstrParseType::ConstI32(constr)) => map(ixx::<u32, I>, constr)(i),
        Some(InstrParseType::ConstI64(constr)) => map(ixx::<u64, I>, constr)(i),
        Some(InstrParseType::ConstF32(constr)) => map(fxx::<f32, I>, constr)(i),
        Some(InstrParseType::ConstF64(constr)) => map(fxx::<f64, I>, constr)(i),
        None => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char))),
    }
}

pub fn expr<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Instruction>, I::Error> {
    terminated(many0(instr), byte(0x0B))(i)
}
