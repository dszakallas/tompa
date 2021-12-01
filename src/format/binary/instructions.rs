use nom::combinator::{map, opt};
use nom::error::{ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::preceded;
use nom::{IResult, bytes::complete::take};

use super::BinaryInput;
use super::types::resulttype;
use super::values::byte;
use crate::ast::{Block, If, Instruction, Loop};
use crate::format::binary::values::{uxx, vec_};
use crate::format::instructions::{INSTR_PARSERS, InstrParseType};

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

fn instr<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Instruction, I::Error> {
    let (i, ib) = take(1u8)(i)?;
    let b = ib.as_bytes()[0];
    match INSTR_PARSERS.get(&b) {
        Some(InstrParseType::Block(_)) => map(block, |v| Instruction::Block(v))(i),
        Some(InstrParseType::Loop(_)) => map(loop_, |v| Instruction::Loop(v))(i),
        Some(InstrParseType::If(_)) => map(if_, |v| Instruction::If(v))(i),
        Some(InstrParseType::NoArg(constr)) => Ok((i, constr())),
        Some(InstrParseType::LocalIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        Some(InstrParseType::GlobalIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        Some(InstrParseType::LabelIdx(constr)) => map(uxx::<u32, I>, constr)(i),
        // Some(InstrParseType::LabelIdxN(constr)) => map(pair(vec_, uxx::<u32>), constr)(i),
        Some(InstrParseType::FuncIdx(constr)) => map(uxx::<u32, I>, constr)(i),
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
        // Some(InstrParseType::MemLs(default_align, constr)) => {
        //     map(memarg(*default_align), constr)(i)
        // },
        // Some(InstrParseType::ConstI32(constr)) => map(ixx::<u32, I>, constr)(i),
        // Some(InstrParseType::ConstI64(constr)) => map(ixx::<u64, I>, constr)(i),
        // Some(InstrParseType::ConstF32(constr)) => map(fxx::<f32, I>, constr)(i),
        // Some(InstrParseType::ConstF64(constr)) => map(fxx::<f64, I>, constr)(i),
        // TODO use None for safety!
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char))),
    };
    todo!()
}
