use nom::{IResult, bytes::complete::take, combinator::map, error::{ErrorKind, ParseError}, sequence::pair};

use crate::{ast::{FuncRef, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType}, format::binary::values::{byte, uxx, vec_}};

use super::ByteStream;


#[inline]
pub fn valtype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, ValType, I::Error>
{
    let (i, bi) = take(1u8)(i)?;
    let b = bi.as_bytes()[0];

    match b {
        0x7F => Ok((i, ValType::I32)),
        0x7E => Ok((i, ValType::I64)),
        0x7D => Ok((i, ValType::F32)),
        0x7C => Ok((i, ValType::F64)),
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Alt)))
    }
}


#[inline]
pub fn resulttype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, ResultType, I::Error>
{
    let (i, bi) = take(1u8)(i)?;
    let b = bi.as_bytes()[0];

    match b {
        0x40 => Ok((i, ResultType { valtype: None})),
        0x7F => Ok((i, ResultType { valtype: Some(ValType::I32) })),
        0x7E => Ok((i, ResultType { valtype: Some(ValType::I64) })),
        0x7D => Ok((i, ResultType { valtype: Some(ValType::F32) })),
        0x7C => Ok((i, ResultType { valtype: Some(ValType::F64) })),
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Alt)))
    }
}

#[inline]
pub fn functype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, FuncType, I::Error>
{
    let (i, bi) = take(1u8)(i)?;
    let b = bi.as_bytes()[0];

    let (i, _) = byte(0x60u8)(i)?;

    let (i, parameters) = vec_(valtype)(i)?;
    let (i, results) = vec_(valtype)(i)?;

    Ok((i, FuncType { parameters, results }))
}

#[inline]
pub fn limits<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, Limits, I::Error>
{
    let (i, bi) = take(1u8)(i)?;
    let b = bi.as_bytes()[0];

    match b {
        0x00 => {
            let (i, min) = uxx::<u32, I>(i)?;
            Ok((i, Limits { min: min, max: None }))
        },
        0x01 => {
            let (i, min) = uxx::<u32, I>(i)?;
            let (i, max) = uxx::<u32, I>(i)?;
            Ok((i, Limits { min: min, max: Some(max) }))
        }
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Alt)))
    }
}

#[inline]
pub fn memtype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, MemType, I::Error>
{
    map(limits, |limits| MemType { limits: limits })(i)
}


#[inline]
pub fn tabletype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, TableType, I::Error>
{
    let (i, _) = byte(0x70)(i)?;
    let (i, l) = limits(i)?;
    Ok((i, TableType { limits: l, elemtype: FuncRef {}}))
}


#[inline]
pub fn globaltype<'a, I: ByteStream<'a> + 'a>(i: I) -> IResult<I, GlobalType, I::Error>
{
    let (i, (vt, mut_)) = pair(valtype, |i: I| {
        let (i, bi) = take(1u8)(i)?;
        let b = bi.as_bytes()[0];
        match b {
            0x00 => Ok((i, Mut::Const)),
            0x01 => Ok((i, Mut::Var)),
            _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Alt)))
        }
    })(i)?;
    Ok((i, GlobalType { valtype: vt, mut_: mut_ }))
}
