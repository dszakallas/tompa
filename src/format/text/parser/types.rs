
use std::ops::{RangeFrom};

use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice, Offset};
use nom::branch::alt;
use nom::bytes::complete::{tag};
use nom::character::complete::{anychar, char, digit1, hex_digit1, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::error::{ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{many0, many1, fold_many0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use crate::syntax::types::{FuncRef, GlobalType, Limits, MemType, Mut, TableType, ValType};

use crate::format::text::parser::values::uxx;
use crate::format::text::parser::{keyword, block, ParserInput, id};
use crate::format::text::lexer::{Token, AsStr, AsChar, LexerInput};
use crate::format::text::lexer::keyword::Keyword::*;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct FuncType<'a, I> {
    pub parameters: Vec<(Option<&'a I>, ValType)>,
    pub results: Vec<ValType>,
}

#[inline]
pub fn valtype<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, ValType, E>
    where
        I1: ParserInput<'a, I2>,
{
    alt((
        value(ValType::I32, keyword(I32)),
        value(ValType::I64, keyword(I64)),
        value(ValType::F32, keyword(F32)),
        value(ValType::F64, keyword(F64))
    ))(i)
}

#[inline]
pub fn globaltype<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, GlobalType, E>
    where
        I1: ParserInput<'a, I2>,
{
    alt((
        map(valtype, |valtype| GlobalType { mut_: Mut::Const, valtype }),
        map(block(preceded(keyword(Mut), valtype)), |valtype| GlobalType { mut_: Mut::Var, valtype })
    ))(i)
}

#[inline]
pub fn limits<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a, E2: ParseError<I2> + 'a>(i: I1) -> IResult<I1, Limits, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    map(
        pair(uxx::<u32, I1, E1, I2, E2>, opt(uxx::<u32, I1, E1, I2, E2>)),
        |(min, max)| Limits { min, max },
    )(i)
}

#[inline]
pub fn tabletype<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a, E2: ParseError<I2> + 'a>(i: I1) -> IResult<I1, TableType, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    map(
        terminated(limits::<I1, E1, I2, E2>, keyword(Funcref)),
        |limits| TableType { limits, elemtype: FuncRef {} },
    )(i)
}

#[inline]
pub fn memtype<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a, E2: ParseError<I2> + 'a>(i: I1) -> IResult<I1, MemType, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>, {
    map(limits::<I1, E1, I2, E2>, |limits| MemType { limits })(i)
}

#[inline]
pub fn functype<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a, E2: ParseError<I2> + 'a>(i: I1) -> IResult<I1, FuncType<'a, I2>, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    map(block(preceded(keyword(Func), tuple((params::<I1, E1, I2>, results::<I1, E1, I2>)))), |(parameters, results)| {
        FuncType::<'a, I2> { parameters, results }
    })(i)
}

#[inline]
pub fn params<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, Vec<(Option<&'a I2>, ValType)>, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>, {

    fold_many0(
        block(preceded(keyword(Param), alt((
            map(pair(id, valtype), |(id, valtype)| vec![(Some(id), valtype)]),
            map(many0(valtype), |v| v.into_iter().map(|valtype| (None, valtype)).collect()),
        )))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn results<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, Vec<ValType>, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>, {

    fold_many0(
        block(preceded(keyword(Result), many0(valtype))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn resulttype<'a, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, ValType, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>, {
    block(preceded(keyword(Result), valtype))(i)
}

//
// #[cfg(test)]
// mod test {
//
//     use nom::error::{ErrorKind};
//
//
//     use super::*;
//
//     type FastError<T> = (T, ErrorKind);
//
//     #[test]
//     fn test_valtype() {
//         assert_eq!(valtype::<&str, FastError<&str>>("i32"), Ok(("", ValType::I32)));
//     }
//
//     #[test]
//     fn test_globaltype() {
//         assert_eq!(
//             globaltype::<'static, &str, FastError<&str>>("f64"),
//             Ok(("", GlobalType { mut_: Mut::Const, valtype: ValType::F64 }))
//         );
//         assert_eq!(
//             globaltype::<'static, &str, FastError<&str>>("(mut f32)"),
//             Ok(("", GlobalType { mut_: Mut::Var, valtype: ValType::F32 }))
//         );
//     }
//
//     #[test]
//     fn test_tabletype() {
//         assert_eq!(
//             tabletype::<'static, &str, FastError<&str>>("13 0x001F funcref o"),
//             Ok(("o", TableType { limits: Limits { min: 13u32, max: Some(31u32) }, elemtype: FuncRef {} }))
//         );
//         tabletype::<'static, &str, FastError<&str>>("-12 0x001F funcref").unwrap_err();
//     }
//
//     #[test]
//     fn test_functype() {
//         assert_eq!(
//             functype::<'static, &str, FastError<&str>>("(func (param $abcd i32) (result f64))("),
//             Ok(("(", FuncType { parameters: vec![ValType::I32], results: vec![ValType::F64] }))
//         )
//     }
// }
