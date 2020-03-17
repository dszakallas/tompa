
use std::ops::{RangeFrom};



use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice, Offset};
use nom::branch::alt;
use nom::bytes::complete::{tag};
use nom::character::complete::{anychar, char, digit1, hex_digit1, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::error::{ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use crate::syntax::types::{FuncRef, FuncType, GlobalType, Limits, MemType, Mut, TableType, ValType};

//use crate::format::text::parser::lexical::ws;
//use crate::format::text::parser::lexical::AsChar;
//use crate::format::text::parser::lexical::token;
use crate::format::text::parser::values::uxx;
use crate::format::text::parser::{keyword, block};
use crate::format::text::lexer::{Token, AsStr, AsChar};
use crate::format::text::lexer::keyword::Keyword::*;


#[inline]
fn valtype<'a, I: 'a, E: ParseError<I> + 'a, I2: 'a>(i: I) -> IResult<I, ValType, E>
    where
        I: Clone
        + InputTake
        + PartialEq
        + InputLength
        + InputIter<Item=&'a Token<I2>>
        + Slice<RangeFrom<usize>>
        + Compare<&'static str>, {
    alt((
        value(ValType::I32, keyword(I32)),
        value(ValType::I64, keyword(I64)),
        value(ValType::F32, keyword(F32)),
        value(ValType::F64, keyword(F64))
    ))(i)
}

#[inline]
fn globaltype<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, GlobalType, E>
    where
        I1: Clone
        + InputTake
        + PartialEq
        + InputLength
        + InputIter<Item=&'a Token<I2>>
        + Slice<RangeFrom<usize>>
        + Compare<&'static str>, {
    alt((
        map(valtype, |valtype| GlobalType { mut_: Mut::Const, valtype }),
        map(block(preceded(keyword(Mut), valtype)), |valtype| GlobalType { mut_: Mut::Var, valtype })
    ))(i)
}

#[inline]
fn limits<'a,
    I1: 'a, E1: ParseError<I1> + 'a,
    I2: 'a, E2: ParseError<I2> + 'a,
    Item2
>(i: I1) -> IResult<I1, Limits, E1>
    where
        I1: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter<Item=&'a Token<I2>>
        + InputLength,

        I2: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + InputIter<Item=Item2>
        + InputLength
        + InputTakeAtPosition<Item=Item2>
        + InputTake
        + Offset
        + Compare<&'static str>
        + AsStr<'a>,

        Item2: NomAsChar + AsChar, {
    map(
        pair(uxx::<u32, I1, E1, I2, E2, Item2>, opt(uxx::<u32, I1, E1, I2, E2, Item2>)),
        |(min, max)| Limits { min, max },
    )(i)
}

// #[inline]
// fn tabletype<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, TableType, E>
//     where
//         I: Clone
//         + PartialEq
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + InputIter
//         + InputLength
//         + InputTakeAtPosition
//         + InputTake
//         + AsStr<'a>
//         + Compare<&'static str>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     map(
//         terminated(limits, token(tag("funcref"))),
//         |limits| TableType { limits, elemtype: FuncRef {} },
//     )(i)
// }

// #[inline]
// fn memtype<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, MemType, E>
//     where
//         I: Clone
//         + PartialEq
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + InputIter
//         + InputLength
//         + InputTakeAtPosition
//         + InputTake
//         + AsStr<'a>
//         + Compare<&'static str>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     map(limits, |limits| MemType { limits })(i)
// }
//
// #[inline]
// fn functype<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, FuncType, E>
//     where
//         I: Clone
//         + PartialEq
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + InputIter
//         + InputLength
//         + InputTakeAtPosition
//         + InputTake
//         + AsStr<'a>
//         + Compare<&'static str>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     map(block!(preceded(token(tag("func")), tuple((params, results)))), |(parameters, results)| {
//         FuncType { parameters, results }
//     })(i)
// }
//
// #[inline]
// fn param<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, ValType, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     block!(
//         preceded(
//             token(tag("param")),
//             map(tuple((opt(token(id)), token(valtype))), |(_id, valtype)| valtype)
//         )
//     )(i)
// }
//
// #[inline]
// fn params<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, Vec<ValType>, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     block!(
//         preceded(
//             token(tag("param")),
//             alt((
//                 map(tuple((token(id), token(valtype))), |(_id, valtype)| vec![valtype]),
//                 many1(token(valtype))
//             ))
//         ))(i)
// }
//
// #[inline]
// pub fn result<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, ValType, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     block!(preceded(token(tag("result")), token(valtype)))(i)
// }
//
// #[inline]
// fn results<'a, I: 'a, E: ParseError<I> + 'a>(i: I) -> IResult<I, Vec<ValType>, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     block!(
//         preceded(
//             token(tag("result")),
//             alt((
//                 map(tuple((token(id), token(valtype))), |(_id, valtype)| vec![valtype]),
//                 many1(token(valtype)),
//             ))
//         ))(i)
// }
//
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
