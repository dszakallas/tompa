use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice, Offset};
use nom::branch::alt;

use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::error::{ParseError};

use nom::multi::{many0, fold_many0};
use nom::sequence::{pair, preceded, terminated, tuple};

use crate::ast::{FuncRef, GlobalType, Limits, MemType, Mut, TableType, ValType};

use crate::format::text::parser::values::uxx;
use crate::format::text::parser::{keyword, par, ParserInput, id};
use crate::format::text::lexer::{LexerInput};
use crate::format::text::lexer::keyword::Keyword::*;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct FuncType<'a, I> {
    pub parameters: Vec<(Option<&'a I>, ValType)>,
    pub results: Vec<ValType>,
}

#[inline]
pub fn valtype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, ValType, I::Error>
    where I::Inner: LexerInput<'a>
{
    alt((
        value(ValType::I32, keyword(I32)),
        value(ValType::I64, keyword(I64)),
        value(ValType::F32, keyword(F32)),
        value(ValType::F64, keyword(F64))
    ))(i)
}

#[inline]
pub fn globaltype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, GlobalType, I::Error>
    where I::Inner: LexerInput<'a>
{
    alt((
        map(valtype, |valtype| GlobalType { mut_: Mut::Const, valtype }),
        map(par(preceded(keyword(Mut), valtype)), |valtype| GlobalType { mut_: Mut::Var, valtype })
    ))(i)
}

#[inline]
pub fn limits<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Limits, I::Error>
    where I::Inner: LexerInput<'a>
{
    map(
        pair(uxx::<u32, I>, opt(uxx::<u32, I>)),
        |(min, max)| Limits { min, max },
    )(i)
}

#[inline]
pub fn tabletype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, TableType, I::Error>
    where I::Inner: LexerInput<'a>
{
    map(
        terminated(limits::<I>, keyword(Funcref)),
        |limits| TableType { limits, elemtype: FuncRef {} },
    )(i)
}

#[inline]
pub fn memtype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, MemType, I::Error>
    where I::Inner: LexerInput<'a>
{
    map(limits::<I>, |limits| MemType { limits })(i)
}

#[inline]
pub fn functype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, FuncType<'a, I::Inner>, I::Error>
    where I::Inner: LexerInput<'a>
{
    map(par(preceded(keyword(Func), tuple((params::<I>, results::<I>)))), |(parameters, results)| {
        FuncType::<'a, I::Inner> { parameters, results }
    })(i)
}

#[inline]
pub fn params<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Vec<(Option<&'a I::Inner>, ValType)>, I::Error>
    where I::Inner: LexerInput<'a>
{
    fold_many0(
        par(preceded(keyword(Param), alt((
            map(pair(id, valtype), |(id, valtype)| vec![(Some(id), valtype)]),
            map(many0(valtype), |v| v.into_iter().map(|valtype| (None, valtype)).collect()),
        )))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn results<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Vec<ValType>, I::Error>
    where I::Inner: LexerInput<'a>
{
    fold_many0(
        par(preceded(keyword(Result), many0(valtype))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn resulttype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, ValType, I::Error>
    where I::Inner: LexerInput<'a>
{
    par(preceded(keyword(Result), valtype))(i)
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
