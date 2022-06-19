use nom::{IResult};
use nom::branch::alt;

use nom::combinator::{map, opt, value};

use nom::multi::{many0, fold_many0};
use nom::sequence::{pair, preceded, terminated, tuple};

use crate::ast::{FuncRef, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType};

use crate::format::text::parser::values::uxx;
use crate::format::text::parser::{tag, par, TokenStream, id};
use crate::format::text::lexer::{CharStream};
use crate::format::text::keywords::Keyword::*;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct FuncType<'a> {
    pub parameters: Vec<(Option<&'a str>, ValType)>,
    pub results: Vec<ValType>,
}

#[inline]
pub fn valtype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, ValType, I::Error>
    where I::Inner: CharStream<'a>
{
    alt((
        value(ValType::I32, tag(I32)),
        value(ValType::I64, tag(I64)),
        value(ValType::F32, tag(F32)),
        value(ValType::F64, tag(F64))
    ))(i)
}

#[inline]
pub fn globaltype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, GlobalType, I::Error>
    where I::Inner: CharStream<'a>
{
    alt((
        map(valtype, |valtype| GlobalType { mut_: Mut::Const, valtype }),
        map(par(preceded(tag(Mut), valtype)), |valtype| GlobalType { mut_: Mut::Var, valtype })
    ))(i)
}

#[inline]
pub fn limits<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Limits, I::Error>
    where I::Inner: CharStream<'a>
{
    map(
        pair(uxx::<u32, I>, opt(uxx::<u32, I>)),
        |(min, max)| Limits { min, max },
    )(i)
}

#[inline]
pub fn tabletype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, TableType, I::Error>
    where I::Inner: CharStream<'a>
{
    map(
        terminated(limits::<I>, tag(Funcref)),
        |limits| TableType { limits, elemtype: FuncRef {} },
    )(i)
}

#[inline]
pub fn memtype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, MemType, I::Error>
    where I::Inner: CharStream<'a>
{
    map(limits::<I>, |limits| MemType { limits })(i)
}

#[inline]
pub fn functype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, FuncType<'a>, I::Error>
    where I::Inner: CharStream<'a>
{
    map(par(preceded(tag(Func), tuple((params::<I>, results::<I>)))), |(parameters, results)| {
        FuncType::<'a> { parameters, results }
    })(i)
}

#[inline]
pub fn params<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Vec<(Option<&'a str>, ValType)>, I::Error>
    where I::Inner: CharStream<'a>
{
    fold_many0(
        par(preceded(tag(Param), alt((
            map(pair(id, valtype), |(id, valtype)| vec![(Some(id), valtype)]),
            map(many0(valtype), |v| v.into_iter().map(|valtype| (None, valtype)).collect()),
        )))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn results<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Vec<ValType>, I::Error>
    where I::Inner: CharStream<'a>
{
    fold_many0(
        par(preceded(tag(Result), many0(valtype))),
        Vec::new(),
        |mut v, mut i| { v.append(&mut i); v }
    )(i)
}

#[inline]
pub fn resulttype<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, ResultType, I::Error>
    where I::Inner: CharStream<'a>
{
    let (i, vt) = opt(par(preceded(tag(Result), valtype)))(i)?;
    Ok((i, ResultType { valtype: vt }))
}


#[cfg(test)]
mod test {

    use crate::format::input::Stream;

    use super::*;

    #[test]
    fn test_valtype() {

        let t = lex!("i32").unwrap();

        assert_eq!(consumed!(valtype, Stream::new(&t)), Ok(ValType::I32));
    }

    #[test]
    fn test_globaltype() {
        {
            let t = lex!("f64").unwrap();
            assert_eq!(consumed!(globaltype, Stream::new(&t)), Ok(GlobalType { mut_: Mut::Const, valtype: ValType::F64 }));
        }
        {
            let t = lex!("(mut i64)").unwrap();
            assert_eq!(consumed!(globaltype, Stream::new(&t)), Ok(GlobalType { mut_: Mut::Var, valtype: ValType::I64 }));
        }
    }

    #[test]
    fn test_tabletype() {
        let t = lex!("13 0x001F funcref").unwrap();

        assert_eq!(consumed!(tabletype, Stream::new(&t)), Ok(TableType { limits: Limits { min: 13u32, max: Some(31u32) }, elemtype: FuncRef {} }));
    }


    #[test]
    fn test_functype() {
        let t = lex!("(func (param $abcd i32) (result f64))").unwrap();

        assert_eq!(consumed!(functype, Stream::new(&t)),
                   Ok(FuncType { parameters: vec![(Some("$abcd"), ValType::I32)], results: vec![ValType::F64] })
        );
    }

    #[test]
    fn test_resulttype() {
        let t = lex!("(result f32)").unwrap();

        assert_eq!(consumed!(resulttype, Stream::new(&t)), Ok(ResultType { valtype: Some(ValType::F32) }));
    }
}
