use nom::{IResult};
use nom::branch::alt;

use nom::combinator::{map, opt, value};

use nom::multi::{many0, fold_many0};
use nom::sequence::{pair, preceded, terminated, tuple};

use crate::ast::{FuncRef, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType};

use crate::format::text::parser::values::uxx;
use crate::format::text::parser::{keyword, par, ParserInput, id};
use crate::format::text::lexer::{LexerInput};
use crate::format::text::keywords::Keyword::*;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct FuncType<'a> {
    pub parameters: Vec<(Option<&'a str>, ValType)>,
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
pub fn functype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, FuncType<'a>, I::Error>
    where I::Inner: LexerInput<'a>
{
    map(par(preceded(keyword(Func), tuple((params::<I>, results::<I>)))), |(parameters, results)| {
        FuncType::<'a> { parameters, results }
    })(i)
}

#[inline]
pub fn params<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Vec<(Option<&'a str>, ValType)>, I::Error>
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
pub fn resulttype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, ResultType, I::Error>
    where I::Inner: LexerInput<'a>
{
    let (i, vt) = opt(par(preceded(keyword(Result), valtype)))(i)?;
    Ok((i, ResultType { valtype: vt }))
}


#[cfg(test)]
mod test {

    use crate::format::input::Input;

    use super::*;

    #[test]
    fn test_valtype() {

        let t = lex!("i32").unwrap();

        assert_eq!(consumed!(valtype, Input::new(&t)), Ok(ValType::I32));
    }

    #[test]
    fn test_globaltype() {
        {
            let t = lex!("f64").unwrap();
            assert_eq!(consumed!(globaltype, Input::new(&t)), Ok(GlobalType { mut_: Mut::Const, valtype: ValType::F64 }));
        }
        {
            let t = lex!("(mut i64)").unwrap();
            assert_eq!(consumed!(globaltype, Input::new(&t)), Ok(GlobalType { mut_: Mut::Var, valtype: ValType::I64 }));
        }
    }

    #[test]
    fn test_tabletype() {
        let t = lex!("13 0x001F funcref").unwrap();

        assert_eq!(consumed!(tabletype, Input::new(&t)), Ok(TableType { limits: Limits { min: 13u32, max: Some(31u32) }, elemtype: FuncRef {} }));
    }


    #[test]
    fn test_functype() {
        let t = lex!("(func (param $abcd i32) (result f64))").unwrap();

        assert_eq!(consumed!(functype, Input::new(&t)),
            Ok(FuncType { parameters: vec![(Some("$abcd"), ValType::I32)], results: vec![ValType::F64] })
        );
    }

    #[test]
    fn test_resulttype() {
        let t = lex!("(result f32)").unwrap();

        assert_eq!(consumed!(resulttype, Input::new(&t)), Ok(ResultType { valtype: Some(ValType::F32) }));
    }
}
