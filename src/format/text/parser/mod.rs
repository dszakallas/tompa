#![macro_use]

use im_rc;

use crate::format::input::{WithParseError, WithWrappedInput, satisfies};
use crate::format::text::keywords::Keyword;
use crate::format::text::lexer::{LexerInput, Num, Token};
use crate::ast::FuncType;
use nom::sequence::delimited;
use nom::{IResult, InputIter, InputLength, Slice};
use core::fmt;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::RangeFrom;

use super::lexer::AsStr;


#[derive(Debug)]
pub struct ParserError;

impl Error for ParserError {}

impl<'a> Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("lexer error"))
    }
}

/// The text format allows the use of symbolic identifiers instead of indices.
/// To resolve these identifiers, some grammar productions
/// are indexed by an identifier context I as a synthesized attribute that
/// records the declared identifiers in each index space. In addition, the
/// context records the types defined in the module, so that parameter indices
/// can be computed for functions.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct IdCtx {
    pub types: im_rc::Vector<Option<String>>,
    pub funcs: im_rc::Vector<Option<String>>,
    pub tables: im_rc::Vector<Option<String>>,
    pub mems: im_rc::Vector<Option<String>>,
    pub globals: im_rc::Vector<Option<String>>,
    pub locals: im_rc::Vector<Option<String>>,
    pub labels: im_rc::Vector<Option<String>>,
    pub typedefs: im_rc::Vector<FuncType>,
}

/// The main abstraction for parsable inputs.
pub trait ParserInput<'a>:
    Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + WithWrappedInput
    + WithParseError
    + InputIter<Item = &'a Token<<Self as WithWrappedInput>::Inner>>
    + InputLength
where
    <Self as WithWrappedInput>::Inner: LexerInput<'a> + 'a,
{
    type InputIterItem;
}

impl<'a, I: 'a> ParserInput<'a> for I
where
    I: Clone
        + PartialEq
        + WithWrappedInput
        + WithParseError
        + Slice<RangeFrom<usize>>
        + InputIter<Item = &'a Token<<Self as WithWrappedInput>::Inner>>
        + InputLength,
    <Self as WithWrappedInput>::Inner: LexerInput<'a> + 'a,
{
    type InputIterItem = <Self as InputIter>::Item;
}

#[inline]
pub fn keyword<'a, I: ParserInput<'a> + 'a>(
    keyword: Keyword,
) -> impl Fn(I) -> IResult<I, &'a I::Inner, I::Error> + 'a
where
    I::Inner: LexerInput<'a>,
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Keyword(i, kw) if *kw == keyword => Ok(i),
        _ => Err(ParserError),
    })
}

#[inline]
pub fn anykeyword<'a, I: ParserInput<'a> + 'a>(
    i: I,
) -> IResult<I, (&'a I::Inner, &'a Keyword), I::Error>
where
    I::Inner: LexerInput<'a>,
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Keyword(i, kw) => Ok((i, kw)),
        _ => Err(ParserError),
    })(i)
}

#[inline]
pub fn num<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, &'a Num<I::Inner>, I::Error>
where
    I::Inner: LexerInput<'a>,
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Num(_, num) => Ok(num),
        _ => Err(ParserError),
    })(i)
}

#[inline]
pub fn id<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, &'a str, I::Error>
where
    I::Inner: LexerInput<'a>,
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Id(i) => Ok(i.as_str()),
        _ => Err(ParserError),
    })(i)
}

#[inline]
pub fn par<'b, 'a: 'b, I: ParserInput<'a> + 'a, O: 'a, F>(
    parser: F,
) -> impl Fn(I) -> IResult<I, O, I::Error> + 'b + 'a
where
    I::Inner: LexerInput<'a>,
    F: Fn(I) -> IResult<I, O, I::Error> + 'b + 'a,
{
    delimited(
        satisfies(move |tok: &'a Token<_>| {
            if let Token::LPar = tok {
                Ok(())
            } else {
                Err(ParserError)
            }
        }),
        parser,
        satisfies(move |tok: &'a Token<_>| {
            if let Token::RPar = tok {
                Ok(())
            } else {
                Err(ParserError)
            }
        }),
    )
}

// FIXME clean up the lifetimes and delete this duplicate
#[inline]
pub fn parc<'a, I: ParserInput<'a> + 'a, O, F>(parser: F, i: I) -> IResult<I, O, I::Error>
where
    I::Inner: LexerInput<'a>,
    F: Fn(I) -> IResult<I, O, I::Error>,
{
    delimited(
        satisfies(move |tok: &'a Token<_>| {
            if let Token::LPar = tok {
                Ok(())
            } else {
                Err(ParserError)
            }
        }),
        parser,
        satisfies(move |tok: &'a Token<_>| {
            if let Token::RPar = tok {
                Ok(())
            } else {
                Err(ParserError)
            }
        }),
    )(i)
}

#[macro_use]
#[cfg(test)]
mod test {
    
    use nom::error::ErrorKind;

    use crate::format::input::{Input, WithParseError};
    use crate::format::text::lexer::Token;

    use crate::format::text::parser::WithWrappedInput;

    impl<'a> WithParseError for Input<'a, Token<&'a str>> {
        type Error = (Input<'a, Token<&'a str>>, ErrorKind);
    }

    impl<'a> WithWrappedInput for Input<'a, Token<&str>> {
        type Inner = &'a str;
    }

    macro_rules! lex {
	    ($str: expr) => { 
            nom::multi::many0($crate::format::text::lexer::token)($str).map(|(_, r)| r)
	    };
    }
}

mod instructions;
mod lexical;
mod types;
mod values;
