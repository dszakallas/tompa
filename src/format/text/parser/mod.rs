use im_rc;

use crate::syntax::{FuncRef, FuncType, GlobalType, Limits, Memarg, MemType, Mut, TableType, ValType};
use nom::error::ParseError;
use std::ops::RangeFrom;
use nom::{Slice, InputIter, InputLength, IResult};
use std::option::NoneError;
use crate::format::text::lexer::{Token, Num, LexerInput};
use crate::format::text::lexer::keyword::Keyword;
use nom::sequence::delimited;
use crate::format::input::{satisfies, WithParseError};
use std::collections::HashMap;

mod lexical;
mod values;
mod types;
mod instructions;

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

pub trait WithWrappedInput {
    type Inner;
}

pub trait ParserInput<'a>: Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + WithWrappedInput
    + WithParseError
    + InputIter<Item=&'a Token<<Self as WithWrappedInput>::Inner>>
    + InputLength
    where
        <Self as WithWrappedInput>::Inner: LexerInput<'a> + 'a
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
        + InputIter<Item=&'a Token<<Self as WithWrappedInput>::Inner>>
        + InputLength,
        <Self as WithWrappedInput>::Inner: LexerInput<'a> + 'a,
{
    type InputIterItem = <Self as InputIter>::Item;
}

#[inline]
pub fn keyword<'a, I: ParserInput<'a> + 'a>(keyword: Keyword) -> impl Fn(I) -> IResult<I, &'a I::Inner, I::Error> + 'a
    where I::Inner: LexerInput<'a>
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Keyword(i, kw) if *kw == keyword => Ok(i),
        _ => Err(NoneError)
    })
}

#[inline]
pub fn anykeyword<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, (&'a I::Inner, &'a Keyword), I::Error>
    where I::Inner: LexerInput<'a>
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Keyword(i, kw) => Ok((i, kw)),
        _ => Err(NoneError)
    })(i)
}

#[inline]
pub fn num<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, &'a Num<I::Inner>, I::Error>
    where I::Inner: LexerInput<'a>
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Num(_, num) => Ok(num),
        _ => Err(NoneError)
    })(i)
}

#[inline]
pub fn id<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, &'a I::Inner, I::Error>
    where I::Inner: LexerInput<'a>
{
    satisfies(move |tok: &'a Token<I::Inner>| match tok {
        Token::Id(i) => Ok(i),
        _ => Err(NoneError)
    })(i)
}

#[inline]
pub fn par<'b, 'a: 'b, I: ParserInput<'a> +'a, O: 'a, F>(parser: F) -> impl Fn(I) -> IResult<I, O, I::Error> + 'b + 'a
    where
        I::Inner: LexerInput<'a>,
        F: Fn(I) -> IResult<I, O, I::Error> + 'b + 'a
{
    delimited(
        satisfies(move |tok: &'a Token<_>| if let Token::LPar(_) = tok { Ok(()) } else { Err(NoneError) }),
        parser,
        satisfies(move |tok: &'a Token<_>| if let Token::RPar(_) = tok { Ok(()) } else { Err(NoneError) }),
    )
}

// FIXME clean up the lifetimes and delete this duplicate
#[inline]
pub fn parc<'a, I: ParserInput<'a> +'a, O, F>(parser: F, i: I) -> IResult<I, O, I::Error>
    where
        I::Inner: LexerInput<'a>,
        F: Fn(I) -> IResult<I, O, I::Error>
{
    delimited(
        satisfies(move |tok: &'a Token<_>| if let Token::LPar(_) = tok { Ok(()) } else { Err(NoneError) }),
        parser,
        satisfies(move |tok: &'a Token<_>| if let Token::RPar(_) = tok { Ok(()) } else { Err(NoneError) }),
    )(i)
}
