use im_rc;

use crate::syntax::types::{FuncRef, FuncType, GlobalType, Limits, MemArg, MemType, Mut, TableType, ValType};
use nom::error::{ParseError, ErrorKind};
use std::ops::{RangeFrom, Try};
use nom::{Slice, InputIter, InputLength, IResult};
use std::option::NoneError;
use crate::format::text::lexer::{Token, Num, LexerInput};
use crate::format::text::lexer::keyword::Keyword;
use nom::sequence::delimited;
use crate::format::input::satisfies;

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

pub trait ParserInput<'a, I: 'a>: Clone
+ PartialEq
+ Slice<RangeFrom<usize>>
+ InputIter<Item=&'a Token<I>>
+ InputLength {
    type LexerInput;
    type InputIterItem;
    //type InputTakeAtPositionItem;
}

impl<'a, I1, I2: 'a> ParserInput<'a, I2> for I1
    where
        I1: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter<Item=&'a Token<I2>>
        + InputLength,
        I2: LexerInput<'a>,
{
    type LexerInput = I2;
    type InputIterItem = <Self as InputIter>::Item;
    //type InputTakeAtPositionItem = <Self as InputTakeAtPosition>::Item;
}

#[inline]
pub fn keyword<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(keyword: Keyword) -> impl Fn(I1) -> IResult<I1, (), E> + 'a
    where
        I1: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter<Item=&'a Token<I2>>
        + InputLength
{
    satisfies(move |tok: &'a Token<_>| match tok {
        Token::Keyword(_, kw) if *kw == keyword => Ok(()),
        _ => Err(NoneError)
    })
}

#[inline]
pub fn num<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(i: I1) -> IResult<I1, &'a Num<I2>, E>
    where
        I1: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter<Item=&'a Token<I2>>
        + InputLength
{
    satisfies(move |tok: &'a Token<I2>| match tok {
        Token::Num(_, num) => Ok((num)),
        _ => Err(NoneError)
    })(i)
}

#[inline]
pub fn block<'a, I1: 'a, E: ParseError<I1> + 'a, I2: 'a, O: 'a, F: 'a>(parser: F) -> impl Fn(I1) -> IResult<I1, O, E> + 'a
    where
        I1: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter<Item=&'a Token<I2>>
        + InputLength,
        F: Fn(I1) -> IResult<I1, O, E>
{
    delimited(
        satisfies(move |tok: &'a Token<_>| if let Token::LPar(_) = tok { Ok(()) } else { Err(NoneError) }),
        parser,
        satisfies(move |tok: &'a Token<_>| if let Token::RPar(_) = tok { Ok(()) } else { Err(NoneError) }),
    )
}



mod lexical;
mod values;
mod types;
//mod instructions;
