#![macro_use]

use im_rc;

use crate::format::input::{ParseError, WithParseError, WithWrappedStream};
use crate::format::text::keywords::Keyword;
use crate::format::text::lexer::{CharStream, Num, Token};
use crate::ast::FuncType;
use nom::sequence::delimited;
use nom::{IResult, InputIter, InputLength, Slice};
use core::fmt;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::RangeFrom;
use nom::error::ErrorKind;

use super::lexer::AsStr;

fn wrap_lexer_error<I, II, E>(input: I, tpe: ParserType, e: nom::Err<E>) -> <I as WithParseError<Parser<E>>>::Error
    where I: WithWrappedStream<Inner = II> + WithParseError<Parser<E>>,
          II: WithParseError<ErrorKind, Error=E>,
          E: ParseError<II, ErrorKind> {
    todo!()
}

macro_rules! token_type {

    ($i: expr, $parser: ident, $token_pat: pat, $ret: expr) => {
        token_type!($i, $parser, $token_pat if true, $ret)
    };

    ($i: expr, $parser: ident, $token_pat: pat if $cond: expr, $ret: expr) => {
        match $i.iter_elements().next() {
            None => Err(nom::Err::Error(ParseError::from_error_kind($i, Parser { tpe: ParserType::$parser, lexer_error: None }))),
            Some(t) => {
                match t {
                    $token_pat if $cond => Ok(($i.slice(1..), $ret)),
                    _ => Err(nom::Err::Error(ParseError::from_error_kind($i, Parser { tpe: ParserType::$parser, lexer_error: None })))
                }
            }
        }
    };

    ($i: expr, $parser: ident, $token_pat: pat, $pat_ident: ident, $map: expr) => {
        match $i.iter_elements().next() {
            None => Err(nom::Err::Error(ParseError::from_error_kind($i, Parser { tpe: ParserType::$parser, lexer_error: None }))),
            Some(t) => {
                if let $token_pat = t {
                    match $map($pat_ident.clone()) {
                        Ok((_, parsed)) => Ok(($i.slice(1..), parsed)),
                        Err(e) => {
                            Err(nom::Err::Failure(crate::format::text::parser::wrap_lexer_error::<I, I::Inner, <I::Inner as WithParseError<ErrorKind>>::Error>(
                                $i,
                                ParserType::$parser,
                                e
                            )))
                        }
                    }
                } else {
                    Err(nom::Err::Error(ParseError::from_error_kind($i, Parser { tpe: ParserType::$parser, lexer_error: None })))
                }
            }
        }
    };
}

#[derive(Debug)]
pub struct ParserError;

impl Error for ParserError {}

impl<'a> Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("lexer error"))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parser<E: Sized> {
    tpe: ParserType,
    lexer_error: Option<E>
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserType {
    String,
    Uxx,
    Ixx,
    Fxx,
    LPar,
    RPar,
    Id,
    Keyword
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

/// Stream of lexed textual tokens.
pub trait TokenStream<'a>:
    Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + WithWrappedStream
    + WithParseError<Parser<<<Self as WithWrappedStream>::Inner as WithParseError<ErrorKind>>::Error>>
    + InputIter<Item = &'a Token<<Self as WithWrappedStream>::Inner>>
    + InputLength
where
    <Self as WithWrappedStream>::Inner: CharStream<'a> + 'a,
{
    type InputIterItem;
}

impl<'a, I: 'a> TokenStream<'a> for I
where
    I: Clone
        + PartialEq
        + WithWrappedStream
        + WithParseError<Parser<<<Self as WithWrappedStream>::Inner as WithParseError<ErrorKind>>::Error>>
        + Slice<RangeFrom<usize>>
        + InputIter<Item = &'a Token<<Self as WithWrappedStream>::Inner>>
        + InputLength,
    <Self as WithWrappedStream>::Inner: CharStream<'a> + 'a,
{
    type InputIterItem = <Self as InputIter>::Item;
}

#[inline]
pub fn tag<'a, I: TokenStream<'a> + 'a>(keyword: Keyword) -> impl Fn(I) -> IResult<I, &'a I::Inner, I::Error> + 'a
where I::Inner: CharStream<'a> {
    move |i| token_type!(i, Keyword, Token::Keyword(o, kw) if keyword == *kw, o)
}

#[inline]
pub fn anykeyword<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, (&'a I::Inner, &'a Keyword), I::Error>
where I::Inner: CharStream<'a> {
    token_type!(i, Keyword, Token::Keyword(o, kw), (o, kw))
}

#[inline]
pub fn id<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, &'a str, I::Error> where I::Inner: CharStream<'a> {
    token_type!(i, Id, Token::Id(o), o.as_str())
}

#[inline]
pub fn par<'b, 'a: 'b, I: TokenStream<'a> + 'a, O: 'a, F>(
    parser: F,
) -> impl Fn(I) -> IResult<I, O, I::Error> + 'b + 'a
where
    I::Inner: CharStream<'a>,
    F: Fn(I) -> IResult<I, O, I::Error> + 'b + 'a,
{
    delimited(
        |i | token_type!(i, LPar, Token::LPar, ()),
        parser,
        |i | token_type!(i, RPar, Token::RPar, ()),
    )
}

// FIXME clean up the lifetimes and delete this duplicate
#[inline]
pub fn parc<'a, I: TokenStream<'a> + 'a, O, F>(parser: F, i: I) -> IResult<I, O, I::Error>
where
    I::Inner: CharStream<'a>,
    F: Fn(I) -> IResult<I, O, I::Error>,
{
    delimited(
        |i | token_type!(i, LPar, Token::LPar, ()),
        parser,
        |i | token_type!(i, RPar, Token::RPar, ()),
    )(i)
}

#[macro_use]
#[cfg(test)]
mod test {
    
    use nom::error::{ErrorKind, VerboseError as NomVerboseError};

    use crate::format::input::{Stream, WithParseError};
    use crate::format::text::error::{VerboseError};
    use crate::format::text::lexer::Token;

    use crate::format::text::parser::{Parser, WithWrappedStream};

    impl<'a> WithParseError<Parser<NomVerboseError<&'a str>>> for Stream<'a, Token<&'a str>> {
        type Error = VerboseError<Stream<'a, Token<&'a str>>, NomVerboseError<&'a str>>;
    }

    impl<'a> WithWrappedStream for Stream<'a, Token<&str>> {
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
