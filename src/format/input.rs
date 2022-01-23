use nom::{InputTake, InputLength, Slice, InputIter, IResult};
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::iter::Enumerate;
use nom::error::{ParseError, ErrorKind};

use crate::format::text::parser::ParserError;


#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Input<'a, I> {
    pub tok: &'a [I],
    pub start: usize,
    pub end: usize,
}

impl<'a, I: 'a> Input<'a, I> {
    pub fn new(vec: &'a Vec<I>) -> Self {
        Input {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }
}


impl<'a, I: 'a> InputLength for Input<'a, I> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a, I: 'a> InputTake for Input<'a, I> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Input {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Input {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Input {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a, I: 'a> Slice<Range<usize>> for Input<'a, I> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Input {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a, I: 'a> Slice<RangeTo<usize>> for Input<'a, I> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a, I: 'a> Slice<RangeFrom<usize>> for Input<'a, I> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a, I: 'a> Slice<RangeFull> for Input<'a, I> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Input {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a, I: 'a> InputIter for Input<'a, I> {
    type Item = &'a I;
    type Iter = Enumerate<::std::slice::Iter<'a, I>>;
    type IterElem = ::std::slice::Iter<'a, I>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, I>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, I> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(|b| predicate(b.clone()))
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tok.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}


/// Helper trait for associating an Error type.
pub trait WithParseError: Sized {
    type Error: ParseError<Self>;
}

pub trait WithWrappedInput {
    type Inner;
}

type PResult<I, T> = IResult<I, T, <I as WithParseError>::Error>;

pub fn pred<I: WithParseError, P, Item>(p: P) -> impl Fn(I) -> PResult<I, Item>
    where
        I: Slice<RangeFrom<usize>> + InputIter<Item=Item>,
        P: Fn(&Item) -> bool
{
    move |i: I| match (i).iter_elements().next().map(|t| {
        let b = p(&t);
        (t, b)
    }) {
        Some((item, true)) => Ok((i.slice(1..), item)),
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char))),
    }
}

#[inline]
pub fn satisfies<I: WithParseError, F, O>(f: F) -> impl Fn(I) -> PResult<I, O>
    where
        I: Slice<RangeFrom<usize>> + InputIter,
        F: Fn(<I as InputIter>::Item) -> Result<O, ParserError>
{
    move |i: I| {
        match (i).iter_elements().next().map_or_else(|| Err(ParserError), |t| f(t)) {
            Ok(o) => Ok((i.slice(1..), o)),
            Err(_) => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::MapRes))),
        }
    }}

impl<'a, I> From<&'a [I]> for Input<'a, I> {
    fn from(i: &'a [I]) -> Self {
        Input { tok: i, start: 0, end: i.len() }
    }
}
