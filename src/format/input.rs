#![macro_use]

use nom::{InputTake, InputLength, Slice, InputIter, IResult};
use nom::error::{ParseError as NomParseError, ErrorKind as NomErrorKind, VerboseError as NomVerboseError};
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::iter::Enumerate;

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Stream<'a, I> {
    pub tok: &'a [I],
    pub start: usize,
    pub end: usize,
}

impl<'a, I: 'a> Stream<'a, I> {
    pub fn new(vec: &'a Vec<I>) -> Self {
        Stream {
            tok: vec.as_slice(),
            start: 0,
            end: vec.len(),
        }
    }
}


impl<'a, I: 'a> InputLength for Stream<'a, I> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a, I: 'a> InputTake for Stream<'a, I> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Stream {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Stream {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Stream {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a, I: 'a> Slice<Range<usize>> for Stream<'a, I> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Stream {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a, I: 'a> Slice<RangeTo<usize>> for Stream<'a, I> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a, I: 'a> Slice<RangeFrom<usize>> for Stream<'a, I> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a, I: 'a> Slice<RangeFull> for Stream<'a, I> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Stream {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a, I: 'a> InputIter for Stream<'a, I> {
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

/// ParseError is an augmentation of nom's similarly named type that
/// can accept any kind of error. This is used to add custom errors for
/// our formats.
pub trait ParseError<I, E>: NomParseError<I> {
    fn from_error_kind(input: I, kind: E) -> Self;
    fn append(input: I, kind: E, other: Self) -> Self;
}

/// Implement ParseError for nom's VerboseError type using nom's
/// default implementation.
impl<I> ParseError<I, NomErrorKind> for NomVerboseError<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        <Self as NomParseError<I>>::from_error_kind(input, kind)
    }

    fn append(input: I, kind: NomErrorKind, other: Self) -> Self {
        <Self as NomParseError<I>>::append(input, kind, other)
    }
}


/// Implement ParseError for nom's (I, ErrorKind) type using nom's
/// default implementation.
impl<I> ParseError<I, NomErrorKind> for (I, NomErrorKind) {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        <Self as NomParseError<I>>::from_error_kind(input, kind)
    }

    fn append(input: I, kind: NomErrorKind, other: Self) -> Self {
        <Self as NomParseError<I>>::append(input, kind, other)
    }
}

/// WithParseError is used to associate a ParseError with a type.
pub trait WithParseError<E>: Sized {
    type Error: ParseError<Self, E>;
}

pub trait WithWrappedStream {
    type Inner;
}

impl<'a, I> From<&'a [I]> for Stream<'a, I> {
    fn from(i: &'a [I]) -> Self {
        Stream { tok: i, start: 0, end: i.len() }
    }
}
