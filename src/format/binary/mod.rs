
use std::{error::Error, fmt::{self, Display, Formatter}, ops::{Range, RangeFrom, RangeTo}};

use nom::{AsBytes, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};

use super::{input::{WithParseError, WithWrappedInput}};



#[derive(Debug)]
pub struct BinaryError;

impl Error for BinaryError {}

impl<'a> Display for BinaryError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("binary error"))
    }
}


/// Blanket trait for traits required by the lexer
pub trait BinaryInput<'a>: Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + InputIter
    + InputLength
    + InputTakeAtPosition
    + InputTake
    + Offset
    + WithParseError
    + AsBytes
    + std::fmt::Debug
{
    type InputIterItem;
    type InputTakeAtPositionItem;
}


// /// The main abstraction for parsable inputs.
// pub trait BinaryParserInput<'a>:
// Clone
//     + PartialEq
//     + Slice<RangeFrom<usize>>
//     + WithWrappedInput
//     + WithParseError
//     + InputIter<Item = &'a Token<<Self as WithWrappedInput>::Inner>>
//     + InputLength
// where
//     <Self as WithWrappedInput>::Inner: BinaryInput<'a> + 'a,
// {
//     type InputIterItem;
// }

// impl<'a, I: 'a> ParserInput<'a> for I
// where
//     I: Clone
//     + PartialEq
//     + WithWrappedInput
//     + WithParseError
//     + Slice<RangeFrom<usize>>
//     + InputIter<Item = &'a Token<<Self as WithWrappedInput>::Inner>>
//     + InputLength,
// <Self as WithWrappedInput>::Inner: BinaryInput<'a> + 'a,
// {
//     type InputIterItem = <Self as InputIter>::Item;
// }

mod values;
mod types;
