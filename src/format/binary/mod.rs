
use std::{error::Error, fmt::{self, Display, Formatter}, ops::{Range, RangeFrom, RangeTo}};

use nom::{AsBytes, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};
use nom::error::ErrorKind;

use super::input::WithParseError;

#[derive(Debug)]
pub struct BinaryError;

impl Error for BinaryError {}

impl<'a> Display for BinaryError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("binary error"))
    }
}

/// Blanket trait for traits required by the lexer
pub trait ByteStream<'a>: Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + InputIter
    + InputLength
    + InputTakeAtPosition
    + InputTake
    + Offset
    + WithParseError<ErrorKind>
    + AsBytes
    + Compare<&'a[u8]>
    + fmt::Debug
{
    type InputIterItem;
    type InputTakeAtPositionItem;
}

mod values;
mod types;
mod instructions;
mod modules;
