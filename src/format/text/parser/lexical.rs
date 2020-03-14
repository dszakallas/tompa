#![macro_use]



use std::ops::{RangeFrom};





use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};

use nom::error::{ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};

use crate::format::text::parser::AsStr;

pub trait AsChar {
    /// Is a valid character in an identifier?
    #[inline]
    fn is_idchar(self) -> bool;

    #[inline]
    fn is_idcontrol(self) -> bool;

    /// Is a valid standalone character in a string?
    #[inline]
    fn is_strchar(self) -> bool;

    #[inline]
    fn as_hex_digit(self) -> u8;

    #[inline]
    fn as_dec_digit(self) -> u8;
}

impl AsChar for char {
    fn is_idchar(self) -> bool {
        self.is_alphanumeric() || self.is_idcontrol()
    }

    fn is_idcontrol(self) -> bool {
        self.is_ascii()
            && match self as u8 {
            b'!'
            | b'#'..=b'\''
            | b'*'..=b'/'
            | b':'
            | b'<'..=b'@'
            | b'\\'
            | b'^'..=b'`'
            | b'|'
            | b'~' => true,
            _ => false,
        }
    }

    fn is_strchar(self) -> bool {
        let ch = self as u32;
        ch >= 0x20 && ch != 0x22 && ch != 0x5C && ch != 0x7F
    }

    fn as_hex_digit(self) -> u8 {
        match self {
            'a'..='f' => 10u8 + self as u8 - 'a' as u8,
            'A'..='F' => 10u8 + self as u8 - 'A' as u8,
            '0'..='9' => self as u8 - '0' as u8,
            _ => panic!("Not a hexadecimal digit")
        }
    }

    fn as_dec_digit(self) -> u8 {
        match self {
            '0'..='9' => self as u8 - '0' as u8,
            _ => panic!("Not a decimal digit")
        }
    }
}

impl<'a> AsChar for &'a char {
    fn is_idchar(self) -> bool {
        self.is_alphanumeric() || self.is_idcontrol()
    }

    fn is_idcontrol(self) -> bool {
        (*self).is_idcontrol()
    }

    fn is_strchar(self) -> bool {
        (*self).is_strchar()
    }

    fn as_hex_digit(self) -> u8 {
        (*self).as_hex_digit()
    }

    fn as_dec_digit(self) -> u8 {
        (*self).as_dec_digit()
    }
}

#[derive(Clone, PartialEq, Debug)]
struct LineComment<'a>(&'a str);

#[derive(Copy, Clone, PartialEq, Debug)]
struct BlockComment;

#[inline]
fn linecomment<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, LineComment<'a>, E>
    where
        I: Clone
        + InputTake
        + InputLength
        + InputIter
        + Compare<&'static str>
        + PartialEq
        + Slice<RangeFrom<usize>>
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar {
    map(preceded(tag(";;"), not_line_ending), |tag: I| LineComment(tag.as_str()))(i)
}

#[inline]
fn blockcomment<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, BlockComment, E>
    where
        I: Clone
        + InputTake
        + InputLength
        + InputIter
        + Compare<&'static str>
        + Slice<RangeFrom<usize>>,
        <I as InputIter>::Item: NomAsChar, {
    let (mut mut_i, _) = tag("(;")(i)?;

    let mut lvl = 1u32;

    while lvl > 0 {
        let (first_i, first_c) = anychar(mut_i.clone())?;
        match first_c {
            ';' => {
                if let Ok((out_i, _)) = char::<I, E>(')')(first_i.clone()) {
                    lvl -= 1;
                    mut_i = out_i;
                } else {
                    mut_i = first_i;
                }
            }
            '(' => {
                if let Ok((in_i, _)) = char::<I, E>(';')(first_i.clone()) {
                    lvl += 1;
                    mut_i = in_i;
                } else {
                    mut_i = first_i;
                }
            }
            _ => {
                mut_i = first_i;
            }
        }
    }
    Ok((mut_i, BlockComment))
}

#[inline]
pub fn ws<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, (), E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + InputIter
        + InputLength
        + InputTake
        + AsStr<'a>
        + Compare<&'static str>,
        <I as InputIter>::Item: NomAsChar {
    alt((
        value((), char('\t')),
        value((), char('\r')),
        value((), char('\n')),
        value((), char(' ')),
        value((), linecomment),
        value((), blockcomment)
    ))(i)
}

#[inline]
pub fn token<'a, I: 'a, E: ParseError<I> + 'a, F: 'a, O: 'a>(parser: F) -> impl Fn(I) -> IResult<I, O, E> + 'a
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + InputIter
        + InputLength
        + InputTakeAtPosition
        + InputTake
        + AsStr<'a>
        + Compare<&'static str>,
        <I as InputIter>::Item: NomAsChar + AsChar,
        <I as InputTakeAtPosition>::Item: AsChar,
        F: Fn(I) -> IResult<I, O, E>, {
    terminated(parser, terminated(peek(not(idchar)), alt((
        value((), many0(ws)),
        value((), peek(alt((char('('), char(')')))))
    ))))
}

#[inline]
pub fn idchar<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, &'a str, E>
    where
        I: InputIter + InputTakeAtPosition + AsStr<'a>,
        <I as InputIter>::Item: AsChar,
        <I as InputTakeAtPosition>::Item: AsChar,
{
    let (i, res) = take_while1(AsChar::is_idchar)(i)?;
    Ok((i, res.as_str()))
}

macro_rules! block {
    ($parser:expr) => {
        delimited(
              terminated(char('('), many0(ws)),
              $parser,
              terminated(char(')'), many0(ws))
        )
    };
    ($parser:expr, $($parsers:expr),*) => {
        delimited(
              terminated(char('('), many0(ws)),
              tuple(($parser, $($parsers),*)),
              terminated(char(')'), many0(ws))
        )
    };
}

#[cfg(test)]
mod test {
    
    use nom::error::{ErrorKind};
    

    use super::*;

    type FastError<T> = (T, ErrorKind);

    #[test]
    fn test_idchar() {
        assert_eq!(
            idchar::<&str, FastError<&str>>("de432#@ b"),
            Ok((" b", "de432#@"))
        );
    }

    #[test]
    fn test_linecomment() {
        assert_eq!(
            linecomment::<&str, FastError<&str>>(";;comment"),
            Ok(("", LineComment::<'static>("comment")))
        );
        assert_eq!(
            linecomment::<&str, FastError<&str>>(";;comment\nnot comment"),
            Ok(("\nnot comment", LineComment::<'static>("comment")))
        );
    }

    #[test]
    fn test_blockcomment() {
        assert_eq!(
            blockcomment::<&str, FastError<&str>>("(;;)"),
            Ok(("", BlockComment))
        );
        assert_eq!(
            blockcomment::<&str, FastError<&str>>("(;(;;);)"),
            Ok(("", BlockComment))
        );
        assert_eq!(
            blockcomment::<&str, FastError<&str>>("(;;((;a;);)"),
            Ok(("", BlockComment))
        );
        assert_eq!(
            blockcomment::<&str, FastError<&str>>("(;;((;a;););)"),
            Ok((";)", BlockComment))
        );
        blockcomment::<&str, FastError<&str>>("(;").unwrap_err();
    }

    #[test]
    fn test_token() {
        assert_eq!(
            token(tag::<'static, &str, &str, FastError<&str>>("func"))("func("),
            Ok(("(", "func"))
        );
        assert_eq!(
            token(tag::<'static, &str, &str, FastError<&str>>("mem"))("mem;;comment\n)"),
            Ok((")", "mem"))
        );
    }
}
