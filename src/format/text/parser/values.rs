
use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice, Offset};
use nom::error::{ParseError, ErrorKind};
use num::{Unsigned, Signed};
use crate::format::text::lexer::{AsChar, Token, NumVariant, hex_num, NumParts, dec_num, AsStr, hex_num_digits, LexerInput, dec_num_digits};
use crate::format::input::satisfies;
use crate::format::text::parser::lexical::{parsed_uxx, parsed_string, parsed_ixx, parsed_fxx};
use crate::format::text::parser::ParserInput;
use lexical_core::FromLexical;

use lexical_core::{Float as LcFloat};
use num::Float;

use num::FromPrimitive;

use crate::format::text::parser::lexical::FromSigned;
use std::iter::FromIterator;

use super::ParserError;


#[cfg(test)]
mod test {
    use crate::format::input::{Input};
    use crate::format::text::lexer::Token;

    use super::*;
    //use crate::format::text::parser::test::*;
  
    #[test]
    fn test_string() {
        string::<Input<Token<&str>>>(Input::from(&[Token::String("\"\"")][..])).unwrap();
        string::<Input<Token<&str>>>(Input::from(&[Token::String("\"absdd\"")][..])).unwrap();
    }
}

#[inline]
pub fn string<'a, I: ParserInput<'a> + 'a>(token_i: I) -> IResult<I, String, I::Error>
    where I::Inner: LexerInput<'a>
{
    let (token_i, i) = satisfies(|tok: &'a Token<I::Inner>| if let Token::String(lit) = tok {
        Ok(lit)
    } else {
        Err(ParserError)
    })(token_i)?;

    if let Ok((_i, parsed)) = parsed_string::<I::Inner>(i.clone()) {
        Ok((token_i, parsed))
    } else {
        Err(nom::Err::Error(ParseError::from_error_kind(token_i, ErrorKind::MapRes)))
    }
}

pub fn uxx<'a, Out: Unsigned, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where I::Inner: LexerInput<'a>
{
    let (i, lexer_i) = satisfies(|tok: &'a Token<I::Inner>| if let Token::Num(lit, _) = tok {
        Ok(lit)
    } else {
        Err(ParserError)
    })(i)?;

    if let Ok((_, parsed)) = parsed_uxx::<Out, I::Inner>(lexer_i.clone()) {
        Ok((i, parsed))
    } else {
        Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::MapRes)))
    }
}

#[inline]
pub fn ixx<'a, Out: Unsigned + FromSigned, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where
        I::Inner: LexerInput<'a>,
        <Out as FromSigned>::Repr: Signed + FromLexical,
{
    let (i, lexer_i) = satisfies(|tok: &'a Token<I::Inner>| if let Token::Num(lit, _) = tok {
        Ok(lit)
    } else {
        Err(ParserError)
    })(i)?;

    if let Ok((_, parsed)) = parsed_ixx::<Out, I::Inner>(lexer_i.clone()) {
        Ok((i, parsed))
    } else {
        Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::MapRes)))
    }
}

#[inline]
pub fn fxx<'a, Out, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where
        I::Inner: LexerInput<'a>,
        Out: Float + LcFloat + FromLexical,
        <Out as LcFloat>::Unsigned: FromPrimitive,
{
    let (i, preparsed) = satisfies(|tok: &'a Token<I::Inner>| if let Token::Num(_, num) = tok {
        Ok(num)
    } else {
        Err(ParserError)
    })(i)?;

    let p = parsed_fxx::<Out, I::Inner>(preparsed).map_err(|_| nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::MapRes)))?;

    Ok((i, p))
}

// #[cfg(test)]
// mod test {
//
//     use nom::error::{ErrorKind};
//
//
//     use super::*;
//
//     type FastError<T> = (T, ErrorKind);
//
//     #[test]
//     fn test_id() {
//         assert_eq!(
//             id::<&str, FastError<&str>>("$$f@0f!# a"),
//             Ok((" a", "$f@0f!#"))
//         );
//         id::<&str, FastError<&str>>("0d@0f!# a").unwrap_err();
//     }
//
//     #[test]
//     fn test_string() {
//         assert_eq!(
//             string::<FastError<&str>>("\"Lorem ipsum dolor sit amet\""),
//             Ok(("", "Lorem ipsum dolor sit amet".to_owned()))
//         );
//         string::<FastError<&str>>("\"Lorem").unwrap_err();
//         assert_eq!(
//             string::<FastError<&str>>(
//                 "\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\""
//             ),
//             Ok(("", "ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()))
//         );
//         string::<FastError<&str>>("\"\\uinvalid\"").unwrap_err();
//         string::<FastError<&str>>("\"\\q\"").unwrap_err();
//     }
//
//     #[test]
//     fn test_unsigned_int() {
//         assert_eq!(
//             uxx::<&str, FastError<&str>, u64>("0xF"),
//             Ok(("", 0xF_u64))
//         );
//         assert_eq!(
//             uxx::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEF"),
//             Ok(("", 0x0123_4567_89AB_CDEF_u64))
//         );
//         assert_eq!(
//             uxx::<&str, FastError<&str>, u32>("1234"),
//             Ok(("", 1234_u32))
//         );
//         assert_eq!(
//             uxx::<&str, FastError<&str>, u32>("000000_00_0_0000000000000000000"),
//             Ok(("", 0_u32))
//         );
//         uxx::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEFFFFFF").unwrap_err();
//     }
//
//     #[test]
//     fn test_signed_int() {
//         sxx::<&str, FastError<&str>, i8>("0xFF").unwrap_err();
//         assert_eq!(
//             sxx::<&str, FastError<&str>, i8>("0x7F"),
//             Ok(("", 127_i8))
//         );
//         assert_eq!(
//             sxx::<&str, FastError<&str>, i8>("-0x8_0"),
//             Ok(("", -128_i8))
//         );
//         assert_eq!(
//             sxx::<&str, FastError<&str>, i8>("-00000_0000_0000000000123"),
//             Ok(("", -123_i8))
//         );
//     }
//
//     #[test]
//     fn test_int() {
//         assert_eq!(ixx::<&str, FastError<&str>, u8>("0x7F"), Ok(("", 127_u8)));
//         assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x8_0"), Ok(("", 128_u8)));
//         assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x1"), Ok(("", 255_u8)));
//         assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x0"), Ok(("", 0_u8)));
//     }
//
//     #[test]
//     fn test_float() {
//         assert_eq!(float::<&str, FastError<&str>, f32>("inf"), Ok(("", f32::INFINITY)));
//         assert_eq!(float::<&str, FastError<&str>, f64>("-inf"), Ok(("", f64::NEG_INFINITY)));
//         assert_ne!(
//             float::<&str, FastError<&str>, f32>("nan"),
//             float::<&str, FastError<&str>, f32>("nan")
//         );
//         assert_eq!(float::<&str, FastError<&str>, f32>("0.1"), Ok(("", 1e-1f32)));
//         assert_eq!(float::<&str, FastError<&str>, f32>("0.000000001"), Ok(("", 1e-9f32)));
//         assert_eq!(float::<&str, FastError<&str>, f64>("-1000.000000001"), Ok(("", -1000.000000001f64)));
//
//         assert_eq!(float::<&str, FastError<&str>, f64>("0x0.0p1324125"), Ok(("", 0.0f64)));
//         assert_eq!(float::<&str, FastError<&str>, f64>("-0x0"), Ok(("", -0.0f64)));
//         // FIXME hex float parsing
//         //assert_eq!(float::<&str, FastError<&str>, f64>("0x400.0p-10"), Ok(("", 1f64)));
//     }
// }
