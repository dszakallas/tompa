use nom::IResult;
use nom::error::{ParseError, ErrorKind};
use num::{Unsigned, Signed};
use crate::format::text::lexer::{Token, LexerInput};
use crate::format::input::satisfies;
use crate::format::text::parser::lexical::{parsed_uxx, parsed_string, parsed_ixx, parsed_fxx};
use crate::format::text::parser::ParserInput;
use crate::format::values::AsUnsigned;
use lexical_core::FromLexical;

use lexical_core::{Float as LcFloat};
use num::Float;

use num::FromPrimitive;

use super::ParserError;


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
pub fn ixx<'a, Out: Unsigned + AsUnsigned, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where
        I::Inner: LexerInput<'a>,
        <Out as AsUnsigned>::Repr: Signed + FromLexical,
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

#[cfg(test)]
mod test {

    use nom::error::{ErrorKind};

    use crate::format::input::Input;
    use super::*;

    use crate::format::values::AsUnsigned;

    type FastError<T> = (T, ErrorKind);

    // #[test]
    // fn test_id() {
    //     {
    //         let t = lex!("$$f@0f!#").unwrap();
    //         assert_eq!(consumed!(id, Input::new(&t)), Ok("$f@0f!#"))
    //     }

    //     {
    //         let t = lex!("0f!#").unwrap();
    //         consumed!(id, Input::new(&t)).unwrap_err();
    //     }
    // }

    #[test]
    fn test_string() {
        {
            let t = lex!("\"Lorem ipsum dolor sit amet\"").unwrap();
            assert_eq!(consumed!(string, Input::new(&t)), Ok("Lorem ipsum dolor sit amet".to_owned()));
        }
        {
            
            let t = lex!("\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\"").unwrap();
            assert_eq!(consumed!(string, Input::new(&t)), Ok("ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()));
        }
        {
            
            let t = lex!("\"Lorem").unwrap();
            consumed!(string, Input::new(&t)).unwrap_err();
        }
        {
            let t = lex!("\"\\uinvalid\"").unwrap();
            consumed!(string, Input::new(&t)).unwrap_err();
        }
        {

            let t = lex!("\"\\q\"").unwrap();
            consumed!(string, Input::new(&t)).unwrap_err();
        }
    }

    #[test]
    fn test_unsigned_int() {
        {
            let t = lex!("0xF").unwrap();
            assert_eq!(consumed!(uxx, Input::new(&t)), Ok(0xF_u64));
        }

        {
            let t = lex!("0x012_3_456_789_ABCDEF").unwrap();
            assert_eq!(consumed!(uxx, Input::new(&t)), Ok(0x0123_4567_89AB_CDEF_u64));
        }
        {
            let t = lex!("1234").unwrap();
            assert_eq!(consumed!(uxx, Input::new(&t)), Ok(1234_u32));
        }

        {
            let t = lex!("000000_00_0_0000000000000000000").unwrap();
            assert_eq!(consumed!(uxx, Input::new(&t)), Ok(0_u32));
        }
        {
            
            let t = lex!("0x012_3_456_789_ABCDEFFFFFF").unwrap();
            consumed!(uxx::<u32, _>, Input::new(&t)).unwrap_err();
        }
    }

    #[test]
    fn test_signed_int() {
        {
            let t = lex!("0xFF").unwrap();
            consumed!(ixx::<u8, _>, Input::new(&t)).unwrap_err();
        }
        {
            let t = lex!("0x7F").unwrap();
            assert_eq!(consumed!(ixx, Input::new(&t)), Ok(127_u8));
        }
        // {
        //     let t = lex!("-0x8_0").unwrap();
        //     assert_eq!(consumed!(ixx, Input::new(&t)), Ok(255_u8));
        // }
        // {
        //     let t = lex!("-00000_0000_0000000000123").unwrap();
        //     assert_eq!(consumed!(ixx, Input::new(&t)), Ok(250_u8));
        // }
    }

    // #[test]
    // fn test_int() {
    //     assert_eq!(ixx::<&str, FastError<&str>, u8>("0x7F"), Ok(("", 127_u8)));
    //     assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x8_0"), Ok(("", 128_u8)));
    //     assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x1"), Ok(("", 255_u8)));
    //     assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x0"), Ok(("", 0_u8)));
    // }

    // #[test]
    // fn test_float() {
    //     assert_eq!(float::<&str, FastError<&str>, f32>("inf"), Ok(("", f32::INFINITY)));
    //     assert_eq!(float::<&str, FastError<&str>, f64>("-inf"), Ok(("", f64::NEG_INFINITY)));
    //     assert_ne!(
    //         float::<&str, FastError<&str>, f32>("nan"),
    //         float::<&str, FastError<&str>, f32>("nan")
    //     );
    //     assert_eq!(float::<&str, FastError<&str>, f32>("0.1"), Ok(("", 1e-1f32)));
    //     assert_eq!(float::<&str, FastError<&str>, f32>("0.000000001"), Ok(("", 1e-9f32)));
    //     assert_eq!(float::<&str, FastError<&str>, f64>("-1000.000000001"), Ok(("", -1000.000000001f64)));

    //     assert_eq!(float::<&str, FastError<&str>, f64>("0x0.0p1324125"), Ok(("", 0.0f64)));
    //     assert_eq!(float::<&str, FastError<&str>, f64>("-0x0"), Ok(("", -0.0f64)));
    //     // FIXME hex float parsing
    //     //assert_eq!(float::<&str, FastError<&str>, f64>("0x400.0p-10"), Ok(("", 1f64)));
    // }
}
