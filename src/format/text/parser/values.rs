use std::ops::RangeFrom;
use nom::{InputIter, IResult, Slice};
use num::{Unsigned, Signed};
use crate::format::text::lexer::{Token, CharStream};
use crate::format::text::parser::lexical::{parsed_uxx, parsed_string, parsed_ixx, parsed_fxx};
use crate::format::text::parser::{Parser, ParserError, ParserType, TokenStream};
use crate::format::values::AsUnsigned;
use crate::format::input::{ParseError, WithParseError, WithWrappedStream};
use lexical_core::FromLexical;

use lexical_core::{Float as LcFloat};
use nom::error::ErrorKind;
use num::Float;

use num::FromPrimitive;
use crate::format::text::error::{VerboseError, VerboseErrorKind};
use crate::format::text::error::VerboseErrorKind::Text;


#[inline]
fn matches<I: WithParseError<E>, E, F, O>(f: F) -> impl Fn(I) -> Option<(I, O)>
    where
        I: Slice<RangeFrom<usize>> + InputIter,
        F: Fn(<I as InputIter>::Item) -> Option<O>
{
    move |i: I| {
        match (i).iter_elements().next() {
            Some(t) => match f(t) {
                Some(o) => Some((i.slice(1..), o)),
                None => None
            }
            None => None
        }
    }
}

#[inline]
pub fn string<'a, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, String, I::Error>
    where I::Inner: CharStream<'a>
{
    token_type!(i, String, Token::String(o), o, parsed_string::<I::Inner>)
}

#[inline]
pub fn uxx<'a, Out: Unsigned, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where I::Inner: CharStream<'a>
{
    token_type!(i, Uxx, Token::Num(o, _), o, parsed_uxx::<Out, I::Inner>)
}

#[inline]
pub fn ixx<'a, Out: Unsigned + AsUnsigned, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where
        I::Inner: CharStream<'a>,
        <Out as AsUnsigned>::Repr: Signed + FromLexical,
{
    token_type!(i, Ixx, Token::Num(o, _), o, parsed_ixx::<Out, I::Inner>)
}

#[inline]
pub fn fxx<'a, Out, I: TokenStream<'a> + 'a>(i: I) -> IResult<I, Out, I::Error>
    where
        I::Inner: CharStream<'a>,
        Out: Float + LcFloat + FromLexical,
        <Out as LcFloat>::Unsigned: FromPrimitive,
{
    if let Some((i, preparsed)) = matches(|tok: &'a Token<I::Inner>| if let Token::Num(_, num) = tok {
        Some(num)
    } else {
        None
    })(i.clone()) {
        let p = parsed_fxx::<Out, I::Inner>(preparsed).map_err(
            |_| nom::Err::Error(ParseError::from_error_kind(i.clone(), Parser { tpe: ParserType::Fxx, lexer_error: None }))
        )?;
        Ok((i, p))
    } else {
        Err(nom::Err::Error(ParseError::from_error_kind(i, Parser { tpe: ParserType::Fxx, lexer_error: None })))
    }

}

#[cfg(test)]
mod test {

    use nom::error::{ErrorKind, VerboseError};

    use crate::format::input::Stream;
    use super::*;

    use crate::format::values::AsUnsigned;

    type FastError<T> = VerboseError<T>;

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
            assert_eq!(consumed!(string, Stream::new(&t)), Ok("Lorem ipsum dolor sit amet".to_owned()));
        }
        {
            
            let t = lex!("\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\"").unwrap();
            assert_eq!(consumed!(string, Stream::new(&t)), Ok("ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()));
        }
        {
            let t = lex!("\"Lorem").unwrap();
            let e = consumed!(string, Stream::new(&t)).unwrap_err();
        }
        {
            let t = lex!("\"\\uinvalid\"").unwrap();
            consumed!(string, Stream::new(&t)).unwrap_err();
        }
        {

            let t = lex!("\"\\q\"").unwrap();
            consumed!(string, Stream::new(&t)).unwrap_err();
        }
        {

            let t = lex!("42").unwrap();
            let e = consumed!(string, Stream::new(&t)).unwrap_err();
            println!("{:?}", e);
        }
    }

    #[test]
    fn test_unsigned_int() {
        {
            let t = lex!("0xF").unwrap();
            assert_eq!(consumed!(uxx, Stream::new(&t)), Ok(0xF_u64));
        }

        {
            let t = lex!("0x012_3_456_789_ABCDEF").unwrap();
            assert_eq!(consumed!(uxx, Stream::new(&t)), Ok(0x0123_4567_89AB_CDEF_u64));
        }
        {
            let t = lex!("1234").unwrap();
            assert_eq!(consumed!(uxx, Stream::new(&t)), Ok(1234_u32));
        }

        {
            let t = lex!("000000_00_0_0000000000000000000").unwrap();
            assert_eq!(consumed!(uxx, Stream::new(&t)), Ok(0_u32));
        }
        {
            
            let t = lex!("0x012_3_456_789_ABCDEFFFFFF").unwrap();
            let e = consumed!(uxx::<u32, _>, Stream::new(&t)).unwrap_err();
        }
    }

    #[test]
    fn test_signed_int() {
        {
            let t = lex!("0xFF").unwrap();
            consumed!(ixx::<u8, _>, Stream::new(&t)).unwrap_err();
        }
        {
            let t = lex!("0x7F").unwrap();
            assert_eq!(consumed!(ixx, Stream::new(&t)), Ok(127_u8));
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
