use nom::{tag, named, alt, char, tuple, take_while1, AsChar as NomAsChar, call, take_while, IResult, do_parse, error::{ErrorKind, ParseError}, character::complete::{char, hex_digit1, digit1}, branch::alt, bytes::complete::{take_while, take_while1, tag}, multi::{many0, fold_many0}, sequence::{tuple}, InputTakeAtPosition, sequence::pair, combinator::{opt, map_res, map}, InputIter, Slice, InputTake, Compare, InputLength};
use num::{Num, Unsigned, Signed, Float};
use std::fmt::Display;
use std::io::Write;

use std::convert::TryFrom;
use nom::bytes::complete::take_while_m_n;
use std::ops::RangeFrom;

#[cfg(test)]
mod test {
    use super::*;
    use nom::error::VerboseError;

    type FastError<T> = (T, ErrorKind);

    impl<'a> AsStr<'a> for &'a str {
        fn as_str(self) -> &'a str {
            self
        }
    }

    #[test]
    fn test_idchar() {
        assert_eq!(idchar::<&str, FastError<&str>>("de432#@ b"), Ok((" b", "de432#@")));
    }

    #[test]
    fn test_id() {
        assert_eq!(id::<&str, FastError<&str>>("$$f@0f!# a"), Ok((" a", "$f@0f!#")));
        id::<&str, FastError<&str>>("0d@0f!# a").unwrap_err();
    }

    #[test]
    fn test_string() {
        assert_eq!(string::<FastError<&str>>("\"Lorem ipsum dolor sit amet\""), Ok(("", "Lorem ipsum dolor sit amet".to_owned())));
        string::<FastError<&str>>("\"Lorem").unwrap_err();
        assert_eq!(
            string::<FastError<&str>>("\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\""),
            Ok(("", "ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()))
        );
        String::from("sdsdfsd").as_str();
        string::<FastError<&str>>("\"\\uinvalid\"").unwrap_err();
        string::<FastError<&str>>("\"\\q\"").unwrap_err();
    }

    #[test]
    fn test_unsigned_int() {
        assert_eq!(unsigned_int::<&str, FastError<&str>, u64>("0xF"), Ok(("", 0xF_u64)));
        assert_eq!(unsigned_int::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEF"), Ok(("", 0x0123_4567_89AB_CDEF_u64)));
        assert_eq!(unsigned_int::<&str, FastError<&str>, u32>("1234"), Ok(("", 1234_u32)));
        assert_eq!(unsigned_int::<&str, FastError<&str>, u32>("000000_00_0_0000000000000000000"), Ok(("", 0_u32)));
        unsigned_int::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEFFFFFF").unwrap_err();
    }

    #[test]
    fn test_signed_int() {
        signed_int::<&str, FastError<&str>, i8>("0xFF").unwrap_err();
        assert_eq!(signed_int::<&str, FastError<&str>, i8>("0x7F"), Ok(("", 127_i8)));
        assert_eq!(signed_int::<&str, FastError<&str>, i8>("-0x8_0"), Ok(("", -128_i8)));
        assert_eq!(signed_int::<&str, FastError<&str>, i8>("-00000_0000_0000000000123"), Ok(("", -123_i8)));
    }

    #[test]
    fn test_int() {
        assert_eq!(int::<&str, FastError<&str>, u8>("0x7F"), Ok(("", 127_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x8_0"), Ok(("", 128_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x1"), Ok(("", 255_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x0"), Ok(("", 0_u8)));
    }
}

trait FromSigned {
    type Repr;

    #[inline]
    fn get(r: Self::Repr) -> Self;
}

macro_rules! from_signed_impl {
    ($($from:tt -> $to:tt),*) => {
        $(
            impl FromSigned for $to {
                type Repr = $from;

                fn get(r: Self::Repr) -> Self {
                    r as Self
                }
            }
        )*
    }
}

from_signed_impl!(i8 -> u8, i16 -> u16, i32 -> u32, i64 -> u64, i128 -> u128);

trait AsStr<'a> {
    #[inline]
    fn as_str(self) -> &'a str;
}

trait AsChar {
    /// Is a valid character in an identifier?
    #[inline]
    fn is_idchar(self) -> bool;

    #[inline]
    fn is_idcontrol(self) -> bool;

    /// Is a valid standalone character in a string?
    #[inline]
    fn is_strchar(self) -> bool;
}

impl AsChar for char {
    fn is_idchar(self) -> bool {
        self.is_alphanumeric() || self.is_idcontrol()
    }

    fn is_idcontrol(self) -> bool {
        self.is_ascii() && match self as u8 {
            b'!' | b'#'..=b'\'' | b'*'..=b'/' | b':' | b'<'..=b'@' | b'\\' | b'^'..=b'`' | b'|' | b'~' => true,
            _ => false
        }
    }

    fn is_strchar(self) -> bool {
        let ch = self as u32;
        ch >= 0x20 && ch != 0x22 && ch != 0x5C && ch != 0x7F
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
}

#[inline]
fn id<I, E: ParseError<I>>(i: I) -> IResult<I, I, E> where
    I: Slice<RangeFrom<usize>> + InputIter + InputTakeAtPosition,
    <I as InputIter>::Item: NomAsChar + AsChar,
    <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
{
    let (i, _) = char('$')(i)?;
    idchar(i)
}

#[inline]
fn idchar<I, E: ParseError<I>>(i: I) -> IResult<I, I, E> where
    I: InputIter + InputTakeAtPosition,
    <I as InputIter>::Item: AsChar,
    <I as InputTakeAtPosition>::Item: AsChar,
{
    take_while1(AsChar::is_idchar)(i)
}

//#[inline]
//fn simple_utf8_strchar<I, E: ParseError<I>>(i: I) -> IResult<I, I, E>
//    where
//        I: InputTakeAtPosition,
//        <I as InputTakeAtPosition>::Item: AsChar,
//{
//    i.split_at_position1_complete(|item| !item.is_strchar(), nom::error::ErrorKind::Char)
//}

macro_rules! assign_input {
    ($input:ident, $parser:expr) => {{
        let (i, r) = $parser($input)?;
        $input = i;
        r
    }};
}

// FIXME add string length bound
#[inline]
fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let (mut i, _) = char('\"')(i)?;
    let mut out = String::new();
    loop {
        let str = assign_input!(i, take_while(|c: char| c.is_strchar()));
        out += str;
        let c = assign_input!(i, alt((char('\\'), char('\"'))));
        if c == '\"' {
            break
        } else {
            let parse_escape = alt((
                map(tag("t"), |_| '\t'),
                map(tag("n"), |_| '\n'),
                map(tag("r"), |_| '\r'),
                map(tag("\""), |_| '\"'),
                map(tag("'"), |_| '\''),
                map(tag("\\"), |_| '\\'),
                map_res(tuple((tag("u{"), |i: &'a str| num(i, hex_digit1), tag("}"))), |(_, cp, _): (&'a str, String, &'a str)| {
                    let parsed = u32::from_str_radix(cp.as_str(), 16).map_err(|_| ())?;
                    let res = char::try_from(parsed).map_err(|_| ())?;
                    Result::<char, ()>::Ok(res)
                }),
                map_res(take_while_m_n(2, 2, |c: char| c.is_hex_digit()), |d: &'a str| {
                    Result::<char, ()>::Ok(char::from(u8::from_str_radix(d, 16).map_err(|_| ())?))
                })
            ));
            let c = assign_input!(i, parse_escape);
            out.push(c);
        }
    }
    Ok((i, out))
}

#[inline]
fn num<'a, I: 'a, E: ParseError<I>, F>(i: I, digit: F) -> IResult<I, String, E> where
    I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: NomAsChar,
    F: Fn(I) -> IResult<I, &'a str, E>, {
    let (i, radix_str) = digit(i)?;
    fold_many0(pair(opt(char('_')), digit),
               radix_str.to_owned(),
               |mut radix_str: String, i: (Option<char>, &'a str)| {
                   radix_str.push_str(i.1);
                   radix_str
               }
    )(i)
}

#[inline]
fn unsigned_int<'a, I: 'a, E: ParseError<I>, Out: Unsigned>(i: I) -> IResult<I, Out, E> where
    I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter + InputTake + InputTakeAtPosition + Compare<&'static str> + AsStr<'a>,
    <I as InputIter>::Item: NomAsChar,
    <I as InputTakeAtPosition>::Item: NomAsChar,
{
    let (i, hex) = opt(tag("0x"))(i)?;
    match hex {
        Some(_) => map_res(|i: I| num(i, map(hex_digit1, |i: I| i.as_str())), |s| <Out as Num>::from_str_radix(s.as_str(), 16))(i),
        None => map_res(|i: I| num(i, map(digit1, |i: I| i.as_str())), |s| <Out as Num>::from_str_radix(s.as_str(), 10))(i)
    }
}

#[inline]
fn sign<I, E: ParseError<I>>(i: I) -> IResult<I, Option<char>, E> where
    I: Clone + Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: NomAsChar
{
    opt(alt((char('+'), char('-'))))(i)
}

#[inline]
fn signed_int<'a, I: 'a, E: ParseError<I>, Out: Signed>(i: I) -> IResult<I, Out, E> where
    I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter + InputTake + InputTakeAtPosition + Compare<&'static str> + AsStr<'a>,
    <I as InputIter>::Item: NomAsChar,
    <I as InputTakeAtPosition>::Item: NomAsChar, {
    let (i, sign) = map(sign, |s| s.unwrap_or('+'))(i)?;
    signed_int_with_sign(i, sign)
}

#[inline]
fn signed_int_with_sign<'a, I: 'a, E: ParseError<I>, Out: Signed>(i: I, sign: char) -> IResult<I, Out, E> where
    I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter + InputTake + InputTakeAtPosition + Compare<&'static str> + AsStr<'a>,
    <I as InputIter>::Item: NomAsChar,
    <I as InputTakeAtPosition>::Item: NomAsChar, {
    let (i, hex) = opt(tag("0x"))(i)?;
    match hex {
        Some(_) => map_res(|i: I| num(i, map(hex_digit1, |i: I| i.as_str())), move |s| <Out as Num>::from_str_radix(format!("{}{}", sign, s.as_str()).as_str(), 16))(i),
        None => map_res(|i: I| num(i, map(digit1, |i: I| i.as_str())), move |s| <Out as Num>::from_str_radix(format!("{}{}", sign, s.as_str()).as_str(), 10))(i)
    }
}

#[inline]
fn int<'a, I: 'a, E: ParseError<I>, Out: Unsigned + FromSigned>(i: I) -> IResult<I, Out, E> where
    I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter + InputTake + InputTakeAtPosition + Compare<&'static str> + AsStr<'a>,
    <I as InputIter>::Item: NomAsChar,
    <I as InputTakeAtPosition>::Item: NomAsChar,
    <Out as FromSigned>::Repr: Signed {
    let (i, sign) = sign(i)?;
    match sign {
        Some(sign) => map(|i| signed_int_with_sign::<'a, I, E, <Out as FromSigned>::Repr>(i, sign), |n| FromSigned::get(n))(i),
        None => unsigned_int::<'a, I, E, Out>(i),
    }
}



//#[inline]
//fn float<'a, E: ParseError<&'a str>, Out: Float>() {
//
//}






//named!(string<&str, (char, &str, char)>, tuple!(
//    char!('\"'),
//    take_while!(alt!(
//        //call!(AsChar::is_strchar) |
//        tag!("\\t") |
//        tag!("\\n") |
//        tag!("\\r") |
//        tag!("\\\"") |
//        tag!("\\'") |
//        tag!("\\\\")
//    )),
//    char!('\"')
//));