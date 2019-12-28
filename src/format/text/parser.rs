use lexical_core::{Float as LcFloat, Integer as LcInteger};
use lexical_core::FromLexical;
use nom::error::ParseError;
use nom::{AsChar as NomAsChar, InputTakeAtPosition, InputIter, IResult, Slice, InputTake, Compare, InputLength};
use std::ops::{Add, RangeFrom};
use nom::branch::alt;
use nom::sequence::{tuple, pair, Tuple, terminated, preceded};
use nom::combinator::{map_res, opt, map, not, value, peek};
use nom::character::complete::{char, hex_digit1, digit1, anychar, not_line_ending};
use nom::bytes::complete::{tag, take_while_m_n, take_while, take_while1};
use nom::multi::{fold_many0, many0, many1};
use num::{Unsigned, Num, Signed, Float, FromPrimitive};
use std::convert::TryFrom;
use crate::syntax::types::{ValType, FuncType};
use nom::lib::std::ops::{Range, RangeTo};
use std::str::Chars;

#[cfg(test)]
mod test {
    use super::*;
    use nom::error::{VerboseError, ErrorKind};
    use nom::Err::Error;
    use nom::error::ErrorKind::Eof;

    type FastError<T> = (T, ErrorKind);

    impl<'a> AsStr<'a> for &'a str {
        fn as_str(self) -> &'a str {
            self
        }
    }

    #[test]
    fn test_idchar() {
        assert_eq!(
            idchar::<&str, FastError<&str>>("de432#@ b"),
            Ok((" b", "de432#@"))
        );
    }

    #[test]
    fn test_id() {
        assert_eq!(
            id::<&str, FastError<&str>>("$$f@0f!# a"),
            Ok((" a", "$f@0f!#"))
        );
        id::<&str, FastError<&str>>("0d@0f!# a").unwrap_err();
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string::<FastError<&str>>("\"Lorem ipsum dolor sit amet\""),
            Ok(("", "Lorem ipsum dolor sit amet".to_owned()))
        );
        string::<FastError<&str>>("\"Lorem").unwrap_err();
        assert_eq!(
            string::<FastError<&str>>(
                "\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\""
            ),
            Ok(("", "ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()))
        );
        string::<FastError<&str>>("\"\\uinvalid\"").unwrap_err();
        string::<FastError<&str>>("\"\\q\"").unwrap_err();
    }

    #[test]
    fn test_unsigned_int() {
        assert_eq!(
            unsigned_int::<&str, FastError<&str>, u64>("0xF"),
            Ok(("", 0xF_u64))
        );
        assert_eq!(
            unsigned_int::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEF"),
            Ok(("", 0x0123_4567_89AB_CDEF_u64))
        );
        assert_eq!(
            unsigned_int::<&str, FastError<&str>, u32>("1234"),
            Ok(("", 1234_u32))
        );
        assert_eq!(
            unsigned_int::<&str, FastError<&str>, u32>("000000_00_0_0000000000000000000"),
            Ok(("", 0_u32))
        );
        unsigned_int::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEFFFFFF").unwrap_err();
    }

    #[test]
    fn test_signed_int() {
        signed_int::<&str, FastError<&str>, i8>("0xFF").unwrap_err();
        assert_eq!(
            signed_int::<&str, FastError<&str>, i8>("0x7F"),
            Ok(("", 127_i8))
        );
        assert_eq!(
            signed_int::<&str, FastError<&str>, i8>("-0x8_0"),
            Ok(("", -128_i8))
        );
        assert_eq!(
            signed_int::<&str, FastError<&str>, i8>("-00000_0000_0000000000123"),
            Ok(("", -123_i8))
        );
    }

    #[test]
    fn test_int() {
        assert_eq!(int::<&str, FastError<&str>, u8>("0x7F"), Ok(("", 127_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x8_0"), Ok(("", 128_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x1"), Ok(("", 255_u8)));
        assert_eq!(int::<&str, FastError<&str>, u8>("-0x0"), Ok(("", 0_u8)));
    }

    #[test]
    fn test_float() {
        assert_eq!(float::<&str, FastError<&str>, f32>("inf"), Ok(("", f32::INFINITY)));
        assert_eq!(float::<&str, FastError<&str>, f64>("-inf"), Ok(("", f64::NEG_INFINITY)));
        assert_ne!(
            float::<&str, FastError<&str>, f32>("nan"),
            float::<&str, FastError<&str>, f32>("nan")
        );
        assert_eq!(float::<&str, FastError<&str>, f32>("0.1"), Ok(("", 1e-1f32)));
        assert_eq!(float::<&str, FastError<&str>, f32>("0.000000001"), Ok(("", 1e-9f32)));
        assert_eq!(float::<&str, FastError<&str>, f64>("-1000.000000001"), Ok(("", -1000.000000001f64)));

        assert_eq!(float::<&str, FastError<&str>, f64>("0x0.0pabcedf"), Ok(("", 0.0f64)));
        assert_eq!(float::<&str, FastError<&str>, f64>("-0x0"), Ok(("", -0.0f64)));
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
            token(float::<&str, FastError<&str>, f32>)("-inf"),
            Ok(("", f32::NEG_INFINITY))
        );
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

    #[inline]
    fn as_hex_digit(self) -> u8;
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
fn ws<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, (), E>
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
fn id<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, &'a str, E>
    where
        I: Slice<RangeFrom<usize>> + InputIter + InputTakeAtPosition + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar + AsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
{
    let (i, _) = char('$')(i)?;
    idchar(i)
}

#[inline]
fn idchar<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, &'a str, E>
    where
        I: InputIter + InputTakeAtPosition + AsStr<'a>,
        <I as InputIter>::Item: AsChar,
        <I as InputTakeAtPosition>::Item: AsChar,
{
    let (i, res) = take_while1(AsChar::is_idchar)(i)?;
    Ok((i, res.as_str()))
}

macro_rules! assign_input {
    ($input:ident, $parser:expr) => {{
        let (i, r) = $parser($input)?;
        $input = i;
        r
    }};
}

#[inline]
fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let (mut i, _) = char('\"')(i)?;
    let mut out = String::new();
    loop {
        let str = assign_input!(i, take_while(|c: char| c.is_strchar()));
        out += str;
        let c = assign_input!(i, alt((char('\\'), char('\"'))));
        if c == '\"' {
            break;
        } else {
            let parse_escape = alt((
                value('\t', char('t')),
                value('\n', char('n')),
                value('\r', char('r')),
                value('\"', char('\"')),
                value('\'', char('\'')),
                value('\\', char('\\')),
                map_res(
                    tuple((tag("u{"), |i: &'a str| num(i, hex_digit1), tag("}"))),
                    |(_, cp, _): (&'a str, String, &'a str)| {
                        let parsed = u32::from_str_radix(cp.as_str(), 16).map_err(|_| ())?;
                        let res = char::try_from(parsed).map_err(|_| ())?;
                        Result::<char, ()>::Ok(res)
                    },
                ),
                map_res(
                    take_while_m_n(2, 2, |c: char| c.is_hex_digit()),
                    |d: &'a str| {
                        Result::<char, ()>::Ok(char::from(
                            u8::from_str_radix(d, 16).map_err(|_| ())?,
                        ))
                    },
                ),
            ));
            let c = assign_input!(i, parse_escape);
            out.push(c);
        }
    }
    Ok((i, out))
}

#[inline]
fn num<'a, I: 'a, E: ParseError<I>, F>(i: I, digit: F) -> IResult<I, String, E>
    where
        I: Clone + PartialEq + Slice<RangeFrom<usize>> + InputIter,
        <I as InputIter>::Item: NomAsChar,
        F: Fn(I) -> IResult<I, &'a str, E>,
{
    let (i, radix_str) = digit(i)?;
    fold_many0(
        pair(opt(char('_')), digit),
        radix_str.to_owned(),
        |mut radix_str: String, i: (Option<char>, &'a str)| {
            radix_str.push_str(i.1);
            radix_str
        },
    )(i)
}

#[inline]
fn hex_num<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, String, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    num(i, map(hex_digit1, |i: I| i.as_str()))
}

#[inline]
fn dec_num<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, String, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    num(i, map(digit1, |i: I| i.as_str()))
}

#[inline]
fn unsigned_int<'a, I: 'a, E: ParseError<I>, Out: Unsigned>(i: I) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + Compare<&'static str>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    let (i, hex) = opt(tag("0x"))(i)?;
    match hex {
        Some(_) => map_res(
            |i: I| hex_num(i),
            |s| <Out as Num>::from_str_radix(s.as_str(), 16),
        )(i),
        None => map_res(
            |i: I| dec_num(i),
            |s| <Out as Num>::from_str_radix(s.as_str(), 10),
        )(i),
    }
}

#[inline]
fn sign<I, E: ParseError<I>>(i: I) -> IResult<I, Option<char>, E>
    where
        I: Clone + Slice<RangeFrom<usize>> + InputIter,
        <I as InputIter>::Item: NomAsChar,
{
    opt(alt((char('+'), char('-'))))(i)
}

#[inline]
fn signed_int<'a, I: 'a, E: ParseError<I>, Out: Signed + FromLexical>(i: I) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + Compare<&'static str>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    let (i, sign) = map(sign, |s| s.unwrap_or('+'))(i)?;
    signed_int_with_sign(i, sign)
}

#[inline]
fn signed_int_with_sign<'a, I: 'a, E: ParseError<I>, Out: Signed + FromLexical>(
    i: I,
    sign: char,
) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + Compare<&'static str>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    let (i, hex) = opt(tag("0x"))(i)?;
    match hex {
        Some(_) => map_res(
            |i: I| hex_num(i),
            move |s| <Out as FromLexical>::from_lexical_radix(format!("{}{}", sign, s.as_str()).as_bytes(), 16),
        )(i),
        None => map_res(
            |i: I| dec_num(i),
            move |s| <Out as FromLexical>::from_lexical_radix(format!("{}{}", sign, s.as_str()).as_bytes(), 10),
        )(i),
    }
}

#[inline]
fn int<'a, I: 'a, E: ParseError<I>, Out: Unsigned + FromSigned>(i: I) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + Compare<&'static str>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
        <Out as FromSigned>::Repr: Signed + FromLexical,
{
    let (i, sign) = sign(i)?;
    match sign {
        Some(sign) => map(
            |i| signed_int_with_sign(i, sign),
            |n| FromSigned::get(n),
        )(i),
        None => unsigned_int::<'a, I, E, Out>(i),
    }
}

#[inline]
fn float<'a, I: 'a, E: ParseError<I> + 'a, Out>(i: I) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + InputTake
        + InputIter
        + InputTakeAtPosition
        + Slice<RangeFrom<usize>>
        + Compare<&'static str>
        + AsStr<'a>,
        <Out as LcFloat>::Unsigned: FromPrimitive,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
        Out: Float + LcFloat + FromLexical,
{
    let (i, sign) = sign(i)?;

    let res = alt((
        nan::<I, E, Out>,
        map(tag("inf"), |_| match sign {
            Some('-') => <Out as Float>::neg_infinity(),
            _ => <Out as Float>::infinity()
        }),
        hex_float_num(sign),
        dec_float_num(sign)
    ))(i)?;

    Ok(res)
}

#[inline]
fn nan<'a, I: 'a, E: ParseError<I>, Out: Float + LcFloat>(i: I) -> IResult<I, Out, E>
    where
        I: Clone
        + PartialEq
        + InputTake
        + InputIter
        + InputTakeAtPosition
        + Slice<RangeFrom<usize>>
        + Compare<&'static str>
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
{
    // FIXME: this is only correct for quiet NaNs. Have to implement signaling NaNs as well.
    map(
        tuple((tag("nan"), opt(tuple((tag(":0x"), hex_num))))),
        |_| <Out as Float>::nan(),
    )(i)
}

#[inline]
fn dec_float_num<'a, I: 'a, E: ParseError<I> + 'a, Out>(s: Option<char>) -> impl Fn(I) -> IResult<I, Out, E> + 'a
    where
        I: Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + InputTake
        + InputTakeAtPosition
        + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar,
        Out: FromLexical,
{
    map_res(tuple((
        dec_num,
        opt(tuple((
            opt(map(preceded(char('.'), dec_num), |frac| format!(".{}", frac))),
            opt(map(preceded(alt((char('e'), char('E'))), tuple((sign, dec_num))), |r| format!("e{}{}", r.0.unwrap_or('+'), r.1))),
        ))))), move |preparsed| {
        let mut lexical_compat_format = format!("{}{}", s.unwrap_or('+'), preparsed.0);
        if let Some(qexp) = preparsed.1 {
            if let Some(q) = qexp.0 {
                lexical_compat_format.push_str(q.as_str());
            }
            if let Some(exp) = qexp.1 {
                lexical_compat_format.push_str(exp.as_str());
            }
        }
        <Out as FromLexical>::from_lexical_radix(lexical_compat_format.as_bytes(), 10)
    })
}

type Uxx<Out> = <Out as LcFloat>::Unsigned;

#[inline]
fn make_float<Out: LcFloat>(is_neg: bool, exponent: i32, significand: Uxx<Out>) -> Out
    where <Out as LcFloat>::Unsigned: FromPrimitive, {
    let _1 = Uxx::<Out>::ONE;
    let _0 = Uxx::<Out>::ZERO;

    let sign_part = if is_neg { _1 } else { _0 };

    // FIXME: I don't have the faintest idea why do I have to decrement the exponent part
    // by MANTISSA_SIZE x 2
    let exponent_part = Uxx::<Out>::from_i32(
        exponent + Out::EXPONENT_BIAS - Out::MANTISSA_SIZE - Out::MANTISSA_SIZE
    ).unwrap();

    let res = (sign_part << (Out::BITS as i32 - 1)) |
        (exponent_part << Out::MANTISSA_SIZE) |
        significand;

    println!("res: {:#018x}", res);

    println!("exp: {:#018x}", 0.0f32.to_bits());

    Out::from_bits(
        (sign_part << (Out::BITS as i32 - 1)) |
            (exponent_part << Out::MANTISSA_SIZE) |
            significand
    )
}

#[inline]
fn hex_float_num<'a, I: 'a, E: ParseError<I> + 'a, Out>(s: Option<char>) -> impl Fn(I) -> IResult<I, Out, E> + 'a
    where
        I: Clone
        + InputTake
        + InputTakeAtPosition
        + Compare<&'static str>
        + PartialEq
        + Slice<RangeFrom<usize>>
        + InputIter
        + AsStr<'a>,
        Out: LcFloat,
        <Out as LcFloat>::Unsigned: FromPrimitive,
        <I as InputIter>::Item: NomAsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar, {
    map_res(preceded(
        tag("0x"),
        tuple((
            hex_num,
            opt(tuple((
                opt(preceded(char('.'), hex_num)),
                opt(preceded(alt((char('p'), char('P'))), tuple((sign, hex_num)))),
            )))))), move |preparsed| {
        let is_neg = s.map(|s| s == '-').unwrap_or(false);

        let mut int = preparsed.0.as_str().chars();

        let mut significand = Uxx::<Out>::zero();
        let mut significand_exponent = 0i32;
        let mut seen_trailing_non_zero = false;

        while let Some(c) = int.next() {
            let digit = c.as_hex_digit();
            if <Out as LcFloat>::BITS as u32 - significand.leading_zeros() <= (<Out as LcFloat>::MANTISSA_SIZE + 1) as u32 {
                significand = (significand << 4) + Uxx::<Out>::from_u8(digit).unwrap();
            } else {
                seen_trailing_non_zero |= digit != 0;
                significand_exponent += 4;
            }
        }

        let mut frac = preparsed.1.iter()
            .flat_map(|t| t.0.iter())
            .flat_map(|str| str.chars());

        while let Some(c) = frac.next() {
            let digit = c.as_hex_digit();
            if <Out as LcFloat>::BITS as u32 - significand.leading_zeros() <= (<Out as LcFloat>::MANTISSA_SIZE + 1) as u32 {
                significand = (significand << 4) + Uxx::<Out>::from_u8(digit).unwrap();
                significand_exponent -= 4;
            } else {
                seen_trailing_non_zero |= digit != 0;
            }
        }

        if significand == Uxx::<Out>::ZERO {
            return Ok(make_float(is_neg, -<Out as LcFloat>::MAX_EXPONENT + 1, significand));
        }


        let res: Result<Out, ()> = unimplemented!("Implement hex float parser");
        return res;
    })
}

#[inline]
fn valtype<I, E: ParseError<I>>(i: I) -> IResult<I, ValType, E>
    where
        I: Clone
        + InputTake
        + Compare<&'static str>, {
    alt((
        value(ValType::I32, tag("i32")),
        value(ValType::I64, tag("i64")),
        value(ValType::F32, tag("f32")),
        value(ValType::F64, tag("f64"))
    ))(i)
}

#[inline]
fn token<I, E: ParseError<I>, F, O>(parser: F) -> impl Fn(I) -> IResult<I, O, E>
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
        + AsStr<'static>
        + Compare<&'static str>,
        <I as InputIter>::Item: NomAsChar + AsChar,
        <I as InputTakeAtPosition>::Item: AsChar
        + PartialEq,
        F: Fn(I) -> IResult<I, O, E>, {
    terminated(parser, terminated(peek(not(idchar)), alt((
        value((), many0(ws)),
        value((), peek(alt((char('('), char(')')))))
    ))))
}

//#[inline]
//fn block<I, O, E: ParseError<I>, List: Tuple<I, O, E>>(inner: List) -> impl Fn(I) -> IResult<I, O, E>
//    where
//    I: Clone
//    + Slice<RangeFrom<usize>>
//    + InputIter,
//    <I as InputIter>::Item: NomAsChar, {
//    map(tuple((char('('), tuple(inner), char(')'))), |t| t.1)
//}
//
//#[inline]
//fn functype<I, E: ParseError<I>>(i: I) -> IResult<I, FuncType, E> {
//    map(
//        block((tag("func"), many0(param), many0(result))),
//        |b| {
//
//        }
//    )
//}

//#[inline]
//fn param<I, E: ParseError<I>>(i: I) -> IResult<I, (Vec<ValType>, Vec<String>), E>
//where
//I: Clone
//+ Slice<RangeFrom<usize>>
//+ PartialEq
//+ InputIter
//+ InputTake
//+ InputTakeAtPosition
//+ Compare<&'static str>
//+ AsStr<'static>,
//<I as InputIter>::Item: NomAsChar + AsChar,
//<I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//    block((tag("param"), alt((
//        map(tuple((id, valtype)), |t| (vec![t.1], vec![id])),
//        map(many1(valtype), |valtypes| (valtypes, vec![])
//    )))))(i)
//}
//
//#[inline]
//fn result<I, E: ParseError<I>>(i: I) -> IResult<I, (Vec<ValType>, Vec<String>), E>
//    where
//        I: Clone
//        + Slice<RangeFrom<usize>>
//        + PartialEq
//        + InputIter
//        + InputTake
//        + InputTakeAtPosition
//        + Compare<&'static str>
//        + AsStr<'static>,
//        <I as InputIter>::Item: NomAsChar + AsChar,
//        <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//    block((tag("result"), alt((
//        map(tuple((id, valtype)), |t| (vec![t.1], vec![id])),
//        map(many1(valtype), |valtypes| (valtypes, vec![])
//        )))))(i)
//}