use std::convert::TryFrom;
use std::ops::{RangeFrom};



use lexical_core::{Float as LcFloat, Integer as LcInteger};
use lexical_core::FromLexical;
use nom::{AsChar as NomAsChar, Compare, InputIter, InputTake, InputTakeAtPosition, IResult, Slice};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while_m_n};
use nom::character::complete::{anychar, char, digit1, hex_digit1, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::error::{ParseError};

use nom::multi::{fold_many0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};
use num::{Float, FromPrimitive, Num, Signed, Unsigned};





use crate::format::text::parser::FromSigned;


use crate::format::text::parser::AsStr;
use crate::format::text::parser::lexical::AsChar;
use crate::format::text::parser::lexical::idchar;

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
                    delimited(tag("u{"), |i: &'a str| num(i, hex_digit1), tag("}")),
                    |cp: String| {
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
fn sign<I, E: ParseError<I>>(i: I) -> IResult<I, Option<char>, E>
    where
        I: Clone + Slice<RangeFrom<usize>> + InputIter,
        <I as InputIter>::Item: NomAsChar,
{
    opt(alt((char('+'), char('-'))))(i)
}


#[inline]
pub fn uxx<'a, I: 'a, E: ParseError<I>, Out: Unsigned>(i: I) -> IResult<I, Out, E>
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
fn sxx_with_sign<'a, I: 'a, E: ParseError<I>, Out: Signed + FromLexical>(
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
fn sxx<'a, I: 'a, E: ParseError<I>, Out: Signed + FromLexical>(i: I) -> IResult<I, Out, E>
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
    sxx_with_sign(i, sign)
}

// uninterpreted integer, stored as unsigned
#[inline]
fn ixx<'a, I: 'a, E: ParseError<I>, Out: Unsigned + FromSigned>(i: I) -> IResult<I, Out, E>
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
            |i| sxx_with_sign(i, sign),
            |n| FromSigned::get(n),
        )(i),
        None => uxx::<'a, I, E, Out>(i),
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

fn make_float<Out: LcFloat>(is_neg: bool, exponent: i32, significand: Uxx<Out>) -> Out
    where <Out as LcFloat>::Unsigned: FromPrimitive, {
    let _1 = Uxx::<Out>::ONE;
    let _0 = Uxx::<Out>::ZERO;

    let sign_part = if is_neg { _1 } else { _0 };

    // Out::EXPONENT_BIAS includes Out::MANTISSA_SIZE, but we don't need that
    let exponent_part = Uxx::<Out>::from_i32(
        exponent + Out::EXPONENT_BIAS - Out::MANTISSA_SIZE
    ).unwrap();

    Out::from_bits(
        (sign_part << (Out::BITS as i32 - 1)) |
            (exponent_part << Out::MANTISSA_SIZE) |
            significand
    )
}

fn shift_and_round_to_nearest<Out: LcFloat>(
    mut significand: Uxx<Out>,
    shift: i32,
    seen_trailing_non_zero: bool) -> Uxx<Out> {
    if (significand & (Uxx::<Out>::ONE << shift)) != Uxx::<Out>::ZERO || seen_trailing_non_zero {
        significand += Uxx::<Out>::ONE << (shift - 1);
    }
    significand = significand >> shift;
    significand
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
                opt(preceded(alt((char('p'), char('P'))), tuple((sign, dec_num)))),
            )))))), move |preparsed| {
        let is_neg = s.map(|s| s == '-').unwrap_or(false);

        let mut int_iter = preparsed.0.as_str().chars();
        let mut significand = Uxx::<Out>::zero();
        let mut significand_exponent = 0i32;
        let mut seen_trailing_non_zero = false;

        let max_exp = <Out as LcFloat>::MAX_EXPONENT + <Out as LcFloat>::MANTISSA_SIZE;
        let min_exp = -max_exp + 1;
        let _1 = Uxx::<Out>::ONE;
        let _0 = Uxx::<Out>::ZERO;


        while let Some(c) = int_iter.next() {
            let digit = c.as_hex_digit();
            if <Out as LcFloat>::BITS as u32 - significand.leading_zeros() <= (<Out as LcFloat>::MANTISSA_SIZE + 1) as u32 {
                significand = (significand << 4) + Uxx::<Out>::from_u8(digit).unwrap();
            } else {
                seen_trailing_non_zero |= digit != 0;
                significand_exponent += 4;
            }
        }

        let mut frac_iter = preparsed.1.iter()
            .flat_map(|t| t.0.iter())
            .flat_map(|str| str.chars());

        while let Some(c) = frac_iter.next() {
            let digit = c.as_hex_digit();
            if <Out as LcFloat>::BITS as u32 - significand.leading_zeros() <= (<Out as LcFloat>::MANTISSA_SIZE + 1) as u32 {
                significand = (significand << 4) + Uxx::<Out>::from_u8(digit).unwrap();
                significand_exponent -= 4;
            } else {
                seen_trailing_non_zero |= digit != 0;
            }
        }

        if significand == _0 {
            return Ok(make_float(is_neg, min_exp, significand));
        }

        let mut exponent = 0i32;
        let mut exponent_is_neg = false;

        if let Some((s, exp)) = preparsed.1.iter().flat_map(|t| t.1.iter()).next() {
            exponent_is_neg = s.map(|s| s == '-').unwrap_or(false);

            // Exponent is always positive, but significand_exponent is signed.
            // significand_exponent is negated if exponent will be negative, so it
            // can be easily summed to see if the exponent is too large (see below).
            let too_large_exp = max_exp -
                if exponent_is_neg { -significand_exponent } else { significand_exponent };

            let mut exp_iter = exp.chars();

            while let Some(c) = exp_iter.next() {
                let digit = c.as_dec_digit();
                exponent = exponent * 10 + (digit as i32);
                if exponent >= too_large_exp {
                    return Err(());
                }
            }

            if exponent_is_neg {
                exponent = -exponent;
            }
        }

        let significand_bits = <Out as LcFloat>::BITS as i32 - significand.leading_zeros() as i32;

        exponent += significand_exponent + significand_bits - 1;

        if exponent <= min_exp {

            // Maybe subnormal

            // Normalize significand
            if significand_bits > <Out as LcFloat>::MANTISSA_SIZE {
                let shift = significand_bits - <Out as LcFloat>::MANTISSA_SIZE;
                significand = significand >> shift;
                // update seen_trailing_non_zero;
                let mask = (_1 << (shift - 1)) - _1;
                seen_trailing_non_zero |= (significand & mask) != _0;
            } else if significand_bits < <Out as LcFloat>::MANTISSA_SIZE {
                significand = significand << (<Out as LcFloat>::MANTISSA_SIZE - significand_bits);
            }

            let shift = min_exp - exponent;
            if shift <= <Out as LcFloat>::MANTISSA_SIZE {
                if shift > 0 {
                    // update seen_trailing_non_zero;
                    let mask = (_1 << (shift - 1)) - _1;
                    seen_trailing_non_zero |= (significand & mask) != _0;

                    significand = shift_and_round_to_nearest::<Out>(significand, shift, seen_trailing_non_zero);
                }

                exponent = min_exp;

                if significand != _0 {
                    return Ok(make_float(is_neg, exponent, significand));
                }
            }
            return Ok(make_float(is_neg, min_exp, _0));
        }

        if significand_bits > <Out as LcFloat>::MANTISSA_SIZE + 1 {
            significand = shift_and_round_to_nearest::<Out>(
                significand,
                significand_bits - <Out as LcFloat>::MANTISSA_SIZE + 1,
                seen_trailing_non_zero,
            );
            if significand > (_1 << <Out as LcFloat>::MANTISSA_SIZE + 1) - _1 {
                exponent = exponent + 1;
            }
        } else if exponent >= max_exp {
            // Would be inf or -inf, but the spec doesn't allow rounding hex-floats to
            // infinity.
            return Err(());
        }

        Ok(make_float(is_neg, exponent, significand & <Out as LcFloat>::MANTISSA_MASK))
    })
}

#[inline]
pub fn id<'a, I: 'a, E: ParseError<I>>(i: I) -> IResult<I, &'a str, E>
    where
        I: Slice<RangeFrom<usize>> + InputIter + InputTakeAtPosition + AsStr<'a>,
        <I as InputIter>::Item: NomAsChar + AsChar,
        <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
{
    preceded(char('$'), idchar)(i)
}


#[cfg(test)]
mod test {
    
    use nom::error::{ErrorKind};
    

    use super::*;

    type FastError<T> = (T, ErrorKind);

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
            uxx::<&str, FastError<&str>, u64>("0xF"),
            Ok(("", 0xF_u64))
        );
        assert_eq!(
            uxx::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEF"),
            Ok(("", 0x0123_4567_89AB_CDEF_u64))
        );
        assert_eq!(
            uxx::<&str, FastError<&str>, u32>("1234"),
            Ok(("", 1234_u32))
        );
        assert_eq!(
            uxx::<&str, FastError<&str>, u32>("000000_00_0_0000000000000000000"),
            Ok(("", 0_u32))
        );
        uxx::<&str, FastError<&str>, u64>("0x012_3_456_789_ABCDEFFFFFF").unwrap_err();
    }

    #[test]
    fn test_signed_int() {
        sxx::<&str, FastError<&str>, i8>("0xFF").unwrap_err();
        assert_eq!(
            sxx::<&str, FastError<&str>, i8>("0x7F"),
            Ok(("", 127_i8))
        );
        assert_eq!(
            sxx::<&str, FastError<&str>, i8>("-0x8_0"),
            Ok(("", -128_i8))
        );
        assert_eq!(
            sxx::<&str, FastError<&str>, i8>("-00000_0000_0000000000123"),
            Ok(("", -123_i8))
        );
    }

    #[test]
    fn test_int() {
        assert_eq!(ixx::<&str, FastError<&str>, u8>("0x7F"), Ok(("", 127_u8)));
        assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x8_0"), Ok(("", 128_u8)));
        assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x1"), Ok(("", 255_u8)));
        assert_eq!(ixx::<&str, FastError<&str>, u8>("-0x0"), Ok(("", 0_u8)));
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

        assert_eq!(float::<&str, FastError<&str>, f64>("0x0.0p1324125"), Ok(("", 0.0f64)));
        assert_eq!(float::<&str, FastError<&str>, f64>("-0x0"), Ok(("", -0.0f64)));
        // FIXME hex float parsing
        //assert_eq!(float::<&str, FastError<&str>, f64>("0x400.0p-10"), Ok(("", 1f64)));
    }
}