#![macro_use]

use nom::{AsChar as NomAsChar, InputIter, InputTakeAtPosition, IResult};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while_m_n};
use nom::character::complete::{char};
use nom::combinator::{map, map_res, opt, peek, value};

use num::{Signed, Unsigned};

use nom::sequence::{delimited, pair};

use num::FromPrimitive;
use lexical_core::Integer;
use crate::format::text::lexer::{AsChar, NumVariant, hex_num, NumParts, dec_num, hex_num_digits};

use crate::format::text::lexer::LexerInput;
use crate::format::text::lexer::Num;
use crate::format::values::AsUnsigned;
use std::iter::FromIterator;
use lexical_core::{FromLexical, Float as LcFloat};
use num::Float;
use std::convert::TryFrom;

#[inline]
pub fn parsed_string<'a, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, String, I::Error> {
    let (mut i, _) = char('\"')(i)?;
    let mut out = String::new();
    loop {
        let str = assign_input!(i, take_while(|c: <I as InputTakeAtPosition>::Item| c.is_strchar()));
        out += str.as_str();
        let c = assign_input!(i, alt((char('\\'), char('\"'))));
        if c == '\"' {
            break;
        } else {
            let parse_escape = alt::<I, _, _, _>((
                value('\t', char('t')),
                value('\n', char('n')),
                value('\r', char('r')),
                value('\"', char('\"')),
                value('\'', char('\'')),
                value('\\', char('\\')),
                map_res(
                    delimited(tag("u{"), hex_num_digits, tag("}")),
                    |parts: Vec<I>| {
                        let cp = parts.into_iter().map(|i| i.as_str()).collect::<Vec<&str>>().join("");
                        let parsed = u32::from_str_radix(cp.as_str(), 16).map_err(|_| ())?;
                        let res = char::try_from(parsed).map_err(|_| ())?;
                        Result::<char, ()>::Ok(res)
                    },
                ),
                map_res(
                    take_while_m_n::<_, I, _>(2, 2, |c: <I as InputIter>::Item| c.is_hex_digit()),
                    |d: I| {
                        Result::<char, ()>::Ok(char::from(
                            u8::from_str_radix(d.as_str(), 16).map_err(|_| ())?,
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

// fn try_parse_integral<Out: num::Num>(&self, radix: u32) -> Result<Out, ()> {
//     let str = self.try_convert_to_integral_string()?;
//     <Out as num::Num>::from_str_radix(str.as_str(), radix).map_err(|_| ())
// }

fn try_convert_to_integral_string<'a, I: LexerInput<'a> + 'a>(n: NumParts<I>) -> Result<String, ()> {
    if n.e == None && n.q == None {
        return Ok(n.p.clone().into_iter().map(|i| i.as_str()).collect::<Vec<&str>>().join(""));
    }
    Err(())
}

#[inline]
fn sign<'a, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, Option<char>, I::Error> {
    opt(alt((char('+'), char('-'))))(i)
}

#[inline]
fn uxx_as_lit_string<'a, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, (String, u32), I::Error> {
    map_res(
        alt((map(hex_num, |n| (n, 16)), map(dec_num, |n| (n, 10)))),
        |(n, radix)| -> Result<(String, u32), ()> { Ok((try_convert_to_integral_string(n)?, radix)) }
    )(i)
}

#[inline]
pub fn parsed_uxx<'a, Out: num::Unsigned, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, Out, I::Error> {
    map_res(uxx_as_lit_string, |(str, n)| <Out as num::Num>::from_str_radix(&str, n))(i)
}

#[inline]
pub fn parsed_sxx<'a, Out: num::Signed, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, Out, I::Error> {
    map_res(pair(sign, uxx_as_lit_string), |(opt_s, (str, n))| {
        let mut signed_str = String::from_iter(opt_s.into_iter());
        signed_str.push_str(&str);
        <Out as num::Num>::from_str_radix(&signed_str, n)
    })(i)
}

#[inline]
pub fn parsed_ixx<'a, Out: num::Unsigned + AsUnsigned, I: 'a + LexerInput<'a>>(i: I) -> IResult<I, Out, I::Error> {
    let (i, sign) = peek(sign)(i)?;

    match sign {
        Some(sign) => map(parsed_sxx::<<Out as AsUnsigned>::Repr, I>, |n| AsUnsigned::get(n))(i),
        None => parsed_uxx::<Out, I>(i),
    }
}

#[inline]
pub fn parsed_fxx<'a, Out, I: 'a + LexerInput<'a>>(preparsed: &'a Num<I>) -> Result<Out, ()>
    where
        Out: Float + LcFloat + FromLexical,
        <Out as LcFloat>::Unsigned: FromPrimitive,
{
    let is_neg = preparsed.sign.map_or(false, |s| s == '-');

    let parsed = match &preparsed.variant {
        NumVariant::NaN(None) => Out::nan(),
        NumVariant::NaN(_) => unimplemented!(), // FIXME: implement NaN with payload
        NumVariant::Inf if is_neg => Out::neg_infinity(),
        NumVariant::Inf => Out::infinity(),
        NumVariant::DecNum(parts) => try_parse_dec_float_num(&preparsed.sign, parts)
            .map_err(|e| ())?,
        NumVariant::HexNum(parts) => try_parse_hex_float_num(&preparsed.sign, parts)
            .map_err(|e| ())?
    };

    Ok(parsed)
}

#[inline]
fn try_parse_dec_float_num<'a, Out: FromLexical, I: LexerInput<'a> + 'a>(s: &Option<char>, preparsed: &NumParts<I>) -> Result<Out, ()> {
    let mut lex_compat_format = String::from_iter(s.iter());

    for p_ in preparsed.p.iter() {
        lex_compat_format.push_str(p_.as_str());
    }

    if let Some(q) = &preparsed.q {
        lex_compat_format.push('.');
        for q_ in q.iter() {
            lex_compat_format.push_str(q_.as_str());
        }
    }

    if let Some((opt_es, e)) = &preparsed.e {
        lex_compat_format.push('e');
        if let Some(es) = opt_es {
            lex_compat_format.push(*es);
        }
        for e_ in e.iter() {
            lex_compat_format.push_str(e_.as_str());
        }
    }

    <Out as FromLexical>::from_lexical_radix(lex_compat_format.as_bytes(), 10).map_err(|e| ())
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
fn try_parse_hex_float_num<'a, Out: LcFloat, I: LexerInput<'a> + 'a>(s: &Option<char>, preparsed: &NumParts<I>) -> Result<Out, ()>
    where <Out as LcFloat>::Unsigned: FromPrimitive,
{
    let is_neg = s.map_or(false, |s| s == '-');

    let mut int_iter = preparsed.p.iter().flat_map(|str| str.as_str().chars());
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

    let mut frac_iter = preparsed.q.iter()
        .flat_map(|t| t.iter())
        .flat_map(|str| str.as_str().chars());

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

    if let Some((s, exp)) = &preparsed.e {
        exponent_is_neg = s.map(|s| s == '-').unwrap_or(false);

        // Exponent is always positive, but significand_exponent is signed.
        // significand_exponent is negated if exponent will be negative, so it
        // can be easily summed to see if the exponent is too large (see below).
        let too_large_exp = max_exp -
            if exponent_is_neg { -significand_exponent } else { significand_exponent };

        let mut exp_iter = exp.iter().flat_map(|str| str.as_str().chars());

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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parsed_string() {
        assert_eq!(
            parsed_string("\"Lorem ipsum dolor sit amet\""),
            Ok(("", "Lorem ipsum dolor sit amet".to_owned()))
        );
        parsed_string("\"Lorem").unwrap_err();
        assert_eq!(
            parsed_string(
                "\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\""
            ),
            Ok(("", "ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()))
        );
        parsed_string("\"\\uinvalid\"").unwrap_err();
        parsed_string("\"\\q\"").unwrap_err();
    }
}
