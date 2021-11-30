use nom::{AsBytes, IResult, ToUsize, bytes::complete::take, combinator::map, error::{ErrorKind, ParseError}, multi::many_m_n};
use num::Float;

use crate::format::values::{AsUnsigned, FromBytes};

use super::{BinaryError, BinaryInput};

use std::{convert::TryInto, mem::size_of, ops::{Add, BitOrAssign, Shl}};


pub fn vec_<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, I, I::Error> {
    let (i, n) = uxx::<u32, I>(i)?;
    take(n)(i)
}

#[inline]
pub fn byte<'a, I: 'a + BinaryInput<'a>>(b: u8) -> impl Fn(I) -> IResult<I, u8, I::Error> {
    move |i: I| {
        let (i, ib) = take(1u8)(i)?;
        if let b = ib.as_bytes()[0] {
            Ok((i, b))
        } else {
            Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
        }
    }
}

pub fn uxx<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Out, I::Error>
    where
    Out: num::Unsigned + BitOrAssign + Shl<usize, Output = Out> + From<u8>
{
    let bits = size_of::<Out>() * 8;

    let mut result = Out::zero();
    let mut shift = 0;
    let mut prev_i = i;

    loop {
        let rem_bits = bits - shift;
        let (i, s) = take(1u8)(prev_i)?;
        prev_i = i;
        let byte = s.as_bytes()[0];
        if rem_bits <= 7 && (byte >> rem_bits) != 0 {
            return Err(nom::Err::Error(ParseError::from_error_kind(prev_i, ErrorKind::Char))) // The continuation bit or unused bits are set
        }
        result |= Out::from(byte & 0x7F) << shift;
        if (byte & 0x80) == 0 {
            break;
        }
        shift += 7;
    }
    Ok((prev_i, result))
}


pub fn sxx<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Out, I::Error>
where
    Out: num::Signed + BitOrAssign + Shl<usize, Output=Out> + From<u8>
{
    let bits = size_of::<Out>() * 8;

    let mut result = Out::zero();
    let mut shift = 0;
    let mut prev_i = i;

    loop {
        let rem_bits = bits - shift;
        let (i, s) = take(1u8)(prev_i)?;
        prev_i = i;
        let byte = s.as_bytes()[0];
        if rem_bits <= 7 {
            let continuation_bit = byte & 0x80;
            let sign_and_unused_bit = (byte << 1) as i8 >> (32 - shift);
            if continuation_bit != 0 || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                return Err(nom::Err::Error(ParseError::from_error_kind(prev_i, ErrorKind::Char))) // The continuation bit or unused bits are set
            }
        }
        result |= Out::from(byte & 0x7F) << shift;
        if (byte & 0x80) == 0 {
            break;
        }
        shift += 7;
    }
    Ok((prev_i, result))
}

pub fn ixx<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Out, I::Error>
where
    Out: num::Unsigned + AsUnsigned,
<Out as AsUnsigned>::Repr: num::Signed + From<u8> + Shl<usize, Output=<Out as AsUnsigned>::Repr> + BitOrAssign
{
    map(sxx, |n: <Out as AsUnsigned>::Repr| <Out as AsUnsigned>::get(n))(i)
}

pub fn fxx<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Out, I::Error> where
    Out: Float + FromBytes {
    let (i, bytes) = take(<Out as FromBytes>::N)(i)?;
    let res = Out::from_le_bytes(bytes.as_bytes());
    Ok((i, res))
}


#[cfg(test)]
mod test { }
