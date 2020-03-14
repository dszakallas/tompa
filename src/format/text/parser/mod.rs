#![macro_use]

use std::convert::TryFrom;
use std::fmt::Debug;
use std::ops::{Add, RangeFrom};
use std::str::Chars;

use im_rc;
use lexical_core::{Float as LcFloat, Integer as LcInteger};
use lexical_core::FromLexical;
use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, digit1, hex_digit1, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::Err::Error;
use nom::error::{ErrorKind, ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};
use num::{Float, FromPrimitive, Num, Signed, Unsigned};

use crate::syntax::instructions::{Block, IfElse, Instr, Loop};
use crate::syntax::types::{FuncRef, FuncType, GlobalType, Limits, MemArg, MemType, Mut, TableType, ValType};

pub trait AsStr<'a> {
    #[inline]
    fn as_str(self) -> &'a str;
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct IdCtx {
    pub types: im_rc::Vector<Option<String>>,
    pub funcs: im_rc::Vector<Option<String>>,
    pub tables: im_rc::Vector<Option<String>>,
    pub mems: im_rc::Vector<Option<String>>,
    pub globals: im_rc::Vector<Option<String>>,
    pub locals: im_rc::Vector<Option<String>>,
    pub labels: im_rc::Vector<Option<String>>,
    pub typedefs: im_rc::Vector<FuncType>,
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

macro_rules! assign_input {
    ($input:ident, $parser:expr) => {{
        let (i, r) = $parser($input)?;
        $input = i;
        r
    }};
}

#[cfg(test)]
impl<'a> AsStr<'a> for &'a str {
    fn as_str(self) -> &'a str {
        self
    }
}

mod lexical;
mod values;
mod types;
mod instructions;