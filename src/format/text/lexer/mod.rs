#![macro_use]

use std::ops::RangeFrom;

use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice, Offset};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, not_line_ending, hex_digit1, digit1};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};

use nom::error::{ParseError, ErrorKind};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{fold_many0, many0, many1, separated_list, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};

use std::fs;
use std::io::{Read, BufReader};
use nom_locate::{LocatedSpan, position};

use nom::character::streaming::alpha1;
use std::convert::TryFrom;
use im_rc::HashSet;

use keyword::Keyword;
use keyword::KEYWORDS_HASH;

pub(crate) mod keyword;

type Span<'a> = LocatedSpan<&'a str>;

macro_rules! assign_input {
    ($input:ident, $parser:expr) => {{
        let (i, r) = $parser($input)?;
        $input = i;
        r
    }};
}

pub trait AsStr<'a> {
    #[inline]
    fn as_str(&self) -> &'a str;
}

impl<'a> AsStr<'a> for &'a str {
    #[inline]
    fn as_str(&self) -> Self {
        self
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token<I> {
    String(I),
    LPar(I),
    RPar(I),
    Keyword(I, Keyword),
    Id(I),
    Num(I, Num<I>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Num<I> {
    pub sign: Option<char>,
    pub variant: NumVariant<I>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NumVariant<I> {
    DecNum(NumParts<I>),
    HexNum(NumParts<I>),
    Inf,
    NaN(Option<Vec<I>>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct NumParts<I> {
    pub p: Vec<I>,
    pub q: Option<Vec<I>>,
    pub e: Option<(Option<char>, Vec<I>)>,
}

impl<'a, I: PartialEq + AsStr<'a> + Clone> NumParts<I> {
    pub fn try_parse_integral<Out: num::Num>(&self, radix: u32) -> Result<Out, ()> {
        let str = self.try_convert_to_integral_string()?;
        <Out as num::Num>::from_str_radix(str.as_str(), radix).map_err(|_| ())
    }

    pub fn try_convert_to_integral_string(&self) -> Result<String, ()> {
        if self.e == None && self.q == None {
            return Ok(self.p.clone().into_iter().map(|i| i.as_str()).collect::<Vec<&str>>().join(""));
        }
        Err(())
    }
}

/// Blanket for traits required by the lexer
pub trait LexerInput<'a>: Clone
    + PartialEq
    + Slice<RangeFrom<usize>>
    + Slice<Range<usize>>
    + Slice<RangeTo<usize>>
    + InputIter<Item: NomAsChar + AsChar>
    + InputLength
    + InputTakeAtPosition<Item: NomAsChar + AsChar>
    + InputTake
    + Offset
    + Compare<&'static str>
    + AsStr<'a> {
    type InputIterItem;
    type InputTakeAtPositionItem;
}

impl<'a, I> LexerInput<'a> for I
    where
        I : Clone
        + PartialEq
        + Slice<RangeFrom<usize>>
        + Slice<Range<usize>>
        + Slice<RangeTo<usize>>
        + InputIter<Item: NomAsChar + AsChar>
        + InputLength
        + InputTakeAtPosition<Item: NomAsChar + AsChar>
        + InputTake
        + Offset
        + Compare<&'static str>
        + AsStr<'a>,
        <&'a str as InputIter>::Item: NomAsChar + AsChar,
        <&'a str as InputTakeAtPosition>::Item: NomAsChar + AsChar,
{
    type InputIterItem = <Self as InputIter>::Item;
    type InputTakeAtPositionItem = <Self as InputTakeAtPosition>::Item;
}

#[inline]
pub fn text<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a, F: 'a, O: 'a>(i: I) -> IResult<I, Token<I>, E>
    where
        F: Fn(I) -> IResult<I, O, E>,
{
    alt((
        token(string, |i, _| Token::String(i)),
        token(num, |i, n| Token::Num(i, n)),
        token(id, |i, _| Token::Id(i)),
        token(char('('), |i, _| Token::LPar(i)),
        token(char(')'), |i, _| Token::RPar(i)),
        token(keyword, |i, kw| Token::Keyword(i, kw))
    ))(i)
}

#[inline]
pub fn string<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(input: I) -> IResult<I, (), E> {
    let (i, _) = char('\"')(input)?;
    let mut loop_i = i;
    loop {
        let (i, _) = take_while(|c: <I as nom::InputTakeAtPosition>::Item| c.is_strchar())(loop_i)?;
        if let Ok((i, _)) = char::<I, E>('\"')(i.clone()) {
            return Ok((i, ()));
        }
        let (i, _c) = preceded(char('\\'), anychar)(i)?;
        loop_i = i;
    }
}

#[inline]
pub fn num<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(input: I) -> IResult<I, Num<I>, E> {
    map(pair(sign,
             alt((
                 map(hex_num, |n| NumVariant::HexNum(n)),
                 map(dec_num, |n| NumVariant::DecNum(n)),
                 map(tag("inf"), |_| NumVariant::Inf),
                 map(preceded(tag("nan"), opt(preceded(tag(":0x"), hex_num_digits))), |pl| NumVariant::NaN(pl)),
             )),
    ), |(sign, variant)| Num { sign, variant })(input)
}

#[inline]
pub fn id<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, I, E> {
    preceded(char('$'), idchar)(i)
}

#[inline]
pub fn keyword<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, Keyword, E> {
    let (_, c) = anychar(i.clone())?;

    if !c.is_kw_prefix() {
        return Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char)));
    }

    let (i, s) = idchar(i)?;

    match KEYWORDS_HASH.get(s.as_str()) {
        Some(kw) => Ok((i, *kw)),
        _ => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char)))
    }
}


#[inline]
fn sign<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, Option<char>, E> {
    opt(alt((char('+'), char('-'))))(i)
}

#[inline]
pub fn hex_num<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, NumParts<I>, E> {
    map(preceded(tag("0x"), tuple((
        hex_num_digits,
        opt(preceded(char('.'), opt(hex_num_digits))),
        opt(preceded(alt((char('p'), char('P'))), pair(sign, dec_num_digits)))
    ))), |(p, q, e)| NumParts { p, q: q.map(|o| o.unwrap_or(vec![])), e })(i)
}


#[inline]
pub fn hex_num_digits<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, Vec<I>, E> {
    separated_nonempty_list(char('_'), hex_digit1)(i)
}

#[inline]
pub fn dec_num<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, NumParts<I>, E> {
    map(tuple((
        dec_num_digits,
        opt(preceded(char('.'), opt(dec_num_digits))),
        opt(preceded(alt((char('e'), char('E'))), pair(sign, dec_num_digits)))
    )), |(p, q, e)| NumParts { p, q: q.map(|o| o.unwrap_or(vec![])), e })(i)
}

#[inline]
pub fn dec_num_digits<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, Vec<I>, E> {
    separated_nonempty_list(char('_'), digit1)(i)
}

#[inline]
pub fn linecomment<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, (), E> {
    value((), preceded(tag(";;"), not_line_ending))(i)
}

#[inline]
pub fn blockcomment<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, (), E> {
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
    Ok((mut_i, ()))
}

#[inline]
pub fn ws<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, (), E> {
    alt((value((), char('\t')),
         value((), char('\r')),
         value((), char('\n')),
         value((), char(' ')),
         value((), linecomment),
         value((), blockcomment)
    ))(i)
}

#[inline]
pub fn token<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a, F, G, O>(parser: F, variant: G) -> impl Fn(I) -> IResult<I, Token<I>, E>
    where
        F: Fn(I) -> IResult<I, O, E>,
        G: Fn(I, O) -> Token<I>,
{
    move |input: I| {
        let (i, o) = terminated(&parser, peek(not(idchar)))(input.clone())?;
        let index = input.offset(&i);
        let tok = variant(input.slice(..index), o);
        let (i, _) = alt((value((), many0(ws)), value((), peek(alt((char('('), char(')')))))))(i)?;
        Ok((i, tok))
    }
}

#[inline]
fn idchar<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a>(i: I) -> IResult<I, I, E> {
    let (i, res) = take_while1(AsChar::is_idchar)(i)?;
    Ok((i, res))
}

pub trait AsChar {
    /// Is a valid character in an identifier?
    #[inline]
    fn is_idchar(self) -> bool;

    #[inline]
    fn is_idcontrol(self) -> bool;

    #[inline]
    fn is_kw_prefix(self) -> bool;

    /// Is a valid standalone character in a string?
    #[inline]
    fn is_strchar(self) -> bool;

    #[inline]
    fn as_hex_digit(self) -> u8;

    #[inline]
    fn as_dec_digit(self) -> u8;
}

impl AsChar for char {
    #[inline]
    fn is_idchar(self) -> bool {
        self.is_alphanumeric() || self.is_idcontrol()
    }

    #[inline]
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

    #[inline]
    fn is_kw_prefix(self) -> bool {
        match self {
            'a'..='z' => true,
            _ => false
        }
    }

    #[inline]
    fn is_strchar(self) -> bool {
        let ch = self as u32;
        ch >= 0x20 && ch != 0x22 && ch != 0x5C && ch != 0x7F
    }

    #[inline]
    fn as_hex_digit(self) -> u8 {
        match self {
            'a'..='f' => 10u8 + self as u8 - 'a' as u8,
            'A'..='F' => 10u8 + self as u8 - 'A' as u8,
            '0'..='9' => self as u8 - '0' as u8,
            _ => panic!("Not a hexadecimal digit")
        }
    }

    #[inline]
    fn as_dec_digit(self) -> u8 {
        match self {
            '0'..='9' => self as u8 - '0' as u8,
            _ => panic!("Not a decimal digit")
        }
    }
}

impl<'a> AsChar for &'a char {
    #[inline]
    fn is_idchar(self) -> bool {
        self.is_alphanumeric() || self.is_idcontrol()
    }

    #[inline]
    fn is_idcontrol(self) -> bool {
        (*self).is_idcontrol()
    }

    #[inline]
    fn is_kw_prefix(self) -> bool {
        (*self).is_kw_prefix()
    }

    #[inline]
    fn is_strchar(self) -> bool {
        (*self).is_strchar()
    }

    #[inline]
    fn as_hex_digit(self) -> u8 {
        (*self).as_hex_digit()
    }

    #[inline]
    fn as_dec_digit(self) -> u8 {
        (*self).as_dec_digit()
    }
}

#[cfg(test)]
mod test {
    use nom::error::{ErrorKind, VerboseError, convert_error};
    use super::*;

    type FastError<T> = (T, ErrorKind);

    macro_rules! parsed_to_end {
       ($p: expr, $str: expr) => {
           terminated::<&str, _, _, FastError<&str>, _, _>($p, not(anychar))($str)
       };
    }

    #[test]
    fn test_string() {
        parsed_to_end!(string, "").unwrap_err();
        parsed_to_end!(string, "\"").unwrap_err();
        parsed_to_end!(string, "\"\"").unwrap();
        parsed_to_end!(string, "\"\\\"").unwrap_err();
        parsed_to_end!(string, "\"\\t\"").unwrap();
        parsed_to_end!(string, "\"\\54\"").unwrap();
        parsed_to_end!(string, "\"\\u{\"").unwrap();
        parsed_to_end!(string, "\"\\u{\\n\"").unwrap();
    }

    #[test]
    fn test_num() {
        parsed_to_end!(num, "").unwrap_err();
        parsed_to_end!(num, "+").unwrap_err();
        parsed_to_end!(num, "0").unwrap();
        parsed_to_end!(num, "0x").unwrap_err();
        parsed_to_end!(num, "_1").unwrap_err();
        parsed_to_end!(num, "2_1").unwrap();
        parsed_to_end!(num, "-3_1").unwrap();
        parsed_to_end!(num, "-3_1.").unwrap();
        parsed_to_end!(num, "-3_1.4").unwrap();
        parsed_to_end!(num, "-3_1.e").unwrap_err();
        parsed_to_end!(num, "-3_1.e2").unwrap();
        parsed_to_end!(num, "-3_1.E2_5").unwrap();
        parsed_to_end!(num, "-3_1E2").unwrap();
        parsed_to_end!(num, "0xD_e").unwrap();
        parsed_to_end!(num, "-0xD_e.").unwrap();
        parsed_to_end!(num, "-0xD_e.p1").unwrap();
        parsed_to_end!(num, "-inf").unwrap();
        parsed_to_end!(num, "-INF").unwrap_err();
        parsed_to_end!(num, "-nan").unwrap();
        parsed_to_end!(num, "nan:0x").unwrap_err();
        parsed_to_end!(num, "nan:0xdeFf").unwrap();
    }

    #[test]
    fn test_id() {
        parsed_to_end!(id, "$").unwrap_err();
        parsed_to_end!(id, "$1>a!%").unwrap();
    }

    #[test]
    fn test_keyword() {
        parsed_to_end!(keyword, "i32").unwrap();
        parsed_to_end!(keyword, "end").unwrap();
        parsed_to_end!(keyword, "endend").unwrap_err();
        parsed_to_end!(keyword, "endend").unwrap_err();
    }


    // #[test]
    // fn test_text() {
    //     let mut parsed_to_end!(text, "(i32.const 42)//a comment\n$my_id \"string\"  ")
    //     text()
    // }
}
