#![macro_use]

use std::ops::{RangeFrom};

use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1, take_while_m_n};
use nom::character::complete::{anychar, char, not_line_ending};
use nom::combinator::{map, map_res, not, opt, peek, recognize, value};

use nom::error::{ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};

use crate::format::text::lexer::{AsChar, Token, NumVariant, hex_num, NumParts, dec_num, AsStr, hex_num_digits};
use crate::format::text::lexer::num;
use crate::format::input::{Input, pred};
use nom::lib::std::option::NoneError;
use crate::format::input::satisfies;

use crate::format::text::lexer::LexerInput;

use std::convert::TryFrom;

#[inline]
pub fn parsed_string<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a, Item>(i: I) -> IResult<I, String, E> {
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

#[inline]
pub fn parsed_uxx<'a, I: 'a + LexerInput<'a>, E: ParseError<I> + 'a, Out: num::Unsigned>(i: I) -> IResult<I, Out, E> {
    alt((
        map_res(hex_num, |parts| { parts.try_parse_integral::<Out>(16) }),
        map_res(dec_num, |parts| { parts.try_parse_integral::<Out>(10) })
    ))(i)
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
    fn test_parsed_string() {
        assert_eq!(
            parsed_string::<_, FastError<&str>, char>("\"Lorem ipsum dolor sit amet\""),
            Ok(("", "Lorem ipsum dolor sit amet".to_owned()))
        );
        parsed_string::<_, FastError<&str>, char>("\"Lorem").unwrap_err();
        assert_eq!(
            parsed_string::<_, FastError<&str>, char>(
                "\"ASCII: \\42, TAB: \\t, CRLF: \\r\\n, Unicode: \\u{05d0}\""
            ),
            Ok(("", "ASCII: B, TAB: \t, CRLF: \r\n, Unicode: א".to_owned()))
        );
        parsed_string::<_, FastError<&str>, char>("\"\\uinvalid\"").unwrap_err();
        parsed_string::<_, FastError<&str>, char>("\"\\q\"").unwrap_err();
    }



    // #[test]
    // fn test_text() {
    //     let mut parsed_to_end!(text, "(i32.const 42)//a comment\n$my_id \"string\"  ")
    //     text()
    // }
}
