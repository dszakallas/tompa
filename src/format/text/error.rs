use nom::error::{ErrorKind as NomErrorKind, ParseError as NomParseError};
use crate::format::input::{ParseError, WithParseError, WithWrappedStream};
use crate::format::text::lexer::{CharStream, Token};
use crate::format::text::parser::{Parser, ParserError, TokenStream};


#[derive(Clone, Debug, PartialEq)]
/// error context for `VerboseError`
pub enum VerboseErrorKind<E: Sized> {
    /// static string added by the `context` function
    Context(&'static str),
    /// indicates which character was expected by the `char` function
    Char(char),
    /// error kind given by various nom parsers
    Nom(NomErrorKind),
    /// error kind given by various parsers
    Text(Parser<E>)
}

#[derive(Clone, Debug, PartialEq)]
pub struct VerboseError<I, E> {
    errors: Vec<(I, VerboseErrorKind<E>)>
}

impl<I, E> NomParseError<I> for VerboseError<I, E>
{
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        VerboseError {
            errors: vec![(input, VerboseErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: NomErrorKind, mut other: Self) -> Self {
        other.errors.push((input, VerboseErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        VerboseError {
            errors: vec![(input, VerboseErrorKind::Char(c))],
        }
    }

    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, VerboseErrorKind::Context(ctx)));
        other
    }
}

impl<I, E> ParseError<I, Parser<E>> for VerboseError<I, E> {
fn from_error_kind(input: I, kind: Parser<E>) -> Self {
        VerboseError {
            errors: vec![(input, VerboseErrorKind::Text(kind))],
        }
    }

    fn append(input: I, kind: Parser<E>, mut other: Self) -> Self {
        other.errors.push((input, VerboseErrorKind::Text(kind)));
        other
    }
}


// pub fn convert_error(input: &TokenStream, e: VerboseError<&TokenStream>) -> crate::lib::std::string::String {
//     use crate::lib::std::fmt::Write;
//     use crate::traits::Offset;

//     let mut result = crate::lib::std::string::String::new();

//     for (i, (substring, kind)) in e.errors.iter().enumerate() {
//         let offset = input.offset(substring);

//         if input.is_empty() {
//             match kind {
//                 VerboseErrorKind::Char(c) => write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c),
//                 VerboseErrorKind::Context(s) => write!(&mut result, "{}: in {}, got empty input\n\n", i, s),
//                 VerboseErrorKind::Nom(e) => write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e),
//             }
//         } else {
//             let prefix = &input.as_bytes()[..offset];

//             // Count the number of newlines in the first `offset` bytes of input
//             let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

//             // Find the line that includes the subslice:
//             // Find the *last* newline before the substring starts
//             let line_begin = prefix.iter().rev().position(|&b| b == b'\n').map(|pos| offset - pos).unwrap_or(0);

//             // Find the full line after that newline
//             let line = input[line_begin..].lines().next().unwrap_or(&input[line_begin..]).trim_end();

//             // The (1-indexed) column number is the offset of our substring into that line
//             let column_number = line.offset(substring) + 1;

//             match kind {
//                 VerboseErrorKind::Char(c) => if let Some(actual) = substring.chars().next() {
//                     write!(
//                         &mut result,
//                         "{i}: at line {line_number}:\n\
//                {line}\n\
//                {caret:>column$}\n\
//                expected '{expected}', found {actual}\n\n",
//                         i = i,
//                         line_number = line_number,
//                         line = line,
//                         caret = '^',
//                         column = column_number,
//                         expected = c,
//                         actual = actual,
//                     )
//                 } else {
//                     write!(
//                         &mut result,
//                         "{i}: at line {line_number}:\n\
//                {line}\n\
//                {caret:>column$}\n\
//                expected '{expected}', got end of input\n\n",
//                         i = i,
//                         line_number = line_number,
//                         line = line,
//                         caret = '^',
//                         column = column_number,
//                         expected = c,
//                     )
//                 },
//                 VerboseErrorKind::Context(s) => write!(
//                     &mut result,
//                     "{i}: at line {line_number}, in {context}:\n\
//              {line}\n\
//              {caret:>column$}\n\n",
//                     i = i,
//                     line_number = line_number,
//                     context = s,
//                     line = line,
//                     caret = '^',
//                     column = column_number,
//                 ),
//                 VerboseErrorKind::Nom(e) => write!(
//                     &mut result,
//                     "{i}: at line {line_number}, in {nom_err:?}:\n\
//              {line}\n\
//              {caret:>column$}\n\n",
//                     i = i,
//                     line_number = line_number,
//                     nom_err = e,
//                     line = line,
//                     caret = '^',
//                     column = column_number,
//                 ),
//             }
//         }
//             // Because `write!` to a `String` is infallible, this `unwrap` is fine.
//             .unwrap();
//     }

//     result
// }


