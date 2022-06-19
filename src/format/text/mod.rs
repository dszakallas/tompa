#![macro_use]

macro_rules! token_type {
    ($i:expr, $case: ident ($out:pat $(, $rest: pat)*)) => {{
        use crate::format::text::lexer::Token;
        if let Some(t) = $i.iter_elements().next() {
            if let Token::$case($out, ) = t {
                Some(($i.slice(1..), $out))
            } else {
                None
            }
        } else {
            None
        }
    }}
}

#[macro_use]
#[cfg(test)]
mod test {
    use crate::format::input::{ParseError, WithParseError};
    use crate::format::text::error::{VerboseError};
    use nom::error::{ErrorKind as NomErrorKind, VerboseError as NomVerboseError};

    impl<'a> WithParseError<NomErrorKind> for &'a str {
        type Error = NomVerboseError<&'a str>;
    }

    macro_rules! consumed {
        ($p: expr, $str: expr) => {
            nom::sequence::terminated($p, nom::combinator::not(nom::bytes::complete::take(1u8)))($str).map(|(_, r)| r)
        };
    }
}

mod error;
mod instructions;
mod keywords;
mod lexer;
pub(crate) mod parser;

