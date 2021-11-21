#[macro_use]
#[cfg(test)]
mod test {
    use nom::error::ErrorKind;

    use crate::format::input::WithParseError;

    
    impl<'a> WithParseError for &'a str {
        type Error = (&'a str, ErrorKind);
    }

    macro_rules! consumed {
        ($p: expr, $str: expr) => {
            nom::sequence::terminated($p, nom::combinator::not(nom::bytes::complete::take(1u8)))($str).map(|(_, r)| r)
        };
    }
}

mod lexer;

#[macro_use]
pub mod parser;
mod printer;
