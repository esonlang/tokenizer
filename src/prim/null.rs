use nom::bytes::complete::tag;
use nom::character::complete::alphanumeric1;
use nom::combinator::{map, not};
use nom::error::VerboseError;
use nom::sequence::terminated;
use nom::IResult;

use crate::token::{Token, TokenRange};
use crate::Span;

pub(crate) fn parse_null(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    map(terminated(tag("null"), not(alphanumeric1)), move |_| {
        Token::TokenPrimNull(TokenRange::from((i, 4)))
    })(i)
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use crate::token::TokenPosition;

    use super::*;

    #[test]
    fn test_null() {
        assert!(parse_null(Span::from("nulll")).is_err());
        assert!(parse_null(Span::from("null ")).is_ok());
        assert!(parse_null(Span::from("1null")).is_err());
        assert!(parse_null(Span::from("1null")).is_err());
        let s = Span::from(r#"null"#);
        assert_eq!(
            parse_null(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNull(TokenRange(
                    TokenPosition { line: 1, column: 1 },
                    TokenPosition { line: 1, column: 5 },
                ))
            ))
        );

        let s = Span::from(r#"null "#);
        assert_eq!(
            parse_null(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNull(TokenRange(
                    TokenPosition { line: 1, column: 1 },
                    TokenPosition { line: 1, column: 5 },
                ))
            ))
        );
    }
}
