use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, char, multispace0};
use nom::combinator::{not, opt};
use nom::error::VerboseError;
use nom::multi::many0;
use nom::sequence::terminated;
use nom::IResult;

use crate::token::TokenRange;
use crate::{Span, Token};

fn sign_negation(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, _) = char('!')(i)?;
    Ok((remaining, Token::TokenOpNot(TokenRange::from((i, 1)))))
}

fn literal_true(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, _) = terminated(tag("true"), not(alphanumeric1))(i)?;
    Ok((
        remaining,
        Token::TokenPrimBoolean(true, TokenRange::from((i, 4))),
    ))
}

fn literal_false(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, _) = terminated(tag("false"), not(alphanumeric1))(i)?;
    Ok((
        remaining,
        Token::TokenPrimBoolean(false, TokenRange::from((i, 5))),
    ))
}

pub(crate) fn parse_boolean(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, mut ts) = many0(terminated(sign_negation, opt(multispace0)))(i)?;
    let (remaining, boolean) = alt((literal_true, literal_false))(remaining)?;
    if ts.len() > 0 {
        ts.push(boolean);
        return Ok((
            remaining,
            Token::TokenExprSequence(ts, TokenRange::from((i, remaining))),
        ));
    }
    return Ok((remaining, boolean));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenPosition;
    use nom::Slice;

    #[test]
    fn test_literal() {
        let s = Span::from(r#"!"#);
        assert_eq!(
            sign_negation(s),
            Ok((
                s.slice(1..),
                Token::TokenOpNot(TokenRange(
                    TokenPosition { line: 1, column: 1 },
                    TokenPosition { line: 1, column: 2 }
                ))
            ))
        );

        let s = Span::from(r#"true"#);
        assert_eq!(
            literal_true(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimBoolean(
                    true,
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition { line: 1, column: 5 }
                    )
                )
            ))
        );

        let s = Span::from(r#"false"#);
        assert_eq!(
            literal_false(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimBoolean(
                    false,
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition { line: 1, column: 6 }
                    )
                )
            ))
        );
    }

    #[test]
    fn test_parse_boolean() {
        assert!(parse_boolean(Span::from("true ")).is_ok());
        assert!(parse_boolean(Span::from("false ")).is_ok());
        assert!(parse_boolean(Span::from("true1")).is_err());
        assert!(parse_boolean(Span::from("falsee")).is_err());
        assert!(parse_boolean(Span::from("false?")).is_ok());
        assert!(parse_boolean(Span::from("!true ")).is_ok());
        assert!(parse_boolean(Span::from("!false ")).is_ok());
        assert!(parse_boolean(Span::from("!\ttrue ")).is_ok());
        assert!(parse_boolean(Span::from("! false ")).is_ok());

        let s = Span::from(r#"true"#);
        assert_eq!(
            parse_boolean(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimBoolean(
                    true,
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition { line: 1, column: 5 }
                    )
                )
            ))
        );

        let s = Span::from(r#"false"#);
        assert_eq!(
            parse_boolean(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimBoolean(
                    false,
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition { line: 1, column: 6 }
                    )
                )
            ))
        );

        let s = Span::from(r#"!true"#);
        assert_eq!(
            parse_boolean(s),
            Ok((
                s.slice(5..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenOpNot(TokenRange(
                            TokenPosition { line: 1, column: 1 },
                            TokenPosition { line: 1, column: 2 }
                        )),
                        Token::TokenPrimBoolean(
                            true,
                            TokenRange(
                                TokenPosition { line: 1, column: 2 },
                                TokenPosition { line: 1, column: 6 }
                            )
                        )
                    ],
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition { line: 1, column: 6 }
                    )
                )
            ))
        );

        let s = Span::from("!\t!! true");
        assert_eq!(
            parse_boolean(s),
            Ok((
                s.slice(9..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenOpNot(TokenRange(
                            TokenPosition { line: 1, column: 1 },
                            TokenPosition { line: 1, column: 2 }
                        )),
                        Token::TokenOpNot(TokenRange(
                            TokenPosition { line: 1, column: 3 },
                            TokenPosition { line: 1, column: 4 }
                        )),
                        Token::TokenOpNot(TokenRange(
                            TokenPosition { line: 1, column: 4 },
                            TokenPosition { line: 1, column: 5 }
                        )),
                        Token::TokenPrimBoolean(
                            true,
                            TokenRange(
                                TokenPosition { line: 1, column: 6 },
                                TokenPosition {
                                    line: 1,
                                    column: 10
                                }
                            )
                        )
                    ],
                    TokenRange(
                        TokenPosition { line: 1, column: 1 },
                        TokenPosition {
                            line: 1,
                            column: 10
                        }
                    )
                )
            ))
        );
    }
}
