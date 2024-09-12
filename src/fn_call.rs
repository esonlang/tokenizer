use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::error::{context, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded};
use nom::IResult;

use crate::expr::parse_expr;
use crate::id::id;
use crate::token::TokenRange;
use crate::{Span, Token};

pub(crate) fn parse_fn_call(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    fn _fn_call(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
        let (remaining, name) = id(i)?;
        let (remaining, _) = delimited(multispace0, tag("("), multispace0)(remaining)?;
        let (remaining, args) = delimited(
            multispace0,
            separated_list0(delimited(multispace0, tag(","), multispace0), parse_expr),
            preceded(multispace0, tag(")")),
        )(remaining)?;
        Ok((
            remaining,
            Token::TokenFnCall(name, args, TokenRange::from((i, remaining))),
        ))
    }

    context("parse_fn_call", _fn_call)(i)
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use crate::token::TokenId;
    use crate::Token::TokenFnCall;
    use crate::TokenKey;

    #[test]
    fn test_parse_fn_call() {
        use super::*;

        let s = Span::from(r#"f()"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(3..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![],
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"f( )"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(4..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"f ( )"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(5..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"f ( ) "#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(5..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"f(1)"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(4..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![Token::TokenPrimNumberInt(1, TokenRange::from((s, 2, 3)))],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"f(1, 2)"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(7..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 2, 3))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                    ],
                    TokenRange::from((s, 0, 7))
                ),
            ))
        );

        let s = Span::from(r#"f(1, "hello")"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(13..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 2, 3))),
                        Token::TokenPrimString(String::from("hello"), TokenRange::from((s, 5, 12)))
                    ],
                    TokenRange::from((s, 0, 13))
                ),
            ))
        );

        let s = Span::from(r#"fn_call(hello, world)"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(21..),
                TokenFnCall(
                    TokenId(String::from("fn_call"), TokenRange::from((s, 0, 7))),
                    vec![
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("hello"),
                                TokenRange::from((s, 8, 13))
                            ), ],
                            TokenRange::from((s, 8, 13))
                        ),
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("world"),
                                TokenRange::from((s, 15, 20))
                            ), ],
                            TokenRange::from((s, 15, 20))
                        ),
                    ],
                    TokenRange::from((s, 0, 21))
                )
            ))
        );

        let s = Span::from(r#"f1(f2())"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(8..),
                TokenFnCall(
                    TokenId(String::from("f1"), TokenRange::from((s, 0, 2))),
                    vec![TokenFnCall(
                        TokenId(String::from("f2"), TokenRange::from((s, 3, 5))),
                        vec![],
                        TokenRange::from((s, 3, 7))
                    )],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );

        let s = Span::from(r#"f(f1(), f2(1, 2), 3)"#);
        assert_eq!(
            parse_fn_call(s),
            Ok((
                s.slice(20..),
                TokenFnCall(
                    TokenId(String::from("f"), TokenRange::from((s, 0, 1))),
                    vec![
                        TokenFnCall(
                            TokenId(String::from("f1"), TokenRange::from((s, 2, 4))),
                            vec![],
                            TokenRange::from((s, 2, 6))
                        ),
                        TokenFnCall(
                            TokenId(String::from("f2"), TokenRange::from((s, 8, 10))),
                            vec![
                                Token::TokenPrimNumberInt(1, TokenRange::from((s, 11, 12))),
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 14, 15)))
                            ],
                            TokenRange::from((s, 8, 16))
                        ),
                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 18, 19)))
                    ],
                    TokenRange::from((s, 0, 20))
                )
            ))
        );
    }
}
