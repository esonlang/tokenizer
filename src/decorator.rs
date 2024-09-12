// @some
// @some1("value")
// @some2("value", "value2")
// @some3([1, 2, 5])
// @some4({
//    "key": "value",
// })
// @some5(1 + 2)
// @some6(f(3) ? 1 : 2)
// @some7((1 + 2) * 3)

use nom::bytes::complete::{tag, take_while};
use nom::combinator::opt;
use nom::error::VerboseError;
use nom::multi::{many1, separated_list0};
use nom::sequence::{delimited, preceded};

use crate::expr::parse_expr;
use crate::id::id;
use crate::token::{TokenDec, TokenRange};
use crate::{soc0, Span};

fn sp_without_br0(i: Span) -> nom::IResult<Span, Span, VerboseError<Span>> {
    let chars = " \t\r";
    take_while(move |c| chars.contains(c))(i)
}

fn decorator(i: Span) -> nom::IResult<Span, TokenDec, VerboseError<Span>> {
    let (remaining, _) = tag("@")(i)?;
    let (remaining, id) = id(remaining)?;
    let (remaining, value) = opt(preceded(
        preceded(sp_without_br0, tag("(")),
        delimited(
            soc0,
            separated_list0(delimited(soc0, tag(","), soc0), parse_expr), // expr_unit
            preceded(soc0, tag(")")),
        ),
    ))(remaining)?;

    Ok((
        remaining,
        TokenDec(
            id,
            value.unwrap_or_else(Vec::new),
            TokenRange::from((i, remaining)),
        ),
    ))
}

pub(crate) fn parse_decorators(i: Span) -> nom::IResult<Span, Vec<TokenDec>, VerboseError<Span>> {
    let (remaining, decorators) = opt(many1(preceded(soc0, decorator)))(i)?;
    Ok((remaining, decorators.unwrap_or_else(Vec::new)))
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use crate::token::{TokenId, TokenPosition};
    use crate::Token;

    use super::*;

    #[test]
    fn test_none() {
        let s = r#"{ data: 1 }"#;
        assert_eq!(parse_decorators(Span::from(s)), Ok((Span::from(s), vec![])));
    }

    #[test]
    fn test_sp_without_br0() {
        let s = Span::from("   ");
        assert_eq!(sp_without_br0(s), Ok((s.slice(3..), s.slice(0..3))));

        let s = Span::from(" \t \r  ");
        assert_eq!(sp_without_br0(s), Ok((s.slice(6..), s.slice(0..6))));

        let s = Span::from("");
        assert_eq!(sp_without_br0(s), Ok((s.slice(0..), s.slice(0..0))));

        let s = Span::from("   \n");
        assert_eq!(sp_without_br0(s), Ok((s.slice(3..), s.slice(0..3))));

        let s = Span::from("   \n  ");
        assert_eq!(sp_without_br0(s), Ok((s.slice(3..), s.slice(0..3))));
    }

    #[test]
    fn test_decorator_after_comment() {
        let s = Span::from(
            r###"
            // foo decorator
            @foo
        "###,
        );
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(46..),
                vec![TokenDec(
                    TokenId(String::from("foo"), TokenRange::from((s, 43, 46))),
                    vec![],
                    TokenRange::from((s, 42, 46))
                )]
            ))
        );
    }

    #[test]
    fn test_decorator() {
        let s = Span::from(r#"@DEF\n"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(4..),
                TokenDec(
                    TokenId("DEF".to_string(), TokenRange::from((s, 1, 4))),
                    vec![],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"@EFG   "#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(4..),
                TokenDec(
                    TokenId("EFG".to_string(), TokenRange::from((s, 1, 4))),
                    vec![],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"@some (1)"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(9..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8)))],
                    TokenRange::from((s, 0, 9))
                ),
            ))
        );

        let s = Span::from(
            r#"@some (1,
         2)"#,
        );
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(21..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 19, 20)))
                    ],
                    TokenRange::from((s, 0, 21))
                )
            ))
        );

        let s = Span::from(r#"@some()"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(7..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![],
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from(r#"@some("foo", "bar")"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(19..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![
                        Token::TokenPrimString("foo".to_string(), TokenRange::from((s, 6, 11))),
                        Token::TokenPrimString("bar".to_string(), TokenRange::from((s, 13, 18)))
                    ],
                    TokenRange::from((s, 0, 19))
                )
            ))
        );

        let s = Span::from(
            r#"@some ( // comment for some
            "foo", // comment for foo
            1      // comment for 1
        )"#,
        );
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(111..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![
                        Token::TokenPrimString("foo".to_string(), TokenRange::from((s, 40, 45))),
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 78, 79)))
                    ],
                    TokenRange::from((s, 0, 111))
                )
            ))
        );
    }

    #[test]
    fn test_decorator_with_expr() {
        let s = Span::from(r#"@some(3)"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(8..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![Token::TokenPrimNumberInt(3, TokenRange::from((s, 6, 7))), ],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );

        let s = Span::from(r#"@some(1 + 2)"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(12..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 6, 7))),
                            Token::TokenOpAdd(TokenRange(
                                TokenPosition { line: 1, column: 9 },
                                TokenPosition {
                                    line: 1,
                                    column: 10,
                                },
                            ), ),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 10, 11))),
                        ],
                        TokenRange::from((s, 6, 11))
                    )],
                    TokenRange::from((s, 0, 12))
                )
            ))
        );

        let s = Span::from(r#"@some(1 + 2, 3)"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(15..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![
                        Token::TokenExprSequence(
                            vec![
                                Token::TokenPrimNumberInt(1, TokenRange::from((s, 6, 7))),
                                Token::TokenOpAdd(TokenRange(
                                    TokenPosition { line: 1, column: 9 },
                                    TokenPosition {
                                        line: 1,
                                        column: 10,
                                    },
                                ), ),
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 10, 11))),
                            ],
                            TokenRange::from((s, 6, 11))
                        ),
                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 13, 14))),
                    ],
                    TokenRange::from((s, 0, 15))
                )
            ))
        );

        let s = Span::from(r#"@some(true? 1: 2)"#);
        assert_eq!(
            decorator(s),
            Ok((
                s.slice(17..),
                TokenDec(
                    TokenId("some".to_string(), TokenRange::from((s, 1, 5))),
                    vec![Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimBoolean(true, TokenRange::from((s, 6, 10))),
                            Token::TokenOpQ(TokenRange(
                                TokenPosition {
                                    line: 1,
                                    column: 11
                                },
                                TokenPosition {
                                    line: 1,
                                    column: 12,
                                },
                            ), ),
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 12, 13))),
                            Token::TokenOpColon(TokenRange(
                                TokenPosition {
                                    line: 1,
                                    column: 14
                                },
                                TokenPosition {
                                    line: 1,
                                    column: 15,
                                },
                            ), ),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 15, 16))),
                        ],
                        TokenRange::from((s, 6, 16))
                    )],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );
    }

    #[test]
    fn test_parse_decorators() {
        let s = Span::from(r#"@world"#);
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(6..),
                vec![TokenDec(
                    TokenId("world".to_string(), TokenRange::from((s, 1, 6))),
                    vec![],
                    TokenRange::from((s, 0, 6))
                )]
            ))
        );

        let s = Span::from(r#"@world@hello"#);
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(12..),
                vec![
                    TokenDec(
                        TokenId("world".to_string(), TokenRange::from((s, 1, 6))),
                        vec![],
                        TokenRange::from((s, 0, 6))
                    ),
                    TokenDec(
                        TokenId("hello".to_string(), TokenRange::from((s, 7, 12))),
                        vec![],
                        TokenRange::from((s, 6, 12))
                    )
                ],
            ))
        );

        let s = Span::from(
            r#"@world
            @hello"#,
        );
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(25..),
                vec![
                    TokenDec(
                        TokenId("world".to_string(), TokenRange::from((s, 1, 6))),
                        vec![],
                        TokenRange::from((s, 0, 6))
                    ),
                    TokenDec(
                        TokenId("hello".to_string(), TokenRange::from((s, 20, 25))),
                        vec![],
                        TokenRange::from((s, 19, 25))
                    )
                ],
            ))
        );

        let s = Span::from(r#"@DEF\n"#);
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(4..),
                vec![TokenDec(
                    TokenId("DEF".to_string(), TokenRange::from((s, 1, 4))),
                    vec![],
                    TokenRange::from((s, 0, 4))
                )]
            ))
        );

        let s = Span::from(
            r##"
            @some
            @some1("value")
            @some2("value", "value2")
        "##,
        );
        assert_eq!(
            parse_decorators(s),
            Ok((
                s.slice(84..),
                vec![
                    TokenDec(
                        TokenId("some".to_string(), TokenRange::from((s, 14, 18))),
                        vec![],
                        TokenRange::from((s, 13, 18))
                    ),
                    TokenDec(
                        TokenId("some1".to_string(), TokenRange::from((s, 32, 37))),
                        vec![Token::TokenPrimString(
                            "value".to_string(),
                            TokenRange::from((s, 38, 45))
                        )],
                        TokenRange::from((s, 31, 46))
                    ),
                    TokenDec(
                        TokenId("some2".to_string(), TokenRange::from((s, 60, 65))),
                        vec![
                            Token::TokenPrimString(
                                "value".to_string(),
                                TokenRange::from((s, 66, 73))
                            ),
                            Token::TokenPrimString(
                                "value2".to_string(),
                                TokenRange::from((s, 75, 83))
                            ),
                        ],
                        TokenRange::from((s, 59, 84))
                    ),
                ]
            ))
        );
    }
}
