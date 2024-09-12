use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char as ch;
use nom::combinator::verify;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::sequence::preceded;
use nom::IResult;

use crate::expr::parse_expr_zone;
use crate::prim::{hash_str_snippet, parse_escaped_char, parse_escaped_whitespace};
use crate::token::TokenRange;
use crate::{Span, Token};

#[derive(Debug, PartialEq)]
enum Fragment<'a> {
    Literal(&'a str, TokenRange),
    EscapedChar(char, TokenRange),
    EscapedWS(TokenRange),
    Expr(Token, TokenRange),
}

/// verify str is not empty and not contains $ or escape \
fn parse_literal(i: Span) -> IResult<Span, (&str, TokenRange), VerboseError<Span>> {
    let (remaining, s) = verify(is_not(r#"\$"#), |s: &Span| !s.fragment().is_empty())(i)?;
    Ok((remaining, (s.fragment(), TokenRange::from((i, remaining)))))
}

/// input: hash string => parse Literal, ${} and \ escape => format string
fn format_string(i: Span) -> IResult<Span, Vec<Fragment>, VerboseError<Span>> {
    let (remaining, s) = hash_str_snippet(i)?;

    fn _escape_char(i: Span) -> IResult<Span, Fragment, VerboseError<Span>> {
        let (remaining, (c, _)) = parse_escaped_char(i)?;
        Ok((
            remaining,
            Fragment::EscapedChar(c, TokenRange::from((i, remaining))),
        ))
    }
    fn _escaped_whitespace(i: Span) -> IResult<Span, Fragment, VerboseError<Span>> {
        let (remaining, _) = parse_escaped_whitespace(i)?;
        Ok((
            remaining,
            Fragment::EscapedWS(TokenRange::from((i, remaining))),
        ))
    }
    fn _expr_zone(i: Span) -> IResult<Span, Fragment, VerboseError<Span>> {
        let (remaining, (t, _)) = parse_expr_zone(i)?;
        Ok((
            remaining,
            Fragment::Expr(t, TokenRange::from((i, remaining))),
        ))
    }
    fn _literal(i: Span) -> IResult<Span, Fragment, VerboseError<Span>> {
        let (remaining, (s, _)) = parse_literal(i)?;
        Ok((
            remaining,
            Fragment::Literal(s, TokenRange::from((i, remaining))),
        ))
    }

    // there should be no remaining str
    let (_, parse_fragments) = many0(alt((
        _escape_char,
        _escaped_whitespace,
        _expr_zone,
        _literal,
    )))(s)?;
    Ok((remaining, parse_fragments))
}

/// f#" ... "#, format string
pub(crate) fn parse_fmt_string(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, fr_string) = preceded(ch('f'), format_string)(i)?;

    // merge adjacent literal string
    let mut fr_tokens =
        fr_string
            .into_iter()
            .fold(Vec::new(), |mut lit_acc: Vec<Token>, s| match s {
                Fragment::Literal(lit, r) => {
                    lit_acc.push(Token::TokenPrimString(lit.to_string(), r));
                    lit_acc.push(Token::TokenDummyOpConcat);
                    lit_acc
                }
                Fragment::EscapedChar(c, r) => {
                    lit_acc.push(Token::TokenPrimString(c.to_string(), r));
                    lit_acc.push(Token::TokenDummyOpConcat);
                    lit_acc
                }
                Fragment::Expr(expr, _) => {
                    lit_acc.push(expr);
                    lit_acc.push(Token::TokenDummyOpConcat);
                    lit_acc
                }
                _ => lit_acc,
            });
    // remove last Token::TokenOpConcat
    fr_tokens.pop();

    Ok((
        remaining,
        Token::TokenExprSequence(fr_tokens, TokenRange::from((i, remaining))),
    ))
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use super::*;
    use crate::token::{Token, TokenId};
    use crate::TokenKey;

    #[test]
    fn test_format_string() {
        let s = Span::from(r#""hello""#);
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(7..),
                vec![Fragment::Literal("hello", TokenRange::from((s, 1, 6)), ), ]
            ))
        );

        let s = Span::from(r#""hello ${name}""#);
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(15..),
                vec![
                    Fragment::Literal("hello ", TokenRange::from((s, 1, 7)), ),
                    Fragment::Expr(
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 9, 13)),
                            )],
                            TokenRange::from((s, 9, 13))
                        ),
                        TokenRange::from((s, 7, 14)),
                    ),
                ]
            ))
        );

        let s = Span::from(r#""hello ${name} \t ${age}""#);
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(25..),
                vec![
                    Fragment::Literal("hello ", TokenRange::from((s, 1, 7)), ),
                    Fragment::Expr(
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 9, 13)),
                            )],
                            TokenRange::from((s, 9, 13))
                        ),
                        TokenRange::from((s, 7, 14)),
                    ),
                    Fragment::Literal(" ", TokenRange::from((s, 14, 15)), ),
                    Fragment::EscapedChar('\t', TokenRange::from((s, 15, 17)), ),
                    Fragment::Literal(" ", TokenRange::from((s, 17, 18)), ),
                    Fragment::Expr(
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("age"),
                                TokenRange::from((s, 20, 23)),
                            )],
                            TokenRange::from((s, 20, 23))
                        ),
                        TokenRange::from((s, 18, 24)),
                    ),
                ]
            ))
        );

        let s = Span::from(
            r#""hello
world""#,
        );
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(13..),
                vec![Fragment::Literal(
                    "hello\nworld",
                    TokenRange::from((s, 1, 12))
                )]
            ))
        );

        let s = Span::from(
            r#""hello \
                world""#,
        );
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(31..),
                vec![
                    Fragment::Literal("hello ", TokenRange::from((s, 1, 7))),
                    Fragment::EscapedWS(TokenRange::from((s, 7, 25))),
                    Fragment::Literal("world", TokenRange::from((s, 25, 30)))
                ]
            ))
        );

        let s = Span::from(
            r#""hello
        ${name}""#,
        );
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(23..),
                vec![
                    Fragment::Literal("hello\n        ", TokenRange::from((s, 1, 15)), ),
                    Fragment::Expr(
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 17, 21)),
                            )],
                            TokenRange::from((s, 17, 21))
                        ),
                        TokenRange::from((s, 15, 22)),
                    ),
                ]
            ))
        );

        let s = Span::from(r##"#"hello world! ${ foo(1) }"#"##);
        assert_eq!(
            format_string(s),
            Ok((
                s.slice(28..),
                vec![
                    Fragment::Literal("hello world! ", TokenRange::from((s, 2, 15)), ),
                    Fragment::Expr(
                        Token::TokenFnCall(
                            TokenId("foo".to_string(), TokenRange::from((s, 18, 21))),
                            vec![Token::TokenPrimNumberInt(1, TokenRange::from((s, 22, 23)), )],
                            TokenRange::from((s, 18, 24)),
                        ),
                        TokenRange::from((s, 15, 26)),
                    ),
                ]
            ))
        );
    }

    #[test]
    fn test_parse_fmt_string() {
        let s = Span::from(r#"f"hello\t${name}""#);
        assert_eq!(
            parse_fmt_string(s),
            Ok((
                s.slice(17..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hello".to_string(), TokenRange::from((s, 2, 7))),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString("\t".to_string(), TokenRange::from((s, 7, 9))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 11, 15))
                            )],
                            TokenRange::from((s, 11, 15))
                        ),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );

        let s = Span::from(r#"f"hello ${ name } world""#);
        assert_eq!(
            parse_fmt_string(s),
            Ok((
                s.slice(24..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hello ".to_string(), TokenRange::from((s, 2, 8))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 11, 15))
                            )],
                            TokenRange::from((s, 11, 15))
                        ),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString(" world".to_string(), TokenRange::from((s, 17, 23))),
                    ],
                    TokenRange::from((s, 0, 24))
                )
            ))
        );

        let s = Span::from(r#"f"hello ${ name } world \n hello, you!""#);
        assert_eq!(
            parse_fmt_string(s),
            Ok((
                s.slice(39..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hello ".to_string(), TokenRange::from((s, 2, 8))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 11, 15))
                            )],
                            TokenRange::from((s, 11, 15))
                        ),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString(
                            " world ".to_string(),
                            TokenRange::from((s, 17, 24))
                        ),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString("\n".to_string(), TokenRange::from((s, 24, 26))),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString(
                            " hello, you!".to_string(),
                            TokenRange::from((s, 26, 38))
                        ),
                    ],
                    TokenRange::from((s, 0, 39))
                )
            ))
        );

        let s = Span::from(r#"f"hi ${name}\n${age}""#);
        assert_eq!(
            parse_fmt_string(s),
            Ok((
                s.slice(21..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hi ".to_string(), TokenRange::from((s, 2, 5))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("name"),
                                TokenRange::from((s, 7, 11))
                            )],
                            TokenRange::from((s, 7, 11))
                        ),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString("\n".to_string(), TokenRange::from((s, 12, 14))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("age"),
                                TokenRange::from((s, 16, 19))
                            )],
                            TokenRange::from((s, 16, 19))
                        ),
                    ],
                    TokenRange::from((s, 0, 21))
                )
            ))
        );

        let s = Span::from(r##"f#"hi ${ foo(1) + 2 }"#"##);
        assert_eq!(
            parse_fmt_string(s),
            Ok((
                s.slice(23..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hi ".to_string(), TokenRange::from((s, 3, 6))),
                        Token::TokenDummyOpConcat,
                        Token::TokenExprSequence(
                            vec![
                                Token::TokenFnCall(
                                    TokenId("foo".to_string(), TokenRange::from((s, 9, 12))),
                                    vec![Token::TokenPrimNumberInt(
                                        1,
                                        TokenRange::from((s, 13, 14)),
                                    )],
                                    TokenRange::from((s, 9, 15)),
                                ),
                                Token::TokenOpAdd(TokenRange::from((s, 16, 17))),
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 18, 19))),
                            ],
                            TokenRange::from((s, 9, 19))
                        ),
                    ],
                    TokenRange::from((s, 0, 23))
                )
            ))
        );
    }
}
