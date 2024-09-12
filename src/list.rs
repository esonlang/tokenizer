use nom::character::complete::char;
use nom::combinator::opt;
use nom::error::{context, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::{delimited, terminated, tuple};
use nom::IResult;

use crate::decorator::parse_decorators;
use crate::expr::parse_expr;
use crate::token::TokenRange;
use crate::{soc0, Span, Token, TokenDec, TokenKey};

fn tuple_decorators_and_expr(i: Span) -> IResult<Span, (Vec<TokenDec>, Token), VerboseError<Span>> {
    let (remaining, decorators) = parse_decorators(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, expr) = parse_expr(remaining)?;
    Ok((remaining, (decorators, expr)))
}

pub fn parse_lst(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, _) = context("list_head", terminated(char('['), soc0))(i)?;
    let (remaining, body) = context(
        "list_body",
        separated_list0(delimited(soc0, char(','), soc0), tuple_decorators_and_expr),
    )(remaining)?;
    let (remaining, _) =
        context("list_tail", tuple((soc0, opt(char(',')), soc0, char(']'))))(remaining)?;

    Ok((
        remaining,
        Token::TokenFrameList(
            body.into_iter()
                .enumerate()
                .map(|(i, v)| (TokenKey::DummySn(i), v.0, v.1))
                .collect(),
            TokenRange::from((i, remaining)),
        ),
    ))
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use super::*;
    use crate::token::{Token, TokenId, TokenRange};

    #[test]
    fn test_tuple_decorators_and_expr() {
        let s = Span::from(r#"@decorator1 @decorator2 1"#);
        assert_eq!(
            tuple_decorators_and_expr(s),
            Ok((
                s.slice(25..),
                (
                    vec![
                        TokenDec(
                            TokenId(String::from("decorator1"), TokenRange::from((s, 1, 11))),
                            vec![],
                            TokenRange::from((s, 0, 11))
                        ),
                        TokenDec(
                            TokenId(String::from("decorator2"), TokenRange::from((s, 13, 23))),
                            vec![],
                            TokenRange::from((s, 12, 23))
                        )
                    ],
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 24, 25)))
                )
            ))
        );
    }

    #[test]
    fn test_parse_int_lst() {
        let s = Span::from(r#"[]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(2..),
                Token::TokenFrameList(vec![], TokenRange::from((s, 0, 2)))
            ))
        );

        let s = Span::from(r#"[ ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(3..),
                Token::TokenFrameList(vec![], TokenRange::from((s, 0, 3)))
            ))
        );

        let s = Span::from(r#"[1]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(3..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                    )],
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"[1,]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(4..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                    )],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"[1, ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(5..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"[1,2]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(5..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 3, 4)))
                        )
                    ],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"[1,2,]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(6..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 3, 4)))
                        )
                    ],
                    TokenRange::from((s, 0, 6))
                )
            ))
        );

        let s = Span::from(r#"[1,2, ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 3, 4)))
                        )
                    ],
                    TokenRange::from((s, 0, 7))
                )
            ))
        );
    }

    #[test]
    fn test_parse_string_lst() {
        let s = Span::from(r#"["a"]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(5..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"["a",]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(6..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                    )],
                    TokenRange::from((s, 0, 6))
                )
            ))
        );

        let s = Span::from(r#"["a", ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![],
                        Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                    )],
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from(r#"["a","b"]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(9..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimString("b".to_string(), TokenRange::from((s, 5, 8)))
                        )
                    ],
                    TokenRange::from((s, 0, 9))
                )
            ))
        );

        let s = Span::from(r#"["a","b",]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(10..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimString("b".to_string(), TokenRange::from((s, 5, 8)))
                        )
                    ],
                    TokenRange::from((s, 0, 10))
                )
            ))
        );
    }

    #[test]
    fn test_mix() {
        let s = Span::from(r#"[1, "a"]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(8..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 4, 7)))
                        )
                    ],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );

        let s = Span::from(r#"[1, "a", ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(10..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 4, 7)))
                        )
                    ],
                    TokenRange::from((s, 0, 10))
                )
            ))
        );

        let s = Span::from(r#"[1, "a", 2]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(11..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 4, 7)))
                        ),
                        (
                            TokenKey::DummySn(2),
                            vec![],
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 9, 10)))
                        )
                    ],
                    TokenRange::from((s, 0, 11))
                )
            ))
        );

        let s = Span::from(
            r#"["a", // comment1
            1, // comment2
            ]"#,
        );
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(58..),
                Token::TokenFrameList(
                    vec![
                        (
                            TokenKey::DummySn(0),
                            vec![],
                            Token::TokenPrimString("a".to_string(), TokenRange::from((s, 1, 4)))
                        ),
                        (
                            TokenKey::DummySn(1),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 30, 31)))
                        )
                    ],
                    TokenRange::from((s, 0, 58))
                )
            ))
        );
    }

    #[test]
    fn test_multi_line() {
        let _ = parse_lst(Span::from(r#"[]"#)).is_ok();
        let _ = parse_lst(Span::from(
            r#"[
        ]"#,
        ))
            .is_ok();
        let _ = parse_lst(Span::from(
            r#"[
            1
        ]"#,
        ))
            .is_ok();
        let _ = parse_lst(Span::from(
            r#"[
            "str"
        ]"#,
        ))
            .is_ok();
        let _ = parse_lst(Span::from(
            r#"[
            "str",
            1
        ]"#,
        ))
            .is_ok();
    }

    #[test]
    fn test_list_expr() {
        let s = Span::from(r#"[ {"f": "o"}]"#);
        assert!(parse_lst(s).is_ok());
    }

    #[test]
    fn test_list_expr_with_decorator() {
        let s = Span::from(r#"[ @decorator1 true ]"#);
        assert_eq!(
            parse_lst(s),
            Ok((
                s.slice(20..),
                Token::TokenFrameList(
                    vec![(
                        TokenKey::DummySn(0),
                        vec![TokenDec(
                            TokenId(String::from("decorator1"), TokenRange::from((s, 3, 13))),
                            vec![],
                            TokenRange::from((s, 2, 13))
                        )],
                        Token::TokenPrimBoolean(true, TokenRange::from((s, 14, 18)))
                    )],
                    TokenRange::from((s, 0, 20))
                )
            ))
        );
    }
}
