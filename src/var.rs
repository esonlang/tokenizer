use crate::id::id;
use crate::prim::parse_string;
use crate::{Span, Token, TokenKey, TokenRange};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, multispace0};
use nom::combinator::{map, map_res, opt};
use nom::error::{context, VerboseError};
use nom::multi::many1;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::IResult;

// eg. 1, 2, ..
pub(crate) fn digit1_k(i: Span) -> IResult<Span, TokenKey, VerboseError<Span>> {
    let (remaining, n) = map_res(digit1, |s: Span| s.fragment().parse::<usize>())(i)?;
    Ok((remaining, TokenKey::Sn(n, TokenRange::from((i, remaining)))))
}

// eg. name, stat ...
fn head_k(i: Span) -> IResult<Span, TokenKey, VerboseError<Span>> {
    context(
        "head_k",
        map(preceded(multispace0, id), |id| TokenKey::String(id.0, id.1)),
    )(i)
}

// .ele | .0 | [0] | ["ele"]
pub(crate) fn rest_k(i: Span) -> IResult<Span, Vec<TokenKey>, VerboseError<Span>> {
    let (remaining, elements) = many1(preceded(
        multispace0,
        alt((
            preceded(terminated(tag("."), multispace0), digit1_k),
            preceded(
                terminated(tag("."), multispace0),
                map(id, |s| TokenKey::String(s.0, s.1)),
            ),
            delimited(
                terminated(tag("["), multispace0),
                digit1_k,
                preceded(multispace0, tag("]")),
            ),
            delimited(
                terminated(tag("["), multispace0),
                map(parse_string, |s| match s {
                    Token::TokenPrimString(s, r) => TokenKey::String(s, r),
                    _ => unreachable!(),
                }),
                preceded(multispace0, tag("]")),
            ),
        )),
    ))(i)?;
    Ok((remaining, elements))
}

// head_var + parse_key
pub(crate) fn parse_var(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, v) = context(
        "parse_var",
        map(pair(head_k, opt(rest_k)), |(head, keys)| {
            let mut v = vec![head];
            if let Some(k) = keys {
                v.extend(k);
            }
            v
        }),
    )(i)?;
    Ok((
        remaining,
        Token::TokenVar(v, TokenRange::from((i, remaining))),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Slice;

    #[test]
    fn test_rest_k() {
        let s = Span::from(r#".ele"#);
        assert_eq!(
            rest_k(s).unwrap().1,
            vec![TokenKey::String(
                String::from("ele"),
                TokenRange::from((s, 1, 4))
            )]
        );

        let s = Span::from(r#".0"#);
        assert_eq!(
            rest_k(s).unwrap().1,
            vec![TokenKey::Sn(0, TokenRange::from((s, 1, 2)))]
        );

        let s = Span::from(r#"[0]"#);
        assert_eq!(
            rest_k(s).unwrap().1,
            vec![TokenKey::Sn(0, TokenRange::from((s, 1, 2)))]
        );

        let s = Span::from(r#"["ele"]"#);
        assert_eq!(
            rest_k(s).unwrap().1,
            vec![TokenKey::String(
                String::from("ele"),
                TokenRange::from((s, 1, 6))
            )]
        );
    }

    #[test]
    fn test_multi_layer_var() {
        let s = Span::from("level1.level2");
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(13..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::String(String::from("level2"), TokenRange::from((s, 7, 13)))
                    ],
                    TokenRange::from((s, 0, 13))
                )
            ))
        );

        let s = Span::from("level1. level2");
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(14..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::String(String::from("level2"), TokenRange::from((s, 8, 14)))
                    ],
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"level1["level2"].level3"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(23..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::String(String::from("level2"), TokenRange::from((s, 7, 15))),
                        TokenKey::String(String::from("level3"), TokenRange::from((s, 17, 23)))
                    ],
                    TokenRange::from((s, 0, 23))
                )
            ))
        );

        let s = Span::from(r#"level1 ["level2"].level3"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(24..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::String(String::from("level2"), TokenRange::from((s, 8, 16))),
                        TokenKey::String(String::from("level3"), TokenRange::from((s, 18, 24)))
                    ],
                    TokenRange::from((s, 0, 24))
                )
            ))
        );

        let s = Span::from(r#"level1 ["level2" ].level3"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(25..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::String(String::from("level2"), TokenRange::from((s, 8, 16))),
                        TokenKey::String(String::from("level3"), TokenRange::from((s, 19, 25)))
                    ],
                    TokenRange::from((s, 0, 25))
                )
            ))
        );

        let s = Span::from("level1[0].level3[1]");
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(19..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::Sn(0, TokenRange::from((s, 7, 8))),
                        TokenKey::String(String::from("level3"), TokenRange::from((s, 10, 16))),
                        TokenKey::Sn(1, TokenRange::from((s, 17, 18)))
                    ],
                    TokenRange::from((s, 0, 19))
                )
            ))
        );

        let s = Span::from("level1[0] .level3[1]");
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(20..),
                Token::TokenVar(
                    vec![
                        TokenKey::String(String::from("level1"), TokenRange::from((s, 0, 6))),
                        TokenKey::Sn(0, TokenRange::from((s, 7, 8))),
                        TokenKey::String(String::from("level3"), TokenRange::from((s, 11, 17))),
                        TokenKey::Sn(1, TokenRange::from((s, 18, 19)))
                    ],
                    TokenRange::from((s, 0, 20))
                )
            ))
        );
    }

    #[test]
    fn test_single_var() {
        assert!(parse_var(Span::from("1name")).is_err());
        assert!(parse_var(Span::from("1name")).is_err());

        let s = Span::from(r#"name"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(4..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("name"),
                        TokenRange::from((s, 0, 4))
                    )],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"name "#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(4..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("name"),
                        TokenRange::from((s, 0, 4))
                    )],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#" name"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(5..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("name"),
                        TokenRange::from((s, 1, 5))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#" name "#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(5..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("name"),
                        TokenRange::from((s, 1, 5))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"name1"#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(5..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("name1"),
                        TokenRange::from((s, 0, 5))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"n_1_ "#);
        assert_eq!(
            parse_var(s),
            Ok((
                s.slice(4..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("n_1_"),
                        TokenRange::from((s, 0, 4))
                    )],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );
    }
}
