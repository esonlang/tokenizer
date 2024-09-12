use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::error::{context, VerboseError};
use nom::sequence::pair;
use nom::IResult;

use crate::token::TokenRange;
use crate::var::rest_k;
use crate::Token::{TokenRefVarRoot, TokenRefVarSibling, TokenRefVarUncle};
use crate::{Span, Token};

fn head_k(i: Span) -> IResult<Span, char, VerboseError<Span>> {
    alt((
        map(tag("&sibling"), |_| 's'),
        map(tag("&uncle"), |_| 'u'),
        map(tag("&root"), |_| 'r'),
    ))(i)
}

// & + parse_var => parse_ref
pub(crate) fn parse_ref_var(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, (head, keys)) = context("parse_ref_var", pair(head_k, rest_k))(i)?;
    let v = match (head, keys) {
        ('s', keys) => TokenRefVarSibling(keys, TokenRange::from((i, remaining))),
        ('u', keys) => TokenRefVarUncle(keys, TokenRange::from((i, remaining))),
        ('r', keys) => TokenRefVarRoot(keys, TokenRange::from((i, remaining))),
        _ => unreachable!(),
    };
    Ok((remaining, v))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TokenKey;
    use nom::Slice;

    #[test]
    fn test_ref_var() {
        let s = Span::from(r#"&uncle1.ele"#);
        assert!(parse_ref_var(s).is_err());

        let s = Span::from(r#"&uncle.ele"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(10..),
                TokenRefVarUncle(
                    vec![TokenKey::String(
                        String::from("ele"),
                        TokenRange::from((s, 7, 10))
                    )],
                    TokenRange::from((s, 0, 10))
                )
            ))
        );

        let s = Span::from(r#"&sibling.ele.ele2"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(17..),
                TokenRefVarSibling(
                    vec![
                        TokenKey::String(String::from("ele"), TokenRange::from((s, 9, 12))),
                        TokenKey::String(String::from("ele2"), TokenRange::from((s, 13, 17))),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );

        let s = Span::from(r#"&sibling.ele.0"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(14..),
                TokenRefVarSibling(
                    vec![
                        TokenKey::String(String::from("ele"), TokenRange::from((s, 9, 12))),
                        TokenKey::Sn(0, TokenRange::from((s, 13, 14)))
                    ],
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"&uncle.ele1[0]"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(14..),
                TokenRefVarUncle(
                    vec![
                        TokenKey::String(String::from("ele1"), TokenRange::from((s, 7, 11))),
                        TokenKey::Sn(0, TokenRange::from((s, 12, 13))),
                    ],
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"&uncle.ele1[0].ele2"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(19..),
                TokenRefVarUncle(
                    vec![
                        TokenKey::String(String::from("ele1"), TokenRange::from((s, 7, 11))),
                        TokenKey::Sn(0, TokenRange::from((s, 12, 13))),
                        TokenKey::String(String::from("ele2"), TokenRange::from((s, 15, 19))),
                    ],
                    TokenRange::from((s, 0, 19))
                )
            ))
        );

        let s = Span::from(r#"&uncle.ele1[0][1].ele2"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(22..),
                TokenRefVarUncle(
                    vec![
                        TokenKey::String(String::from("ele1"), TokenRange::from((s, 7, 11))),
                        TokenKey::Sn(0, TokenRange::from((s, 12, 13))),
                        TokenKey::Sn(1, TokenRange::from((s, 15, 16))),
                        TokenKey::String(String::from("ele2"), TokenRange::from((s, 18, 22))),
                    ],
                    TokenRange::from((s, 0, 22))
                )
            ))
        );

        let s = Span::from(r#"&uncle[1]"#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(9..),
                TokenRefVarUncle(
                    vec![TokenKey::Sn(1, TokenRange::from((s, 7, 8)))],
                    TokenRange::from((s, 0, 9))
                )
            ))
        );

        let s = Span::from(r#"&root[0] "#);
        assert_eq!(
            parse_ref_var(s),
            Ok((
                s.slice(8..),
                TokenRefVarRoot(
                    vec![TokenKey::Sn(0, TokenRange::from((s, 6, 7)))],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );
    }
}
