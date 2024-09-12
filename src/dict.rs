use nom::branch::alt;
use nom::character::complete::{char, digit1};
use nom::combinator::{cut, map, opt};
use nom::error::{context, VerboseError};
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::decorator::parse_decorators;
use crate::expr::parse_expr;
use crate::id::id;
use crate::prim::parse_string;
use crate::token::{TokenDec, TokenRange};
use crate::{soc0, Span, Token, TokenKey};

fn tuple_decorators_and_k(i: Span) -> IResult<Span, (TokenKey, Vec<TokenDec>), VerboseError<Span>> {
    let (remaining, decorators) = parse_decorators(i)?;
    let (remaining, k) = preceded(
        soc0,
        alt((
            map(parse_string, |s| {
                let Token::TokenPrimString(n, range) = s else {
                    unreachable!()
                };
                TokenKey::String(n, range)
            }),
            map(digit1, |s: Span| {
                TokenKey::String(
                    s.fragment().to_string(),
                    TokenRange::from((s, s.fragment().len())),
                )
            }),
            map(id, |s| TokenKey::String(s.0, s.1)),
        )),
    )(remaining)?;
    Ok((remaining, (k, decorators)))
}

fn tuple_k_decorators_v(
    i: Span,
) -> IResult<Span, ((TokenKey, Vec<TokenDec>), Token), VerboseError<Span>> {
    separated_pair(
        tuple_decorators_and_k,
        cut(delimited(soc0, char(':'), soc0)),
        parse_expr,
    )(i)
}

pub fn parse_dict(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, _) = context("dict_head", terminated(char('{'), soc0))(i)?;
    let (remaining, body) = context(
        "dict_body",
        separated_list0(delimited(soc0, char(','), soc0), tuple_k_decorators_v),
    )(remaining)?;
    let (remaining, _) =
        context("dict_tail", tuple((soc0, opt(char(',')), soc0, char('}'))))(remaining)?;

    Ok((
        remaining,
        Token::TokenFrameDict(
            body.into_iter().map(|((k, d), v)| (k, d, v)).collect(),
            TokenRange::from((i, remaining)),
        ),
    ))
}

#[cfg(test)]
mod tests {
    use nom::sequence::pair;
    use nom::Slice;

    use crate::token::TokenId;
    use crate::Token::{
        TokenDummyOpConcat, TokenExprSequence, TokenFnCall, TokenFrameDict, TokenFrameList,
        TokenOpAdd, TokenPrimNumberInt, TokenPrimString, TokenVar,
    };

    use super::*;

    #[test]
    fn test_offset() {
        let s = Span::from(
            r###"
            // foo decorator
            @foo
        "###,
        );
        let (rest, _) = parse_decorators(s).unwrap();
        assert_eq!(rest, s.slice(46..));

        let s = Span::from("\n foo");
        let (rest, (_, id)) = pair(soc0, id)(s).unwrap();
        assert_eq!(rest, s.slice(5..));
        assert_eq!(id, TokenId("foo".to_string(), TokenRange::from((s, 2, 5))));

        let s = Span::from(r#"@bar \n foo"#);
        let (_, ds) = parse_decorators(s).unwrap();
        assert_eq!(
            ds,
            vec![TokenDec(
                TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                vec![],
                TokenRange::from((s, 0, 4))
            )]
        );
    }

    #[test]
    fn test_tuple_decorators_k_v() {
        let s = Span::from(r#"foo: 1"#);
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 0, 3))),
                vec![]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 5, 6))));
    }

    #[test]
    fn test_k_decorators() {
        let s = Span::from(r#"foo"#);
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 0, 3))),
                vec![]
            )
        );

        let s = Span::from(r#"0"#);
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("0".to_string(), TokenRange::from((s, 0, 1))),
                vec![]
            )
        );

        let s = Span::from(r#"a0"#);
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("a0".to_string(), TokenRange::from((s, 0, 2))),
                vec![]
            )
        );

        let s = Span::from(r#"0a"#);
        let (rest, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("0".to_string(), TokenRange::from((s, 0, 1))),
                vec![]
            )
        );
        assert_eq!(rest, s.slice(1..));

        let s = Span::from(r#" "0a" "#);
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("0a".to_string(), TokenRange::from((s, 1, 5))),
                vec![]
            )
        );
    }

    #[test]
    fn test_() {
        let s = Span::from(r#"1"#);
        assert_eq!(
            tuple_decorators_and_k(s),
            Ok((
                s.slice(1..),
                (
                    TokenKey::String("1".to_string(), TokenRange::from((s, 0, 1))),
                    vec![]
                )
            ))
        );

        let s = Span::from(r#"@foo 1"#);
        assert_eq!(
            tuple_decorators_and_k(s),
            Ok((
                s.slice(6..),
                (
                    TokenKey::String("1".to_string(), TokenRange::from((s, 5, 6))),
                    vec![TokenDec(
                        TokenId("foo".to_string(), TokenRange::from((s, 1, 4))),
                        vec![],
                        TokenRange::from((s, 0, 4))
                    )]
                )
            ))
        );

        let s = Span::from("@bar(1) \n foo");
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".into(), TokenRange::from((s, 10, 13))),
                vec![TokenDec(
                    TokenId("bar".into(), TokenRange::from((s, 1, 4))),
                    vec![TokenPrimNumberInt(1, TokenRange::from((s, 5, 6)))],
                    TokenRange::from((s, 0, 7))
                )]
            )
        );

        let s = Span::from("@bar(1, 2) \n foo");
        let (_, k1) = tuple_decorators_and_k(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 13, 16))),
                vec![TokenDec(
                    TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                    vec![
                        TokenPrimNumberInt(1, TokenRange::from((s, 5, 6))),
                        TokenPrimNumberInt(2, TokenRange::from((s, 8, 9)))
                    ],
                    TokenRange::from((s, 0, 10))
                )]
            )
        );
    }

    #[test]
    fn test_key_value() {
        let s = Span::from(r#"foo: 1"#);
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 0, 3))),
                vec![]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 5, 6))));

        let s = Span::from("@bar \n foo: 1");
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 7, 10))),
                vec![TokenDec(
                    TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                    vec![],
                    TokenRange::from((s, 0, 4))
                )]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 12, 13))));

        let s = Span::from("@bar() \n foo: 1");
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 9, 12))),
                vec![TokenDec(
                    TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                    vec![],
                    TokenRange::from((s, 0, 6))
                )]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 14, 15))));

        let s = Span::from("@bar(1) \n foo: 1");
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 10, 13))),
                vec![TokenDec(
                    TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                    vec![TokenPrimNumberInt(1, TokenRange::from((s, 5, 6)))],
                    TokenRange::from((s, 0, 7))
                )]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 15, 16))));

        let s = Span::from("@bar(1, 2) \n foo: 1");
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("foo".to_string(), TokenRange::from((s, 13, 16))),
                vec![TokenDec(
                    TokenId("bar".to_string(), TokenRange::from((s, 1, 4))),
                    vec![
                        TokenPrimNumberInt(1, TokenRange::from((s, 5, 6))),
                        TokenPrimNumberInt(2, TokenRange::from((s, 8, 9)))
                    ],
                    TokenRange::from((s, 0, 10))
                )]
            )
        );
        assert_eq!(v1, TokenPrimNumberInt(1, TokenRange::from((s, 18, 19))));

        let s = Span::from(r#""bar": f"hello ${name}""#);
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("bar".to_string(), TokenRange::from((s, 0, 5))),
                vec![]
            )
        );
        assert_eq!(
            v1,
            TokenExprSequence(
                vec![
                    TokenPrimString("hello ".to_string(), TokenRange::from((s, 9, 15))),
                    TokenDummyOpConcat,
                    TokenVar(
                        vec![TokenKey::String(
                            "name".to_string(),
                            TokenRange::from((s, 17, 21))
                        )],
                        TokenRange::from((s, 17, 21))
                    ),
                ],
                TokenRange::from((s, 7, 23))
            )
        );

        // k: expr
        let s = Span::from(r#"k: 1 + 2"#);
        let (_, (k1, v1)) = tuple_k_decorators_v(s).unwrap();
        assert_eq!(
            k1,
            (
                TokenKey::String("k".to_string(), TokenRange::from((s, 0, 1))),
                vec![]
            )
        );
        assert_eq!(
            v1,
            TokenExprSequence(
                vec![
                    TokenPrimNumberInt(1, TokenRange::from((s, 3, 4))),
                    TokenOpAdd(TokenRange::from((s, 5, 6))),
                    TokenPrimNumberInt(2, TokenRange::from((s, 7, 8))),
                ],
                TokenRange::from((s, 3, 8))
            )
        );
    }

    #[test]
    fn test_parse_dict() {
        let s = Span::from(r#"{}"#);
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(2..),
                TokenFrameDict(vec![], TokenRange::from((s, 0, 2)))
            ))
        );

        let s = Span::from(r#"{foo: 1}"#);
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(8..),
                TokenFrameDict(
                    vec![(
                        TokenKey::String("foo".to_string(), TokenRange::from((s, 1, 4))),
                        vec![],
                        TokenPrimNumberInt(1, TokenRange::from((s, 6, 7)))
                    )],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );

        let s = Span::from(r#"{foo: 1, bar: 2}"#);
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(16..),
                TokenFrameDict(
                    vec![
                        (
                            TokenKey::String("foo".to_string(), TokenRange::from((s, 1, 4))),
                            vec![],
                            TokenPrimNumberInt(1, TokenRange::from((s, 6, 7)))
                        ),
                        (
                            TokenKey::String("bar".to_string(), TokenRange::from((s, 9, 12))),
                            vec![],
                            TokenPrimNumberInt(2, TokenRange::from((s, 14, 15)))
                        ),
                    ],
                    TokenRange::from((s, 0, 16))
                )
            ))
        );

        let s = Span::from(r#"{foo: 1, bar: 2,}"#);
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(17..),
                TokenFrameDict(
                    vec![
                        (
                            TokenKey::String("foo".to_string(), TokenRange::from((s, 1, 4))),
                            vec![],
                            TokenPrimNumberInt(1, TokenRange::from((s, 6, 7)))
                        ),
                        (
                            TokenKey::String("bar".to_string(), TokenRange::from((s, 9, 12))),
                            vec![],
                            TokenPrimNumberInt(2, TokenRange::from((s, 14, 15)))
                        ),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );

        let s = Span::from(r#"{foo: 1, bar: 2,} // comment"#);
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(17..),
                TokenFrameDict(
                    vec![
                        (
                            TokenKey::String("foo".to_string(), TokenRange::from((s, 1, 4))),
                            vec![],
                            TokenPrimNumberInt(1, TokenRange::from((s, 6, 7)))
                        ),
                        (
                            TokenKey::String("bar".to_string(), TokenRange::from((s, 9, 12))),
                            vec![],
                            TokenPrimNumberInt(2, TokenRange::from((s, 14, 15)))
                        ),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );
    }

    #[test]
    fn test_parse_dict_with_comment_and_decorator() {
        let s = Span::from(
            r###"{
                // hello
                @hello("a")
                "c": {},
                }"###,
        );
        assert_eq!(
            parse_dict(s),
            Ok((
                s.slice(97..),
                TokenFrameDict(
                    vec![(
                        TokenKey::String("c".to_string(), TokenRange::from((s, 71, 74))),
                        vec![TokenDec(
                            TokenId("hello".to_string(), TokenRange::from((s, 44, 49))),
                            vec![Token::TokenPrimString(
                                "a".to_string(),
                                TokenRange::from((s, 50, 53))
                            )],
                            TokenRange::from((s, 43, 54))
                        )],
                        TokenFrameDict(vec![], TokenRange::from((s, 76, 78)))
                    )],
                    TokenRange::from((s, 0, 97))
                )
            ))
        );

        let s = Span::from(
            r###"{
                        // foo decorator
                        @foo

                        "c": "hello",
                        "d": "bar",
                    }"###,
        );
        let (_, r) = parse_dict(s).unwrap();
        assert_eq!(
            r,
            TokenFrameDict(
                vec![
                    (
                        TokenKey::String("c".to_string(), TokenRange::from((s, 97, 100))),
                        vec![TokenDec(
                            TokenId("foo".to_string(), TokenRange::from((s, 68, 71))),
                            vec![],
                            TokenRange::from((s, 67, 71))
                        )],
                        TokenPrimString("hello".to_string(), TokenRange::from((s, 102, 109))),
                    ),
                    (
                        TokenKey::String("d".to_string(), TokenRange::from((s, 135, 138))),
                        vec![],
                        TokenPrimString("bar".to_string(), TokenRange::from((s, 140, 145))),
                    ),
                ],
                TokenRange::from((s, 0, 168))
            )
        );

        let s = Span::from(
            r###"{
                    "c": {
                        "hello": "world",
                        "foo": r"bar",
                        // comment
                        "bar": f"hello ${name}",
                    }
                }"###,
        );
        let (_, r) = parse_dict(s).unwrap();
        assert_eq!(
            r,
            TokenFrameDict(
                vec![(
                    TokenKey::String("c".to_string(), TokenRange::from((s, 22, 25))),
                    vec![],
                    TokenFrameDict(
                        vec![
                            (
                                TokenKey::String(
                                    "hello".to_string(),
                                    TokenRange::from((s, 53, 60))
                                ),
                                vec![],
                                TokenPrimString("world".to_string(), TokenRange::from((s, 62, 69))),
                            ),
                            (
                                TokenKey::String("foo".to_string(), TokenRange::from((s, 95, 100))),
                                vec![],
                                TokenPrimString("bar".to_string(), TokenRange::from((s, 102, 108))),
                            ),
                            (
                                TokenKey::String(
                                    "bar".to_string(),
                                    TokenRange::from((s, 169, 174))
                                ),
                                vec![],
                                TokenExprSequence(
                                    vec![
                                        TokenPrimString(
                                            "hello ".to_string(),
                                            TokenRange::from((s, 178, 184))
                                        ),
                                        TokenDummyOpConcat,
                                        TokenVar(
                                            vec![TokenKey::String(
                                                "name".to_string(),
                                                TokenRange::from((s, 186, 190))
                                            )],
                                            TokenRange::from((s, 186, 190))
                                        ),
                                    ],
                                    TokenRange::from((s, 176, 192))
                                ),
                            ),
                        ],
                        TokenRange::from((s, 27, 215))
                    ),
                )],
                TokenRange::from((s, 0, 233))
            )
        );
    }

    #[test]
    fn test_k_func_v() {
        let s = Span::from(
            r#"{
                    k: f(f2(), [
                        1,
                        "a",
                        f3(),
                    ]),
                }"#,
        );
        let (_, r) = parse_dict(s).unwrap();
        assert_eq!(
            r,
            TokenFrameDict(
                vec![(
                    TokenKey::String("k".to_string(), TokenRange::from((s, 22, 23))),
                    vec![],
                    TokenFnCall(
                        TokenId("f".to_string(), TokenRange::from((s, 25, 26))),
                        vec![
                            TokenFnCall(
                                TokenId("f2".to_string(), TokenRange::from((s, 27, 29))),
                                vec![],
                                TokenRange::from((s, 27, 31))
                            ),
                            TokenFrameList(
                                vec![
                                    (
                                        TokenKey::DummySn(0),
                                        vec![],
                                        TokenPrimNumberInt(1, TokenRange::from((s, 59, 60)))
                                    ),
                                    (
                                        TokenKey::DummySn(1),
                                        vec![],
                                        TokenPrimString(
                                            "a".to_string(),
                                            TokenRange::from((s, 86, 89))
                                        )
                                    ),
                                    (
                                        TokenKey::DummySn(2),
                                        vec![],
                                        TokenFnCall(
                                            TokenId(
                                                "f3".to_string(),
                                                TokenRange::from((s, 115, 117))
                                            ),
                                            vec![],
                                            TokenRange::from((s, 115, 119))
                                        )
                                    ),
                                ],
                                TokenRange::from((s, 33, 142))
                            ),
                        ],
                        TokenRange::from((s, 25, 143))
                    ),
                )],
                TokenRange::from((s, 0, 162))
            )
        );
    }
}
