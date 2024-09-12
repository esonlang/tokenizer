extern crate core;

use nom::branch::alt;
use nom::error::VerboseError;
use nom::IResult;
use nom_locate::LocatedSpan;

pub use token::{ReadTokenRange, Token, TokenDec, TokenKey, TokenId, TokenRange};

use crate::decorator::parse_decorators;
use crate::dict::parse_dict;
use crate::list::parse_lst;
use crate::prim::{parse_boolean, parse_null, parse_number, parse_string};
use crate::sp::soc0;

pub type Span<'a> = LocatedSpan<&'a str>;

mod comments;
mod decorator;
mod dict;
mod expr;
mod fmt_string;
mod fn_call;
mod id;
mod list;
mod prim;
mod reference_var;
mod sp;
mod token;
mod var;

pub fn parse_prim(i: Span) -> nom::IResult<Span, Token, VerboseError<Span>> {
    alt((parse_boolean, parse_null, parse_number, parse_string))(i)
}

pub fn parse_base(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, decorators) = parse_decorators(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, root) = alt((parse_dict, parse_lst))(remaining)?;
    Ok((
        remaining,
        Token::TokenFrameRoot(
            decorators.into_iter().map(|d| d).collect(),
            Box::from(root),
            TokenRange::from((i, remaining)),
        ),
    ))
}

// trait SpanExt {
//     // character_count(Span::from("hello \u{1F601}")) => 7
//     fn character_count(&self) -> usize;
//
//     // byte_count(Span::from("hello \u{1F601}")) => 10
//     fn byte_count(&self) -> usize;
// }
//
// impl SpanExt for Span<'_> {
//     fn character_count(&self) -> usize {
//         self.fragment().chars().count()
//     }
//
//     fn byte_count(&self) -> usize {
//         self.fragment().len()
//     }
// }

#[cfg(test)]
mod tests {
    use nom::character::complete::{digit1, one_of};
    use nom::combinator::rest;
    use nom::sequence::preceded;
    use nom::Slice;

    use crate::token::{TokenDec, TokenId, TokenKey, TokenRange};
    use crate::Token::{TokenFrameDict, TokenFrameRoot, TokenPrimNumberInt};

    use super::*;

    #[test]
    fn test_comments() {
        let s = Span::from(
            r##"/* hello world */
// this is a test file
{}"##,
        );
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(43..),
                TokenFrameRoot(
                    vec![],
                    Box::from(TokenFrameDict(vec![], TokenRange::from((s, 41, 43)))),
                    TokenRange::from((s, 0, 43))
                )
            ))
        );
    }

    // #[test]
    // fn test_span_ext() {
    //     let s = Span::from("hello \n");
    //     assert_eq!(s.character_count(), 7);
    //     assert_eq!(s.byte_count(), 7);
    //
    //     let s = Span::from("hello \r\n");
    //     assert_eq!(s.character_count(), 8);
    //     assert_eq!(s.byte_count(), 8);
    //
    //     let s = Span::from("hello \u{08}");
    //     assert_eq!(s.to_string(), "hello \u{08}");
    //     assert_eq!(s.character_count(), 7);
    //     assert_eq!(s.byte_count(), 7);
    //
    //     let s = Span::from("hello \u{FE0F}");
    //     assert_eq!(s.to_string(), "hello \u{FE0F}");
    //     assert_eq!(s.character_count(), 7);
    //     assert_eq!(s.byte_count(), 9);
    //
    //     let s = Span::from("hello \u{1F601}");
    //     assert_eq!(s.to_string(), "hello 😁");
    //     assert_eq!(s.character_count(), 7);
    //     assert_eq!(s.byte_count(), 10);
    // }

    #[test]
    fn test_parse_prim() {
        let s = Span::from(r#"true"#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4)))
            ))
        );

        let s = Span::from(r#"false"#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimBoolean(false, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"null"#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNull(TokenRange::from((s, 0, 4)))
            ))
        );

        let s = Span::from(r#"1"#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(1..),
                Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1)))
            ))
        );

        let s = Span::from(r#"1.0"#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimNumberFloat(1.0, TokenRange::from((s, 0, 3)))
            ))
        );

        let s = Span::from(r#""foo""#);
        assert_eq!(
            parse_prim(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimString("foo".to_string(), TokenRange::from((s, 0, 5)))
            ))
        );
    }

    #[test]
    fn test_simple_root() {
        let s = Span::from(r#"{ "a": 1 }"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(10..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameDict(
                        vec![(
                            TokenKey::String("a".to_string(), TokenRange::from((s, 2, 5))),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8)))
                        )],
                        TokenRange::from((s, 0, 10))
                    )),
                    TokenRange::from((s, 0, 10))
                )
            ))
        );

        let s = Span::from("// comment \n {foo: 1, bar: 2,}");
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(30..),
                TokenFrameRoot(
                    vec![],
                    Box::from(TokenFrameDict(
                        vec![
                            (
                                TokenKey::String("foo".to_string(), TokenRange::from((s, 14, 17))),
                                vec![],
                                TokenPrimNumberInt(1, TokenRange::from((s, 19, 20)))
                            ),
                            (
                                TokenKey::String("bar".to_string(), TokenRange::from((s, 22, 25))),
                                vec![],
                                TokenPrimNumberInt(2, TokenRange::from((s, 27, 28)))
                            ),
                        ],
                        TokenRange::from((s, 13, 30))
                    )),
                    TokenRange::from((s, 0, 30))
                )
            ))
        );
    }

    #[test]
    fn test_expr_eson() {
        let s = Span::from(r#"{ "a": 1 != 2 }"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(15..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameDict(
                        vec![(
                            TokenKey::String("a".to_string(), TokenRange::from((s, 2, 5))),
                            vec![],
                            Token::TokenExprSequence(
                                vec![
                                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8))),
                                    Token::TokenOpNe(TokenRange::from((s, 9, 11))),
                                    Token::TokenPrimNumberInt(2, TokenRange::from((s, 12, 13))),
                                ],
                                TokenRange::from((s, 7, 13))
                            )
                        )],
                        TokenRange::from((s, 0, 15))
                    )),
                    TokenRange::from((s, 0, 15))
                )
            ))
        );
    }

    #[test]
    fn test_comment_decorator_eson() {
        let s = Span::from(
            r###"
                // foo decorator
                @foo
                { "a": 1 }"###,
        );
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(81..),
                Token::TokenFrameRoot(
                    vec![TokenDec(
                        TokenId("foo".to_string(), TokenRange::from((s, 51, 54))),
                        vec![],
                        TokenRange::from((s, 50, 54))
                    )],
                    Box::from(Token::TokenFrameDict(
                        vec![(
                            TokenKey::String("a".to_string(), TokenRange::from((s, 73, 76))),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 78, 79)))
                        )],
                        TokenRange::from((s, 71, 81))
                    )),
                    TokenRange::from((s, 0, 81))
                )
            ))
        );
    }

    #[test]
    fn test_list_eson() {
        let s = Span::from(r#"[1, 2, 3]"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(9..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameList(
                        vec![
                            (
                                TokenKey::DummySn(0),
                                vec![],
                                Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2)))
                            ),
                            (
                                TokenKey::DummySn(1),
                                vec![],
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 4, 5)))
                            ),
                            (
                                TokenKey::DummySn(2),
                                vec![],
                                Token::TokenPrimNumberInt(3, TokenRange::from((s, 7, 8)))
                            ),
                        ],
                        TokenRange::from((s, 0, 9))
                    )),
                    TokenRange::from((s, 0, 9))
                )
            ))
        );
    }

    #[test]
    fn test_ref_dict() {
        let s = Span::from(r#"{ "a": &sibling.b, "b": 2 }"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(27..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameDict(
                        vec![
                            (
                                TokenKey::String("a".to_string(), TokenRange::from((s, 2, 5))),
                                vec![],
                                Token::TokenRefVarSibling(
                                    vec![TokenKey::String(
                                        "b".to_string(),
                                        TokenRange::from((s, 16, 17))
                                    )],
                                    TokenRange::from((s, 7, 17))
                                )
                            ),
                            (
                                TokenKey::String("b".to_string(), TokenRange::from((s, 19, 22))),
                                vec![],
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 24, 25)))
                            ),
                        ],
                        TokenRange::from((s, 0, 27))
                    )),
                    TokenRange::from((s, 0, 27))
                )
            ))
        );
    }

    #[test]
    fn test_ref_list() {
        let s = Span::from(r#"[&sibling[1], 2]"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(16..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameList(
                        vec![
                            (
                                TokenKey::DummySn(0),
                                vec![],
                                Token::TokenRefVarSibling(
                                    vec![TokenKey::Sn(1, TokenRange::from((s, 10, 11)))],
                                    TokenRange::from((s, 1, 12))
                                )
                            ),
                            (
                                TokenKey::DummySn(1),
                                vec![],
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 14, 15)))
                            ),
                        ],
                        TokenRange::from((s, 0, 16))
                    )),
                    TokenRange::from((s, 0, 16))
                )
            ))
        );
    }

    #[test]
    fn test_var_list() {
        let s = Span::from(r#"[a, 2]"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(6..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameList(
                        vec![
                            (
                                TokenKey::DummySn(0),
                                vec![],
                                Token::TokenVar(
                                    vec![TokenKey::String(
                                        "a".to_string(),
                                        TokenRange::from((s, 1, 2))
                                    )],
                                    TokenRange::from((s, 1, 2))
                                )
                            ),
                            (
                                TokenKey::DummySn(1),
                                vec![],
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 4, 5)))
                            ),
                        ],
                        TokenRange::from((s, 0, 6))
                    )),
                    TokenRange::from((s, 0, 6))
                )
            ))
        );
    }

    #[test]
    fn test_fn_call_list() {
        let s = Span::from(r#"[f({a: 1}), 2]"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(14..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameList(
                        vec![
                            (
                                TokenKey::DummySn(0),
                                vec![],
                                Token::TokenFnCall(
                                    TokenId("f".to_string(), TokenRange::from((s, 1, 2))),
                                    vec![Token::TokenFrameDict(
                                        vec![(
                                            TokenKey::String(
                                                "a".to_string(),
                                                TokenRange::from((s, 4, 5))
                                            ),
                                            vec![],
                                            Token::TokenPrimNumberInt(
                                                1,
                                                TokenRange::from((s, 7, 8))
                                            )
                                        )],
                                        TokenRange::from((s, 3, 9))
                                    )],
                                    TokenRange::from((s, 1, 10))
                                )
                            ),
                            (
                                TokenKey::DummySn(1),
                                vec![],
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 12, 13)))
                            ),
                        ],
                        TokenRange::from((s, 0, 14))
                    )),
                    TokenRange::from((s, 0, 14))
                )
            ))
        );
    }

    #[test]
    fn test_fmt_string_list() {
        let s = Span::from(r#"[f"a ${ &sibling[1] }", "b"]"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(28..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameList(
                        vec![
                            (
                                TokenKey::DummySn(0),
                                vec![],
                                Token::TokenExprSequence(
                                    vec![
                                        Token::TokenPrimString(
                                            "a ".to_string(),
                                            TokenRange::from((s, 3, 5))
                                        ),
                                        Token::TokenDummyOpConcat,
                                        Token::TokenRefVarSibling(
                                            vec![TokenKey::Sn(1, TokenRange::from((s, 17, 18)))],
                                            TokenRange::from((s, 8, 19))
                                        ),
                                    ],
                                    TokenRange::from((s, 1, 22))
                                )
                            ),
                            (
                                TokenKey::DummySn(1),
                                vec![],
                                Token::TokenPrimString(
                                    "b".to_string(),
                                    TokenRange::from((s, 24, 27))
                                )
                            ),
                        ],
                        TokenRange::from((s, 0, 28))
                    )),
                    TokenRange::from((s, 0, 28))
                )
            ))
        );
    }

    #[test]
    fn test_root_ref_in_fmt_string_dict() {
        let s = Span::from(r#"{ "a": f"a ${ &root.b[0] }", "b": [0, 1] }"#);
        assert_eq!(
            parse_base(s),
            Ok((
                s.slice(42..),
                Token::TokenFrameRoot(
                    vec![],
                    Box::from(Token::TokenFrameDict(
                        vec![
                            (
                                TokenKey::String("a".to_string(), TokenRange::from((s, 2, 5))),
                                vec![],
                                Token::TokenExprSequence(
                                    vec![
                                        Token::TokenPrimString(
                                            "a ".to_string(),
                                            TokenRange::from((s, 9, 11))
                                        ),
                                        Token::TokenDummyOpConcat,
                                        Token::TokenRefVarRoot(
                                            vec![
                                                TokenKey::String(
                                                    "b".to_string(),
                                                    TokenRange::from((s, 20, 21))
                                                ),
                                                TokenKey::Sn(0, TokenRange::from((s, 22, 23))),
                                            ],
                                            TokenRange::from((s, 14, 24))
                                        ),
                                    ],
                                    TokenRange::from((s, 7, 27))
                                )
                            ),
                            (
                                TokenKey::String("b".to_string(), TokenRange::from((s, 29, 32))),
                                vec![],
                                Token::TokenFrameList(
                                    vec![
                                        (
                                            TokenKey::DummySn(0),
                                            vec![],
                                            Token::TokenPrimNumberInt(
                                                0,
                                                TokenRange::from((s, 35, 36))
                                            )
                                        ),
                                        (
                                            TokenKey::DummySn(1),
                                            vec![],
                                            Token::TokenPrimNumberInt(
                                                1,
                                                TokenRange::from((s, 38, 39))
                                            )
                                        ),
                                    ],
                                    TokenRange::from((s, 34, 40))
                                )
                            ),
                        ],
                        TokenRange::from((s, 0, 42))
                    )),
                    TokenRange::from((s, 0, 42))
                )
            ))
        );
    }

    #[test]
    fn test() {
        use nom::combinator::cut;

        fn parser(input: &str) -> IResult<&str, &str> {
            alt((preceded(one_of("+-"), digit1), rest))(input)
        }

        assert_eq!(parser("+10 ab"), Ok((" ab", "10")));
        assert_eq!(parser("ab"), Ok(("", "ab")));
        assert_eq!(parser("+"), Ok(("", "+")));

        fn parser2(input: &str) -> IResult<&str, &str> {
            alt((preceded(one_of("+-"), cut(digit1)), rest))(input)
        }

        assert_eq!(parser2("+10 ab"), Ok((" ab", "10")));
        assert_eq!(parser2("ab"), Ok(("", "ab")));
    }
}
