use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char as ch, multispace0, multispace1};
use nom::combinator::{map, opt, recognize};
use nom::error::{context, VerboseError};
use nom::multi::{many0, many_till};
use nom::sequence::{delimited, pair};
use nom::{IResult, Slice};

use crate::comments::block_comment;
use crate::dict::parse_dict;
use crate::fmt_string::parse_fmt_string;
use crate::fn_call::parse_fn_call;
use crate::list::parse_lst;
use crate::prim::{parse_boolean, parse_null, parse_number, parse_string};
use crate::reference_var::parse_ref_var;
use crate::token::TokenRange;
use crate::var::parse_var;
use crate::{Span, Token};

fn operator(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, op) = context(
        "operator",
        alt((
            tag("=="),
            tag("!="),
            tag("<="),
            tag(">="),
            tag("&&"),
            tag("||"),
            tag("!"),
            tag(">"),
            tag("<"),
            tag(".."),
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("|"),
            tag("?"),
            tag(":"),
        )),
    )(i)?;
    let t = match *op.fragment() {
        "==" => Token::TokenOpEq(TokenRange::from((op, 2))),
        "!=" => Token::TokenOpNe(TokenRange::from((op, 2))),
        "<=" => Token::TokenOpLe(TokenRange::from((op, 2))),
        ">=" => Token::TokenOpGe(TokenRange::from((op, 2))),
        "&&" => Token::TokenOpAnd(TokenRange::from((op, 2))),
        "||" => Token::TokenOpOr(TokenRange::from((op, 2))),
        "!" => Token::TokenOpNot(TokenRange::from((op, 1))),
        ">" => Token::TokenOpGt(TokenRange::from((op, 1))),
        "<" => Token::TokenOpLt(TokenRange::from((op, 1))),
        ".." => Token::TokenOpConcat(TokenRange::from((op, 2))),
        "+" => Token::TokenOpAdd(TokenRange::from((op, 1))),
        "-" => Token::TokenOpSub(TokenRange::from((op, 1))),
        "*" => Token::TokenOpMul(TokenRange::from((op, 1))),
        "/" => Token::TokenOpDiv(TokenRange::from((op, 1))),
        "%" => Token::TokenOpMod(TokenRange::from((op, 1))),
        "|" => Token::TokenOpPipe(TokenRange::from((op, 1))),
        "?" => Token::TokenOpQ(TokenRange::from((op, 1))),
        ":" => Token::TokenOpColon(TokenRange::from((op, 1))),
        _ => unreachable!(),
    };
    Ok((remaining, t))
}

fn group(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    let (remaining, r) = context(
        "group",
        delimited(
            pair(ch('('), multispace0),
            parse_expr,
            pair(multispace0, ch(')')),
        ),
    )(i)?;
    Ok((
        remaining,
        Token::TokenFrameExprGroup(Box::new(r), TokenRange::from((i, remaining))),
    ))
}

pub(crate) fn expr_unit(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    context(
        "unit",
        alt((
            parse_null,
            parse_number,
            parse_string,
            parse_boolean,
            parse_dict,
            parse_lst,
            parse_fn_call,
            parse_fmt_string,
            parse_ref_var,
            parse_var,
            group,
        )),
    )(i)
}

// expr_unit + (expr_unit + operator)* => expr_unit | expr_sequence
pub(crate) fn parse_expr(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    fn _expr_pair(i: Span) -> IResult<Span, (Vec<Token>, TokenRange), VerboseError<Span>> {
        let (remaining, first) = expr_unit(i)?;
        let (remaining, rest) = many0(pair(
            delimited(multispace0, operator, multispace0),
            expr_unit,
        ))(remaining)?;
        let mut tokens = vec![first];
        for (op, token) in rest {
            tokens.push(op);
            tokens.push(token);
        }
        Ok((remaining, (tokens, TokenRange::from((i, remaining)))))
    }
    context(
        "parse_expr",
        map(_expr_pair, |(mut tokens, range)| match tokens.len() {
            0 => unreachable!(),
            1 => tokens.pop().unwrap_or_else(|| unreachable!()),
            _ => Token::TokenExprSequence(tokens, range),
        }),
    )(i)
}

fn line_comment_in_expr_zone(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    let (remaining, head) = recognize(pair(tag("//"), multispace0))(i)?;
    let (_, (r, _)) = many_till(anychar, alt((ch('}'), ch('\n'))))(remaining)?;
    let offset = head.len();
    let p = offset + r.len();
    Ok((i.slice(p..), i.slice(offset..p)))
}

fn space_or_comment_in_expr_zone(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    context(
        "space_or_comment",
        alt((multispace1, line_comment_in_expr_zone, block_comment)),
    )(i)
}

// f#"${ ... } in format string"#
pub(crate) fn parse_expr_zone(i: Span) -> IResult<Span, (Token, TokenRange), VerboseError<Span>> {
    let (remaining, r) = context(
        "parse_expr_zone",
        delimited(
            pair(tag("${"), many0(space_or_comment_in_expr_zone)),
            opt(parse_expr),
            pair(many0(space_or_comment_in_expr_zone), ch('}')),
        ),
    )(i)?;
    Ok((
        remaining,
        (
            r.unwrap_or_else(|| Token::Void),
            TokenRange::from((i, remaining)),
        ),
    ))
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use crate::token::{TokenId, TokenKey, TokenRange};

    use super::*;

    #[test]
    fn test_operator() {
        let s = Span::from("==");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpEq(TokenRange::from((s, 0, 2))),))
        );

        let s = Span::from("!=");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpNe(TokenRange::from((s, 0, 2)))))
        );
        let s = Span::from("<=");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpLe(TokenRange::from((s, 0, 2)))))
        );
        let s = Span::from(">=");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpGe(TokenRange::from((s, 0, 2)))))
        );
        let s = Span::from("&&");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpAnd(TokenRange::from((s, 0, 2)))))
        );
        let s = Span::from("||");
        assert_eq!(
            operator(s),
            Ok((s.slice(2..), Token::TokenOpOr(TokenRange::from((s, 0, 2)))))
        );
        let s = Span::from("!");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpNot(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from(">");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpGt(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("<");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpLt(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("+");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpAdd(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("-");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpSub(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("*");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpMul(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("/");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpDiv(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("%");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpMod(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from("|");
        assert_eq!(
            operator(s),
            Ok((
                s.slice(1..),
                Token::TokenOpPipe(TokenRange::from((s, 0, 1)))
            ))
        );
        let s = Span::from("?");
        assert_eq!(
            operator(s),
            Ok((s.slice(1..), Token::TokenOpQ(TokenRange::from((s, 0, 1)))))
        );
        let s = Span::from(":");
        assert_eq!(
            operator(s),
            Ok((
                s.slice(1..),
                Token::TokenOpColon(TokenRange::from((s, 0, 1)))
            ))
        );
    }

    #[test]
    fn test_group() {
        let s = Span::from("(1 + 2)");
        assert_eq!(
            group(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameExprGroup(
                    Box::new(Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2))),
                            Token::TokenOpAdd(TokenRange::from((s, 3, 4))),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                        ],
                        TokenRange::from((s, 1, 6))
                    )),
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from("(1 + 2) * 3");
        assert_eq!(
            group(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameExprGroup(
                    Box::new(Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2))),
                            Token::TokenOpAdd(TokenRange::from((s, 3, 4))),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                        ],
                        TokenRange::from((s, 1, 6))
                    )),
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from("(2 * (1 + 3))");
        assert_eq!(
            group(s),
            Ok((
                s.slice(13..),
                Token::TokenFrameExprGroup(
                    Box::new(Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 1, 2))),
                            Token::TokenOpMul(TokenRange::from((s, 3, 4))),
                            Token::TokenFrameExprGroup(
                                Box::new(Token::TokenExprSequence(
                                    vec![
                                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 6, 7))),
                                        Token::TokenOpAdd(TokenRange::from((s, 8, 9))),
                                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 10, 11)))
                                    ],
                                    TokenRange::from((s, 6, 11))
                                )),
                                TokenRange::from((s, 5, 12))
                            )
                        ],
                        TokenRange::from((s, 1, 12))
                    )),
                    TokenRange::from((s, 0, 13))
                )
            ))
        );
    }

    #[test]
    fn test_expr_unit() {
        let s = Span::from("null");
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNull(TokenRange::from((s, 0, 4)))
            ))
        );

        let s = Span::from("true");
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4)))
            ))
        );

        let s = Span::from("false");
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimBoolean(false, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from("1");
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(1..),
                Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1)))
            ))
        );

        let s = Span::from("1.0");
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimNumberFloat(1.0, TokenRange::from((s, 0, 3)))
            ))
        );

        let s = Span::from(r#""hello""#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(7..),
                Token::TokenPrimString(String::from("hello"), TokenRange::from((s, 0, 7)))
            ))
        );

        let s = Span::from(r#""hello world""#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(13..),
                Token::TokenPrimString(String::from("hello world"), TokenRange::from((s, 0, 13)))
            ))
        );

        let s = Span::from(r#"f()"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(3..),
                Token::TokenFnCall(
                    TokenId("f".to_string(), TokenRange::from((s, 0, 1))),
                    vec![],
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"f(1)"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(4..),
                Token::TokenFnCall(
                    TokenId("f".to_string(), TokenRange::from((s, 0, 1))),
                    vec![Token::TokenPrimNumberInt(1, TokenRange::from((s, 2, 3)))],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"f(1, 2)"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(7..),
                Token::TokenFnCall(
                    TokenId("f".to_string(), TokenRange::from((s, 0, 1))),
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 2, 3))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                    ],
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from(r#"var"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(3..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("var"),
                        TokenRange::from((s, 0, 3))
                    )],
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"var1"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(4..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("var1"),
                        TokenRange::from((s, 0, 4))
                    )],
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"var_1"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(5..),
                Token::TokenVar(
                    vec![TokenKey::String(
                        String::from("var_1"),
                        TokenRange::from((s, 0, 5))
                    )],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"&sibling.k1"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(11..),
                Token::TokenRefVarSibling(
                    vec![TokenKey::String(
                        String::from("k1"),
                        TokenRange::from((s, 9, 11))
                    )],
                    TokenRange::from((s, 0, 11))
                )
            ))
        );

        let s = Span::from(r#"&sibling.k1.k2"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(14..),
                Token::TokenRefVarSibling(
                    vec![
                        TokenKey::String(String::from("k1"), TokenRange::from((s, 9, 11))),
                        TokenKey::String(String::from("k2"), TokenRange::from((s, 12, 14))),
                    ],
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"&uncle.k1"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(9..),
                Token::TokenRefVarUncle(
                    vec![TokenKey::String(
                        String::from("k1"),
                        TokenRange::from((s, 7, 9))
                    )],
                    TokenRange::from((s, 0, 9))
                )
            ))
        );

        let s = Span::from(r#"&uncle.k1.k2"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(12..),
                Token::TokenRefVarUncle(
                    vec![
                        TokenKey::String(String::from("k1"), TokenRange::from((s, 7, 9))),
                        TokenKey::String(String::from("k2"), TokenRange::from((s, 10, 12))),
                    ],
                    TokenRange::from((s, 0, 12))
                )
            ))
        );

        let s = Span::from(r#"&root.k1"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(8..),
                Token::TokenRefVarRoot(
                    vec![TokenKey::String(
                        String::from("k1"),
                        TokenRange::from((s, 6, 8))
                    )],
                    TokenRange::from((s, 0, 8))
                )
            ))
        );

        let s = Span::from(r#"{}"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(2..),
                Token::TokenFrameDict(vec![], TokenRange::from((s, 0, 2)))
            ))
        );

        let s = Span::from(r#"{k1: 1}"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameDict(
                    vec![(
                        TokenKey::String(String::from("k1"), TokenRange::from((s, 1, 3))),
                        vec![],
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 5, 6)))
                    )],
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from(r#"{k1: 1, k2: 2}"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(14..),
                Token::TokenFrameDict(
                    vec![
                        (
                            TokenKey::String(String::from("k1"), TokenRange::from((s, 1, 3))),
                            vec![],
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 5, 6)))
                        ),
                        (
                            TokenKey::String(String::from("k2"), TokenRange::from((s, 8, 10))),
                            vec![],
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 12, 13)))
                        ),
                    ],
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"[]"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(2..),
                Token::TokenFrameList(vec![], TokenRange::from((s, 0, 2)))
            ))
        );

        let s = Span::from(r#"[1]"#);
        assert_eq!(
            expr_unit(s),
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

        let s = Span::from(r#"[1, 2]"#);
        assert_eq!(
            expr_unit(s),
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
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 4, 5)))
                        )
                    ],
                    TokenRange::from((s, 0, 6))
                )
            ))
        );

        let s = Span::from(r#"(1 + 2)"#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(7..),
                Token::TokenFrameExprGroup(
                    Box::new(Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2))),
                            Token::TokenOpAdd(TokenRange::from((s, 3, 4))),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                        ],
                        TokenRange::from((s, 1, 6))
                    )),
                    TokenRange::from((s, 0, 7))
                )
            ))
        );

        let s = Span::from(r#"f"hello ${name}!""#);
        assert_eq!(
            expr_unit(s),
            Ok((
                s.slice(17..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimString("hello ".to_string(), TokenRange::from((s, 2, 8))),
                        Token::TokenDummyOpConcat,
                        Token::TokenVar(
                            vec![TokenKey::String(
                                "name".to_string(),
                                TokenRange::from((s, 10, 14))
                            )],
                            TokenRange::from((s, 10, 14))
                        ),
                        Token::TokenDummyOpConcat,
                        Token::TokenPrimString("!".to_string(), TokenRange::from((s, 15, 16))),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );
    }

    #[test]
    fn test_parse_expr() {
        let s = Span::from(r#"1"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(1..),
                Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1)))
            ))
        );

        let s = Span::from(r#"true"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4)))
            ))
        );

        let s = Span::from(r#"1 + 2"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(5..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1))),
                        Token::TokenOpAdd(TokenRange::from((s, 2, 3))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 4, 5)))
                    ],
                    TokenRange::from((s, 0, 5))
                )
            ))
        );

        let s = Span::from(r#"(1 + 2) * 3"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(11..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenFrameExprGroup(
                            Box::new(Token::TokenExprSequence(
                                vec![
                                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 1, 2))),
                                    Token::TokenOpAdd(TokenRange::from((s, 3, 4))),
                                    Token::TokenPrimNumberInt(2, TokenRange::from((s, 5, 6)))
                                ],
                                TokenRange::from((s, 1, 6))
                            )),
                            TokenRange::from((s, 0, 7))
                        ),
                        Token::TokenOpMul(TokenRange::from((s, 8, 9))),
                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 10, 11))),
                    ],
                    TokenRange::from((s, 0, 11))
                )
            ))
        );

        let s = Span::from(r#"1 + f(2, 3) * 3"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(15..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1))),
                        Token::TokenOpAdd(TokenRange::from((s, 2, 3))),
                        Token::TokenFnCall(
                            TokenId("f".to_string(), TokenRange::from((s, 4, 5))),
                            vec![
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 6, 7))),
                                Token::TokenPrimNumberInt(3, TokenRange::from((s, 9, 10)))
                            ],
                            TokenRange::from((s, 4, 11))
                        ),
                        Token::TokenOpMul(TokenRange::from((s, 12, 13))),
                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 14, 15))),
                    ],
                    TokenRange::from((s, 0, 15))
                )
            ))
        );

        let s = Span::from(r#"1 + f(2, 3) * var3"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(18..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1))),
                        Token::TokenOpAdd(TokenRange::from((s, 2, 3))),
                        Token::TokenFnCall(
                            TokenId("f".to_string(), TokenRange::from((s, 4, 5))),
                            vec![
                                Token::TokenPrimNumberInt(2, TokenRange::from((s, 6, 7))),
                                Token::TokenPrimNumberInt(3, TokenRange::from((s, 9, 10)))
                            ],
                            TokenRange::from((s, 4, 11))
                        ),
                        Token::TokenOpMul(TokenRange::from((s, 12, 13))),
                        Token::TokenVar(
                            vec![TokenKey::String(
                                String::from("var3"),
                                TokenRange::from((s, 14, 18))
                            )],
                            TokenRange::from((s, 14, 18))
                        ),
                    ],
                    TokenRange::from((s, 0, 18))
                )
            ))
        );

        let s = Span::from(r#"1 + 2 * 3 / 4 % 5"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(17..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 0, 1))),
                        Token::TokenOpAdd(TokenRange::from((s, 2, 3))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 4, 5))),
                        Token::TokenOpMul(TokenRange::from((s, 6, 7))),
                        Token::TokenPrimNumberInt(3, TokenRange::from((s, 8, 9))),
                        Token::TokenOpDiv(TokenRange::from((s, 10, 11))),
                        Token::TokenPrimNumberInt(4, TokenRange::from((s, 12, 13))),
                        Token::TokenOpMod(TokenRange::from((s, 14, 15))),
                        Token::TokenPrimNumberInt(5, TokenRange::from((s, 16, 17))),
                    ],
                    TokenRange::from((s, 0, 17))
                )
            ))
        );
    }

    #[test]
    fn test_line_comment_in_expr_zone() {
        let s = Span::from("// comment\n");
        assert_eq!(
            line_comment_in_expr_zone(s),
            Ok((s.slice(10..), s.slice(3..10)))
        );

        let s = Span::from("// comment\nworld");
        assert_eq!(
            line_comment_in_expr_zone(s),
            Ok((s.slice(10..), s.slice(3..10)))
        );

        let s = Span::from(
            r#"// comment
        1"#,
        );
        assert_eq!(
            line_comment_in_expr_zone(s),
            Ok((s.slice(10..), s.slice(3..10)))
        );

        let s = Span::from(
            r#"//TEST
        1}"#,
        );
        assert_eq!(
            line_comment_in_expr_zone(s),
            Ok((s.slice(6..), s.slice(2..6)))
        );

        let s = Span::from(r#"// comment } out"#);
        assert_eq!(
            line_comment_in_expr_zone(s),
            Ok((s.slice(11..), s.slice(3..11)))
        );
    }

    #[test]
    fn test_more_comment_in_expr_zone() {
        let s = Span::from(r#"/* comment } */out"#);
        assert_eq!(
            space_or_comment_in_expr_zone(s),
            Ok((s.slice(15..), s.slice(3..12)))
        );

        let s = Span::from(r#"${/* TEST */1}"#);
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(14..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 12, 13))),
                    TokenRange::from((s, 0, 14))
                )
            ))
        );

        let s = Span::from(r#"${/* TEST */1//line comment}"#);
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(28..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 12, 13))),
                    TokenRange::from((s, 0, 28))
                )
            ))
        );

        let s = Span::from(r#"${/* TEST1 */1/* TEST2 */}"#);
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(26..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 13, 14))),
                    TokenRange::from((s, 0, 26))
                )
            ))
        );

        let s = Span::from(
            r#"${// TEST
        1 /* TEST3 */}"#,
        );
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(32..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 18, 19))),
                    TokenRange::from((s, 0, 32))
                )
            ))
        );

        let s = Span::from(r#"${ 1 /* TEST */}"#);
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(16..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 3, 4))),
                    TokenRange::from((s, 0, 16))
                )
            ))
        );
    }

    #[test]
    fn test_parse_expr_zone() {
        let s = Span::from("${}");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(3..), (Token::Void, TokenRange::from((s, 0, 3)))))
        );

        let s = Span::from("${ }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(4..), (Token::Void, TokenRange::from((s, 0, 4)))))
        );

        let s = Span::from("${ 1 }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(6..),
                (
                    Token::TokenPrimNumberInt(1, TokenRange::from((s, 3, 4))),
                    TokenRange::from((s, 0, 6))
                )
            ))
        );

        let s = Span::from("${ 1 + 2 }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(10..),
                (
                    Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 3, 4))),
                            Token::TokenOpAdd(TokenRange::from((s, 5, 6))),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 7, 8)))
                        ],
                        TokenRange::from((s, 3, 8))
                    ),
                    TokenRange::from((s, 0, 10))
                )
            ))
        );

        let s = Span::from("${ 1 + 2 * 3 }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((
                s.slice(14..),
                (
                    Token::TokenExprSequence(
                        vec![
                            Token::TokenPrimNumberInt(1, TokenRange::from((s, 3, 4))),
                            Token::TokenOpAdd(TokenRange::from((s, 5, 6))),
                            Token::TokenPrimNumberInt(2, TokenRange::from((s, 7, 8)), ),
                            Token::TokenOpMul(TokenRange::from((s, 9, 10))),
                            Token::TokenPrimNumberInt(3, TokenRange::from((s, 11, 12)), ),
                        ],
                        TokenRange::from((s, 3, 12))
                    ),
                    TokenRange::from((s, 0, 14))
                )
            ))
        );
    }

    #[test]
    fn test_expr_zero() {
        assert!(operator(Span::from("")).is_err());
        assert!(group(Span::from("")).is_err());
        assert!(expr_unit(Span::from("")).is_err());
        assert!(parse_expr(Span::from("")).is_err());
        assert!(parse_expr_zone(Span::from("")).is_err());

        let s = Span::from("${}");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(3..), (Token::Void, TokenRange::from((s, 0, 3)))))
        );

        let s = Span::from("${ }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(4..), (Token::Void, TokenRange::from((s, 0, 4)))))
        );

        let s = Span::from("${// comment\n}");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(14..), (Token::Void, TokenRange::from((s, 0, 14)))))
        );

        let s = Span::from("${// comment\n }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(15..), (Token::Void, TokenRange::from((s, 0, 15)))))
        );

        let s = Span::from("${// comment\n// comment\n}");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(25..), (Token::Void, TokenRange::from((s, 0, 25)))))
        );

        let s = Span::from(r#"${// comment } out"#);
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(14..), (Token::Void, TokenRange::from((s, 0, 14)))))
        );

        let s = Span::from(
            r#"${
            // comment
        }"#,
        );
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(35..), (Token::Void, TokenRange::from((s, 0, 35)))))
        );

        let s = Span::from("${}");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(3..), (Token::Void, TokenRange::from((s, 0, 3)))))
        );

        let s = Span::from("${ }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(4..), (Token::Void, TokenRange::from((s, 0, 4)))))
        );

        let s = Span::from("${ // nothing\n }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(16..), (Token::Void, TokenRange::from((s, 0, 16)))))
        );

        let s = Span::from("${ // nothing }");
        assert_eq!(
            parse_expr_zone(s),
            Ok((s.slice(15..), (Token::Void, TokenRange::from((s, 0, 15)))))
        );
    }

    #[test]
    fn test_expr_ternary() {
        let s = Span::from(r#"true ? 1 : 2"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(12..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4))),
                        Token::TokenOpQ(TokenRange::from((s, 5, 6))),
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8))),
                        Token::TokenOpColon(TokenRange::from((s, 9, 10))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 11, 12))),
                    ],
                    TokenRange::from((s, 0, 12))
                )
            ))
        );

        let s = Span::from(r#"true ? 1: 2"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(11..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4))),
                        Token::TokenOpQ(TokenRange::from((s, 5, 6))),
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 7, 8))),
                        Token::TokenOpColon(TokenRange::from((s, 8, 9))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 10, 11))),
                    ],
                    TokenRange::from((s, 0, 11))
                )
            ))
        );

        let s = Span::from(r#"true? 1:2"#);
        assert_eq!(
            parse_expr(s),
            Ok((
                s.slice(9..),
                Token::TokenExprSequence(
                    vec![
                        Token::TokenPrimBoolean(true, TokenRange::from((s, 0, 4))),
                        Token::TokenOpQ(TokenRange::from((s, 4, 5))),
                        Token::TokenPrimNumberInt(1, TokenRange::from((s, 6, 7))),
                        Token::TokenOpColon(TokenRange::from((s, 7, 8))),
                        Token::TokenPrimNumberInt(2, TokenRange::from((s, 8, 9))),
                    ],
                    TokenRange::from((s, 0, 9))
                )
            ))
        );
    }
}
