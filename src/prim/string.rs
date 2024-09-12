use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while_m_n};
use nom::character::complete::{char as ch, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::error::{context, VerboseError};
use nom::multi::{count, fold_many0, many0, many_till};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Slice};

use crate::token::{Token, TokenRange};
use crate::Span;

#[derive(Debug, PartialEq)]
enum Fragment<'a> {
    Literal(&'a str, TokenRange),
    EscapedChar(char, TokenRange),
    EscapedWS(TokenRange),
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
pub(crate) fn parse_escaped_char(i: Span) -> IResult<Span, (char, TokenRange), VerboseError<Span>> {
    fn parse_unicode(i: Span) -> IResult<Span, char, VerboseError<Span>> {
        let parse_1_to_6_hex_num = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
        let parse_4_hex_num = take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit());

        // eg: u{1F601} => 1F601
        //     uFE0F => FE0F
        let parse_prefixed_hex = preceded(
            ch('u'),
            alt((
                delimited(ch('{'), parse_1_to_6_hex_num, ch('}')),
                parse_4_hex_num,
            )),
        );

        map_opt(
            map_res(parse_prefixed_hex, move |hex: Span| {
                u32::from_str_radix(hex.fragment(), 16)
            }),
            |value| std::char::from_u32(value),
        )(i)
    }

    let (remaining, c) = preceded(
        ch('\\'),
        alt((
            parse_unicode,
            value('\n', ch('n')),
            value('\r', ch('r')),
            value('\t', ch('t')),
            value('\u{08}', ch('b')),
            value('\u{0C}', ch('f')),
            value('\\', ch('\\')),
            value('/', ch('/')),
            value('"', ch('"')),
        )),
    )(i)?;

    // dbg!(c, i, remaining);
    Ok((remaining, (c, TokenRange::from((i, remaining)))))
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
pub(crate) fn parse_escaped_whitespace(i: Span) -> IResult<Span, TokenRange, VerboseError<Span>> {
    let (remaining, _) = preceded(ch('\\'), multispace1)(i)?;
    Ok((remaining, TokenRange::from((i, remaining))))
}

fn parse_literal(i: Span) -> IResult<Span, (&str, TokenRange), VerboseError<Span>> {
    let (remaining, s) = verify(is_not(r#""\"#), |s: &Span| !s.fragment().is_empty())(i)?;
    Ok((remaining, (s.fragment(), TokenRange::from((i, remaining)))))
}

/// parse an empty string, e.g. "",
/// must pay your attention that this parser will match any string, so it should be placed to last
/// if there are other string parsers
fn empty_string(i: Span) -> IResult<Span, (String, TokenRange), VerboseError<Span>> {
    let (remaining, _) = tag("")(i)?;
    Ok((
        remaining,
        ("".to_string(), TokenRange::from((i, remaining))),
    ))
}

fn string(i: Span) -> IResult<Span, (String, TokenRange), VerboseError<Span>> {
    let parse_fragment = alt((
        map(parse_literal, |(s, r)| Fragment::Literal(s, r)),
        map(parse_escaped_char, |(c, r)| Fragment::EscapedChar(c, r)),
        map(parse_escaped_whitespace, |r| Fragment::EscapedWS(r)),
    ));
    let (remaining, s) = many0(parse_fragment)(i)?;
    let rst = s.iter().fold(String::new(), |mut string, fragment| {
        match fragment {
            Fragment::Literal(s, _) => string.push_str(s),
            Fragment::EscapedChar(c, _) => string.push(*c),
            _ => {}
        }
        string
    });
    Ok((remaining, (rst, TokenRange::from((i, remaining)))))
}

/// a string snippet that starts with # and ends with # repeated hash_count times
/// e.g.: #hello#, ##hello##, ###hello###, etc.
/// the function returns a Span that point to the body slice of the input snippet
pub(crate) fn hash_str_snippet(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    // Count the number of leading #
    let (remaining, hash_count) = fold_many0(tag("#"), || 0, |acc, _| acc + 1)(i)?;
    // Match " after leading #
    let (remaining, _) = tag(r#"""#)(remaining)?;
    // Take until closing "# (# repeated hash_count times)
    let closing = pair(tag("\""), count(tag("#"), hash_count));

    let (remaining, (inner, _)) = many_till(take(1u8), closing)(remaining)?;

    // Extract inner range
    let offset = hash_count + 1;
    let length = inner.len();

    Ok((remaining, i.slice(offset..offset + length)))
}

pub(crate) fn parse_string(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
    // " ... ", normal string
    fn _string(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
        let (remaining, (s, _)) = delimited(ch('"'), alt((string, empty_string)), ch('"'))(i)?;
        Ok((
            remaining,
            Token::TokenPrimString(s, TokenRange::from((i, remaining))),
        ))
    }
    // r#" ... "#, return row string just by String::from(hash_str_snippet)
    fn _raw_string(i: Span) -> IResult<Span, Token, VerboseError<Span>> {
        let (remaining, s) = preceded(ch('r'), hash_str_snippet)(i)?;
        Ok((
            remaining,
            Token::TokenPrimString(s.to_string(), TokenRange::from((i, remaining))),
        ))
    }

    context(
        "parse_string",
        alt((
            context("string", _string),
            context("raw_string", _raw_string),
        )),
    )(i)
}

#[cfg(test)]
mod tests {
    use nom::character::complete::char as ch;
    use nom::Slice;

    // use crate::SpanExt;

    use super::*;

    #[test]
    fn test_esc() {
        fn parse_br(i: Span) -> IResult<Span, char, VerboseError<Span>> {
            ch('\n')(i)
        }
        let s = Span::from("\n");
        assert_eq!(parse_br(s), Ok((s.slice(1..), '\n')));

        // parse_escaped_char
        let s = Span::from(r#"\n"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((s.slice(2..), ('\n', TokenRange::from((s, 0, 2)))))
        );
    }

    #[test]
    fn test_empty() {
        let s = Span::from(r#""""#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(2..),
                Token::TokenPrimString("".to_string(), TokenRange::from((s, 0, 2))),
            ))
        );

        let s = Span::from(r#""" "#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(2..),
                Token::TokenPrimString("".to_string(), TokenRange::from((s, 0, 2))),
            ))
        );

        let s = Span::from(r#"r"""#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimString("".to_string(), TokenRange::from((s, 0, 3))),
            ))
        );

        let s = Span::from(r#"r"" "#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimString("".to_string(), TokenRange::from((s, 0, 3)), )
            ))
        );

        let s = Span::from(r##"r#""#""##);
        assert_eq!(parse_string(s).unwrap().1.to_string().is_empty(), true);
    }

    #[test]
    fn test_parse_escaped_whitespace() {
        let s = Span::from(r#"\   "#);
        assert_eq!(
            parse_escaped_whitespace(s),
            Ok((s.slice(4..), TokenRange::from((s, 0, 4))))
        );

        let s = Span::from(
            r#"\
        "#,
        );
        assert_eq!(
            parse_escaped_whitespace(s),
            Ok((s.slice(10..), TokenRange::from((s, 0, 10))))
        );
    }

    #[test]
    fn test_parse_escaped_char() {
        let s = Span::from(r#"\n"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(2..),
                ('\n', TokenRange::from((Span::from(r#"\n"#), 2)))
            ))
        );

        let s = Span::from(r#"\t"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(2..),
                ('\t', TokenRange::from((Span::from(r#"\t"#), 2)))
            ))
        );

        let s = Span::from(r#"\r"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(2..),
                ('\r', TokenRange::from((Span::from(r#"\r"#), 2)))
            ))
        );

        let s = Span::from(r"\u{1F601}");
        assert_eq!(
            parse_escaped_char(s),
            Ok((s.slice(9..), ('😁', TokenRange::from((s, 9)))))
        );
        assert!(parse_escaped_char(Span::from(r#"\u{FE0F"#)).is_err(),);

        let s = Span::from(r#"\uFE0F"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(6..),
                ('\u{FE0F}', TokenRange::from((Span::from(r#"\uFE0F"#), 6)))
            ))
        );

        let s = Span::from(r#"\u{FE0F}"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(8..),
                ('\u{FE0F}', TokenRange::from((Span::from(r#"\u{FE0F}"#), 8)))
            ))
        );

        let s = Span::from(r#"\uFE0F"#);
        assert_eq!(
            parse_escaped_char(s),
            Ok((
                s.slice(6..),
                ('\u{FE0F}', TokenRange::from((Span::from(r#"\uFE0F"#), 6)))
            ))
        );
    }

    #[test]
    fn test_parse_literal() {
        let s = Span::from(r#"hello"#);
        assert_eq!(
            parse_literal(s),
            Ok((s.slice(5..), ("hello", TokenRange::from((s, 0, 5)))))
        );

        let s = Span::from(r#"hello""#);
        assert_eq!(
            parse_literal(s),
            Ok((s.slice(5..), ("hello", TokenRange::from((s, 0, 5)))))
        );

        let s = Span::from(r#"hello \n John"#);
        assert_eq!(
            parse_literal(s),
            Ok((s.slice(6..), ("hello ", TokenRange::from((s, 0, 6)))))
        );

        let s = Span::from(r#"hello \u{1F601}"#);
        assert_eq!(
            parse_literal(s),
            Ok((s.slice(6..), ("hello ", TokenRange::from((s, 0, 6)))))
        );

        let s = Span::from(r#"hello \u{FE0F}"#);
        assert_eq!(
            parse_literal(s),
            Ok((s.slice(6..), ("hello ", TokenRange::from((s, 0, 6)))))
        );
    }

    // if the string contains escaped character-combination, it should be parsed as an escaped char
    #[test]
    fn test_normal_string() {
        let s = Span::from(r#"hello"#);
        assert_eq!(
            string(s),
            Ok((
                s.slice(5..),
                ("hello".to_string(), TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"hello \n John"#);
        assert_eq!(
            string(s),
            Ok((
                s.slice(13..),
                ("hello \n John".to_string(), TokenRange::from((s, 0, 13)))
            ))
        );

        let s = Span::from(r#"hello \u{1F601} John"#);
        assert_eq!(
            string(s),
            Ok((
                s.slice(20..),
                ("hello 😁 John".to_string(), TokenRange::from((s, 0, 20)))
            ))
        );

        let s = Span::from(r#"hello \u{FE0F} John"#);
        assert_eq!(
            string(s),
            Ok((
                s.slice(19..),
                (
                    "hello \u{FE0F} John".to_string(),
                    TokenRange::from((s, 0, 19))
                )
            ))
        );
    }

    #[test]
    fn test_hash_string() {
        let s = Span::from(r#""hello \n John",}"#);
        assert_eq!(
            hash_str_snippet(s),
            Ok((
                s.slice(15..),
                s.slice(1..14) // (, TokenRange::from((s, 0, 15))),
            ))
        );

        let s = Span::from(r##"#"hello ${name}"#,}"##);
        assert_eq!(
            hash_str_snippet(s),
            Ok((
                s.slice(17..),
                s.slice(2..15) // (, TokenRange::from((s, 0, 17))),
            ))
        );

        let s = Span::from(r####"##"hello ${name}"##,}"####);
        assert_eq!(
            hash_str_snippet(s),
            Ok((
                s.slice(19..),
                s.slice(3..16) // (, TokenRange::from((s, 0, 19))),
            ))
        );
    }

    #[test]
    fn test_parse_string() {
        let s = Span::from(r#""John""#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimString("John".to_string(), TokenRange::from((s, 0, 6)))
            ))
        );

        assert!(parse_string(Span::from("\"John")).is_err());

        let s = Span::from(
            r#""hello \
        John""#,
        );
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(22..),
                Token::TokenPrimString("hello John".to_string(), TokenRange::from((s, 0, 22)))
            ))
        );

        let s = Span::from(r#"r"John""#);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(7..),
                Token::TokenPrimString("John".to_string(), TokenRange::from((s, 0, 7)))
            ))
        );

        let s = Span::from(r##"r#"John"#"##);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(9..),
                Token::TokenPrimString("John".to_string(), TokenRange::from((s, 0, 9)))
            ))
        );

        let s = Span::from(r###"r##"John"##"###);
        assert_eq!(
            parse_string(s),
            Ok((
                s.slice(11..),
                Token::TokenPrimString("John".to_string(), TokenRange::from((s, 0, 11)))
            ))
        );
    }
}
