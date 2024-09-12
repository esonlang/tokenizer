use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{line_ending, multispace0, not_line_ending};
use nom::combinator::{eof, recognize};
use nom::error::{context, VerboseError};
use nom::multi::many_till;
use nom::sequence::{pair, preceded, terminated};
use nom::{IResult, Slice};

use crate::Span;

pub(crate) fn comment(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    let (remaining, c) = context(
        "comment",
        alt((
            context("block_comment", block_comment),
            context("line_comment", line_comment),
        )),
    )(i)?;
    Ok((remaining, c))
}

pub(crate) fn block_comment(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    let (remaining, head) = recognize(pair(tag("/*"), multispace0))(i)?;
    let tail = pair(multispace0, tag("*/"));
    let (remaining, (body, _)) = many_till(take(1u8), tail)(remaining)?;

    let offset = head.fragment().len();
    let length = body.len();
    Ok((remaining, i.slice(offset..offset + length)))
}

fn line_comment(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
    let (remaining, _) = tag("//")(i)?;
    let (remaining, c) = preceded(
        multispace0,
        terminated(recognize(not_line_ending), alt((line_ending, eof))),
    )(remaining)?;
    Ok((remaining, c))
}

#[cfg(test)]
mod tests {
    use nom::multi::many0;
    use nom::sequence::pair;
    use nom::Slice;

    use super::*;

    #[test]
    fn test_comment() {
        let s = Span::from(
            r#"/*
            block_comment
        */"#,
        );
        assert_eq!(comment(s), Ok((s.slice(39..), s.slice(15..28))));

        let s = Span::from("// line_comment\n");
        assert_eq!(comment(s), Ok((s.slice(16..), s.slice(3..15))));
    }

    #[test]
    fn test_line_ending() {
        fn parse_line(i: Span) -> IResult<Span, Span, VerboseError<Span>> {
            alt((line_ending, eof))(i)
        }

        let s = Span::from("\n");
        assert_eq!(parse_line(s), Ok((s.slice(1..), s.slice(0..1))));

        let s = Span::from("\r\n");
        assert_eq!(parse_line(s), Ok((s.slice(2..), s.slice(0..2))));

        let s = Span::from(r#""#);
        assert_eq!(parse_line(s), Ok((s.slice(0..), s.slice(0..0))));
    }

    #[test]
    fn test_block_comment() {
        let s = Span::from("/* hello */");
        assert_eq!(block_comment(s), Ok((s.slice(11..), s.slice(3..8))));

        let s = Span::from("/* hello");
        assert!(block_comment(s).is_err());

        let s = Span::from("/* hello\n");
        assert!(block_comment(s).is_err());

        let s = Span::from("/* hello\nworld */");
        assert_eq!(block_comment(s), Ok((s.slice(17..), s.slice(3..14))));

        let s = Span::from("/* hello\nworld");
        assert!(block_comment(s).is_err());

        let s = Span::from("/* hello\nworld\n*/");
        assert_eq!(block_comment(s), Ok((s.slice(17..), s.slice(3..14))));

        let s = Span::from("/*hello world*/");
        assert_eq!(block_comment(s), Ok((s.slice(15..), s.slice(2..13))));
    }

    #[test]
    fn test_line_comment() {
        let s = Span::from("// hello\n");
        assert_eq!(line_comment(s), Ok((s.slice(9..), s.slice(3..8))));

        let s = Span::from(r#"// hello"#);
        assert_eq!(line_comment(s), Ok((s.slice(8..), s.slice(3..))));

        let s = Span::from(
            r#"// hello
        @world"#,
        );
        assert_eq!(line_comment(s), Ok((s.slice(9..), s.slice(3..8))));

        let s = Span::from(
            r##"// comment1
        // comment2
        {}"##,
        );
        assert_eq!(
            many0(pair(multispace0, line_comment))(s),
            Ok((
                s.slice(32..),
                vec![
                    (s.slice(..0), s.slice(3..11)),
                    (s.slice(12..20), s.slice(23..31))
                ]
            ))
        );
    }
}
