use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take_while1};
use nom::character::complete::char as ch;
use nom::combinator::{map, opt};
use nom::error::VerboseError;
use nom::sequence::{pair, preceded};

use crate::sp::soc0;
use crate::token::TokenRange;
use crate::{Span, Token};

fn parse_hex(i: Span) -> nom::IResult<Span, (Option<char>, Span, TokenRange), VerboseError<Span>> {
    let (remaining, sign) = opt(alt((ch('+'), ch('-'))))(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, _) = tag("0x")(remaining)?;
    let (remaining, dat) = take_while1(|c: char| c.is_digit(16))(remaining)?;
    Ok((remaining, (sign, dat, TokenRange::from((i, remaining)))))
}

fn parse_oct(i: Span) -> nom::IResult<Span, (Option<char>, Span, TokenRange), VerboseError<Span>> {
    let (remaining, sign) = opt(alt((ch('+'), ch('-'))))(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, _) = tag("0o")(remaining)?;
    let (remaining, dat) = take_while1(|c: char| c.is_digit(8))(remaining)?;
    Ok((remaining, (sign, dat, TokenRange::from((i, remaining)))))
}

fn parse_bin(i: Span) -> nom::IResult<Span, (Option<char>, Span, TokenRange), VerboseError<Span>> {
    let (remaining, sign) = opt(alt((ch('+'), ch('-'))))(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, _) = tag("0b")(remaining)?;
    let (remaining, dat) = take_while1(|c: char| c.is_digit(2))(remaining)?;
    Ok((remaining, (sign, dat, TokenRange::from((i, remaining)))))
}

fn parse_dec(
    i: Span,
) -> nom::IResult<
    Span,
    (
        Option<char>,                 // sign +, -
        Span,                         // int part
        Option<Span>,                 // fractional part
        Option<(Option<char>, Span)>, // exp part( sign, exp )
        TokenRange,                   // token range
    ),
    VerboseError<Span>,
> {
    let (remaining, sign) = opt(alt((ch('+'), ch('-'))))(i)?;
    let (remaining, _) = soc0(remaining)?;
    let (remaining, int_part) = take_while1(|c: char| c.is_digit(10))(remaining)?;
    let (remaining, fractional_part) =
        opt(preceded(ch('.'), take_while1(|c: char| c.is_digit(10))))(remaining)?;
    let (remaining, exp_part) = opt(preceded(
        tag_no_case("e"),
        pair(
            opt(alt((ch('+'), ch('-')))),
            take_while1(|c: char| c.is_digit(10)),
        ),
    ))(remaining)?;
    Ok((
        remaining,
        (
            sign,
            int_part,
            fractional_part,
            exp_part,
            TokenRange::from((i, remaining)),
        ),
    ))
}

pub(crate) fn parse_number(i: Span) -> nom::IResult<Span, Token, VerboseError<Span>> {
    fn _dec_str_to_f64(sign: Option<char>, int_part: Span, fractional_part: Option<Span>) -> f64 {
        let num_str = format!(
            "{}{}{}",
            sign.unwrap_or('+'),
            int_part.fragment(),
            fractional_part.map_or(String::from(""), |d| format!(".{}", d.fragment()))
        );
        let f = num_str
            .parse::<f64>()
            .unwrap_or_else(|e| panic!("parse::<f64>() err: {}", e)); // todo!("error handling");
        f
    }

    let (remaining, _) = soc0(i)?;
    alt((
        map(parse_bin, |(sign, dat, range)| {
            let num_str = format!("{}{}", sign.unwrap_or('+'), dat.fragment());
            let i = i64::from_str_radix(num_str.as_str(), 2)
                .unwrap_or_else(|e| panic!("parse<i64> from_str_radix() err: {}", e)); // todo!("error handling");
            Token::TokenPrimNumberInt(i, range)
        }),
        map(parse_oct, |(sign, dat, range)| {
            let num_str = format!("{}{}", sign.unwrap_or('+'), dat.fragment());
            let i = i64::from_str_radix(num_str.as_str(), 8)
                .unwrap_or_else(|e| panic!("parse<i64> from_str_radix() err: {}", e)); // todo!("error handling");
            Token::TokenPrimNumberInt(i, range)
        }),
        map(parse_hex, |(sign, dat, range)| {
            let num_str = format!("{}{}", sign.unwrap_or('+'), dat.fragment());
            let i = i64::from_str_radix(num_str.as_str(), 16)
                .unwrap_or_else(|e| panic!("parse<i64> from_str_radix() err: {}", e)); // todo!("error handling");
            Token::TokenPrimNumberInt(i, range)
        }),
        map(
            parse_dec,
            |(sign, int_part, fractional_part, exp_part, range)| {
                if fractional_part.is_none() && exp_part.is_none() {
                    let num_str = format!("{}{}", sign.unwrap_or('+'), int_part.fragment());
                    // no decimal point or exponent => integer
                    let i = num_str
                        .parse::<i64>()
                        .unwrap_or_else(|e| panic!("parse::<i64>() err: {}", e));
                    Token::TokenPrimNumberInt(i, range)
                } else {
                    let num_str = format!(
                        "{}{}{}{}",
                        sign.unwrap_or('+'),
                        int_part.fragment(),
                        fractional_part.map_or(String::from(""), |d| format!(".{}", d.fragment())),
                        exp_part.map_or(String::from(""), |(exp_sign, exp)| {
                            format!("e{}{}", exp_sign.unwrap_or('+'), exp.fragment())
                        })
                    );
                    let f = num_str
                        .parse::<f64>()
                        .unwrap_or_else(|e| panic!("parse::<f64>() err: {}", e)); // todo!("error handling");
                    Token::TokenPrimNumberFloat(f, range)
                }
            },
        ),
        map(tag("Infinity"), |s| {
            Token::TokenPrimNumberFloat(f64::INFINITY, TokenRange::from((s, 8)))
        }),
        map(tag("-Infinity"), |s| {
            Token::TokenPrimNumberFloat(f64::NEG_INFINITY, TokenRange::from((s, 9)))
        }),
        map(tag("NaN"), |s| {
            Token::TokenPrimNumberFloat(f64::NAN, TokenRange::from((s, 3)))
        }),
    ))(remaining)
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use super::*;

    #[test]
    fn test_special_symbol() {
        assert!(parse_number(Span::from("Infinity")).is_ok());
        assert!(parse_number(Span::from("-Infinity")).is_ok());
        assert!(parse_number(Span::from("NaN")).is_ok());

        let s = Span::from(r#"Infinity"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(8..),
                Token::TokenPrimNumberFloat(f64::INFINITY, TokenRange::from((s, 0, 8)))
            ))
        );

        let s = Span::from(r#"-Infinity"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(9..),
                Token::TokenPrimNumberFloat(f64::NEG_INFINITY, TokenRange::from((s, 0, 9)))
            ))
        );
    }

    #[test]
    fn test_parse_hex() {
        assert!(parse_hex(Span::from("0x")).is_err());
        assert!(parse_hex(Span::from("0xh")).is_err());

        let s = Span::from(r#"0x1af"#);
        assert_eq!(
            parse_hex(s),
            Ok((
                s.slice(5..),
                (None, s.slice(2..), TokenRange::from((s, 0, 5)))
            ))
        );
    }

    #[test]
    fn test_parse_oct() {
        assert!(parse_oct(Span::from("0o")).is_err());
        assert!(parse_oct(Span::from("0oh")).is_err());

        let s = Span::from(r#"0o123"#);
        assert_eq!(
            parse_oct(s),
            Ok((
                // s.slice(5..),
                s.slice(5..),
                (
                    None,
                    s.slice(2..),
                    // s.slice(2..),
                    TokenRange::from((s, 0, 5))
                )
            ))
        );
    }

    #[test]
    fn test_parse_bin() {
        assert!(parse_bin(Span::from("0b")).is_err());
        assert!(parse_bin(Span::from("0bh")).is_err());

        let s = Span::from(r#"0b101"#);
        assert_eq!(
            parse_bin(s),
            Ok((
                s.slice(5..),
                (None, s.slice(2..), TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"-0b1001"#);
        assert_eq!(
            parse_bin(s),
            Ok((
                s.slice(7..),
                (
                    // Some(s.slice(0..)),
                    Some('-'),
                    s.slice(3..),
                    TokenRange::from((s, 0, 7))
                )
            ))
        );
    }

    #[test]
    fn test_parse_dec() {
        assert!(parse_dec(Span::from("")).is_err());

        let s = Span::from(r#"1"#);
        assert_eq!(
            parse_dec(s),
            Ok((
                s.slice(1..),
                (None, s.slice(0..), None, None, TokenRange::from((s, 0, 1)))
            ))
        );

        let s = Span::from(r#"1.0"#);
        assert_eq!(
            parse_dec(s),
            Ok((
                s.slice(3..),
                (
                    None,
                    s.slice(0..1),
                    Some(s.slice(2..)),
                    None,
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"1e1"#);
        assert_eq!(
            parse_dec(s),
            Ok((
                s.slice(3..),
                (
                    None,
                    s.slice(0..1),
                    None,
                    Some((None, s.slice(2..))),
                    TokenRange::from((s, 0, 3))
                )
            ))
        );

        let s = Span::from(r#"1e-1"#);
        assert_eq!(
            parse_dec(s),
            Ok((
                s.slice(4..),
                (
                    None,
                    s.slice(0..1),
                    None,
                    Some((Some('-'), s.slice(3..))),
                    TokenRange::from((s, 0, 4))
                )
            ))
        );

        let s = Span::from(r#"1.0e+1"#);
        assert_eq!(
            parse_dec(s),
            Ok((
                s.slice(6..),
                (
                    None,
                    s.slice(0..1),
                    Some(s.slice(2..3)),
                    Some((Some('+'), s.slice(5..))),
                    TokenRange::from((s, 0, 6))
                )
            ))
        );
    }

    #[test]
    fn test_parse_number_hex() {
        let s = Span::from(r#"0x123"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimNumberInt(291, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"-0x123"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-291, TokenRange::from((s, 0, 6)))
            ))
        );
    }

    #[test]
    fn test_parse_number_oct() {
        let s = Span::from(r#"0o777"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimNumberInt(511, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"-0o777"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-511, TokenRange::from((s, 0, 6)))
            ))
        );
    }

    #[test]
    fn test_parse_number_bin() {
        let s = Span::from(r#"0b101"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimNumberInt(5, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"-0b101"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-5, TokenRange::from((s, 0, 6)))
            ))
        );
    }

    #[test]
    fn test_e() {
        let s = Span::from(r#"1.0"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimNumberFloat(1.0, TokenRange::from((s, 0, 3)))
            ))
        );

        let s = Span::from(r#"1.0e1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimNumberFloat(10.0, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"1.0e-1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberFloat(0.1, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"1.0e+1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberFloat(10.0, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"1.0E1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(5..),
                Token::TokenPrimNumberFloat(10.0, TokenRange::from((s, 0, 5)))
            ))
        );

        let s = Span::from(r#"1.0E-1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberFloat(0.1, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"1.0E+1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberFloat(10.0, TokenRange::from((s, 0, 6)))
            ))
        );
    }

    #[test]
    fn test_more_base() {
        let n = Token::TokenPrimNumberInt(0b101, TokenRange::from((Span::from("0b101"), 0, 5)));
        assert_eq!(format!("{}", n), "5");

        let n = Token::TokenPrimNumberInt(0o101, TokenRange::from((Span::from("0o101"), 0, 5)));
        assert_eq!(format!("{}", n), "65");

        let n = Token::TokenPrimNumberInt(0x101, TokenRange::from((Span::from("0x101"), 0, 5)));
        assert_eq!(format!("{}", n), "257");

        let s = Span::from(r#"0b1010"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(10, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"123.456"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(7..),
                Token::TokenPrimNumberFloat(123.456, TokenRange::from((s, 0, 7)))
            ))
        );

        let s = Span::from(r#"123.456e-10"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(11..),
                Token::TokenPrimNumberFloat(0.0000000123456, TokenRange::from((s, 0, 11)))
            ))
        );

        let s = Span::from(r#"123.456e+10"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(11..),
                Token::TokenPrimNumberFloat(1234560000000.0, TokenRange::from((s, 0, 11)))
            ))
        );

        let s = Span::from(r#"123.456e10"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(10..),
                Token::TokenPrimNumberFloat(1234560000000.0, TokenRange::from((s, 0, 10)))
            ))
        );

        let s = Span::from(r#"123"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(3..),
                Token::TokenPrimNumberInt(123, TokenRange::from((s, 0, 3)))
            ))
        );

        let s = Span::from(r#" 123"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNumberInt(123, TokenRange::from((s.slice(1..), 3)))
            ))
        );
    }

    #[test]
    fn test_display() {
        let n = Token::TokenPrimNumberInt(1, TokenRange::from((Span::from("1"), 0, 1)));
        assert_eq!(format!("{}", n), "1");

        let n = Token::TokenPrimNumberFloat(1.0, TokenRange::from((Span::from("1.0"), 0, 3)));
        assert_eq!(format!("{}", n), "1");

        let n = Token::TokenPrimNumberFloat(1.5, TokenRange::from((Span::from("1.5"), 0, 3)));
        assert_eq!(format!("{}", n), "1.5");
    }

    #[test]
    fn test_ordered_float_e_eq() {
        let a = 1.0;
        let b = 1.0e0;
        assert_eq!(a == b, true);
    }

    #[test]
    fn test_neg() {
        let s = Span::from(r#"-0b101"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-5, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"-0o777"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-511, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"-0x123"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(6..),
                Token::TokenPrimNumberInt(-291, TokenRange::from((s, 0, 6)))
            ))
        );

        let s = Span::from(r#"-1"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(2..),
                Token::TokenPrimNumberInt(-1, TokenRange::from((s, 0, 2)))
            ))
        );

        let s = Span::from(r#"-1.0"#);
        assert_eq!(
            parse_number(s),
            Ok((
                s.slice(4..),
                Token::TokenPrimNumberFloat(-1.0, TokenRange::from((s, 0, 4)))
            ))
        );
    }
}
