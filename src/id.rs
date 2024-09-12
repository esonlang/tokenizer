use nom::character::complete::one_of;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::IResult;

use crate::token::{TokenId, TokenRange};
use crate::Span;

// Resolve valid variable or function identifiers
// The identifier can contain only letters (a to z, A to Z), digits (0 to 9), and underscores (_).
// The first character of the identifier must be a letter or underscore
pub fn id(i: Span) -> IResult<Span, TokenId, VerboseError<Span>> {
    let (remaining, id) = nom::combinator::recognize(nom::sequence::pair(
        one_of("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        many0(one_of(
            "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
        )),
    ))(i)?;
    Ok((
        remaining,
        TokenId(
            id.fragment().to_string(),
            TokenRange::from((id, id.fragment().len())),
        ),
    ))
}

#[cfg(test)]
mod tests {
    use nom::Slice;

    use super::*;

    #[test]
    fn test_id() {
        assert!(id(Span::from("1name")).is_err());
        assert!(id(Span::from("-name")).is_err());
        assert!(id(Span::from("$name")).is_err());

        let s = Span::from(r#"your-name"#);
        assert_eq!(
            id(s),
            Ok((
                s.slice(4..),
                TokenId(
                    "your".to_string(),
                    TokenRange::from((Span::from("your"), 4)),
                ),
            ))
        );
        let s = Span::from(r#"name"#);
        assert_eq!(
            id(s),
            Ok((
                s.slice(4..),
                TokenId(
                    "name".to_string(),
                    TokenRange::from((Span::from("name"), 4)),
                ),
            ))
        );
        let s = Span::from(r#"Name1_"#);
        assert_eq!(
            id(s),
            Ok((
                s.slice(6..),
                TokenId(
                    "Name1_".to_string(),
                    TokenRange::from((Span::from("Name1_"), 6)),
                )
            ))
        );
    }
}
