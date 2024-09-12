use nom::branch::alt;
use nom::character::complete::multispace1;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::IResult;

use crate::comments::comment;
use crate::Span;

/// soc0: zero or more spaces or comments
pub(crate) fn soc0(i: Span) -> IResult<Span, Vec<Span>, VerboseError<Span>> {
    // let chars = " \t\r\n";
    // take_while(move |c| chars.contains(c))(input)
    let (remaining, r) = many0(alt((multispace1, comment)))(i)?;
    Ok((remaining, r))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Slice;


    #[test]
    fn test_sp() {
        let s = Span::from(r#"  "#);
        assert_eq!(soc0(s), Ok((s.slice(2..), vec![s.slice(0..2)])));

        let s = Span::from(" \t\r\n");
        assert_eq!(soc0(s), Ok((s.slice(4..), vec![s.slice(0..4)])));

        let s = Span::from(r#""#);
        assert_eq!(soc0(s), Ok((s, vec![])));

        let s = Span::from(
            r##"// comment1
        {} // comment2"##,
        );
        assert_eq!(
            soc0(s),
            Ok((s.slice(20..), vec![s.slice(3..11), s.slice(12..20)])) // rest: "{} // comment2", match: ["// comment1", "\n        "]
        );
    }
}
