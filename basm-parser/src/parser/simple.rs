use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char as char_parser, one_of, satisfy},
    combinator::{not, opt, recognize, value},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};

pub(super) fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char_parser('#'), is_not("\r\n"))(input)
}

pub(super) fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub(super) fn number(input: &str) -> IResult<&str, i32> {
    use nom::error;
    let (i, s) = terminated(
        recognize(alt((
            value((), char_parser('0')),
            value(
                (),
                tuple((
                    opt(char_parser('-')),
                    one_of("123456789"),
                    many0(one_of("1234567890")),
                )),
            ),
        ))),
        not(one_of("1234567890")),
    )(input)?;
    let n = match s.parse() {
        Ok(n) => n,
        Err(_) => {
            return Err(nom::Err::Error(error::Error::new(
                input,
                error::ErrorKind::Satisfy,
            )))
        }
    };
    Ok((i, n))
}

pub(super) fn character(input: &str) -> IResult<&str, u8> {
    let (input, _) = char_parser('\'')(input)?;
    let (input, slash) = opt(char_parser('\\'))(input)?;
    let (input, c) = if slash.is_some() {
        one_of("nrt\\0'")(input)?
    } else {
        satisfy(|c| c as u32 <= 0x7F)(input)?
    };
    let (input, _) = char_parser('\'')(input)?;
    Ok((input, c as u8))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number() {
        assert_eq!(number("123"), Ok(("", 123)));
        assert_eq!(number("-987654"), Ok(("", -987654)));
        assert_eq!(number("0"), Ok(("", 0)));
        assert!(number("01").is_err());
    }

    #[test]
    fn test_character() {
        assert_eq!(character("'a'"), Ok(("", b'a')));
        assert_eq!(character("'\\\\'"), Ok(("", b'\\')));
        assert!(character("'🍺'").is_err());
    }
}
