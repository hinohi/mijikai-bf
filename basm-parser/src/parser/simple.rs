use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{opt, recognize, value},
    multi::many0,
    sequence::{pair, preceded, tuple},
    IResult,
};

pub fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char('#'), is_not("\r\n"))(input)
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub fn number(input: &str) -> IResult<&str, i32> {
    use nom::error;
    let (i, s) = recognize(alt((
        value((), char('0')),
        value(
            (),
            tuple((
                opt(char('-')),
                one_of("123456789"),
                many0(one_of("1234567890")),
            )),
        ),
    )))(input)?;
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
