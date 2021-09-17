use nom::{
    branch::alt,
    character::complete::{alphanumeric1, char},
    combinator::{not, recognize, value},
    sequence::terminated,
    IResult,
};

pub fn ident_char(input: &str) -> IResult<&str, &str> {
    recognize(alt((value((), alphanumeric1), value((), char('_')))))(input)
}

pub fn isolated_tag(tag: &'static str) -> impl FnMut(&str) -> IResult<&str, &str> {
    move |i| terminated(nom::bytes::complete::tag(tag), not(ident_char))(i)
}
