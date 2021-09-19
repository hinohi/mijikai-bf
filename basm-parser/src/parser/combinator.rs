use nom::{
    branch::alt,
    character::complete::{alphanumeric1, char},
    combinator::{not, recognize, value},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};

use super::skip;

pub fn ident_char(input: &str) -> IResult<&str, &str> {
    recognize(alt((value((), alphanumeric1), value((), char('_')))))(input)
}

pub fn isolated_tag(tag: &'static str) -> impl FnMut(&str) -> IResult<&str, &str> {
    move |i| terminated(nom::bytes::complete::tag(tag), not(ident_char))(i)
}

pub fn ptrim<'a, O, T>(mut parser: T) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    T: FnMut(&'a str) -> IResult<&'a str, O>,
{
    move |i| {
        let (i, _) = skip(i)?;
        parser(i)
    }
}

pub fn trail_sep0<'a, P, O>(
    open: char,
    parser: P,
    sep: char,
    close: char,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> + Copy
where
    P: FnMut(&'a str) -> IResult<&'a str, O> + Copy,
{
    move |i| {
        let separator = tuple((skip, char(sep), skip));
        let trail = value((), pair(char(sep), ptrim(char(close))));
        delimited(
            char(open),
            separated_list0(separator, ptrim(parser)),
            ptrim(alt((value((), char(close)), trail))),
        )(i)
    }
}

pub fn trail_sep1<'a, P, O>(
    open: char,
    parser: P,
    sep: char,
    close: char,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>>
where
    P: FnMut(&'a str) -> IResult<&'a str, O> + Copy,
{
    move |i| {
        let separator = tuple((skip, char(sep), skip));
        let trail = value((), pair(char(sep), ptrim(char(close))));
        delimited(
            char(open),
            separated_list1(separator, ptrim(parser)),
            ptrim(alt((value((), char(close)), trail))),
        )(i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parser(input: &str) -> IResult<&str, &str> {
        nom::bytes::complete::tag("abc")(input)
    }

    #[test]
    fn test_trail_sep0() {
        fn p(input: &str) -> IResult<&str, Vec<&str>> {
            trail_sep0('(', parser, ',', ')')(input)
        }
        assert_eq!(p("()"), Ok(("", Vec::new())));
        assert_eq!(p("(abc)"), Ok(("", vec!["abc"])));
        assert_eq!(p("(abc,)"), Ok(("", vec!["abc"])));
        assert_eq!(p("(abc, abc)"), Ok(("", vec!["abc"; 2])));
        assert_eq!(
            p(r#"(
            abc,
            abc,
        )"#),
            Ok(("", vec!["abc"; 2]))
        );
    }

    #[test]
    fn test_trail_sep1() {
        fn p(input: &str) -> IResult<&str, Vec<&str>> {
            trail_sep1('(', parser, ',', ')')(input)
        }
        assert!(p("()").is_err());
        assert_eq!(p("(abc)"), Ok(("", vec!["abc"])));
        assert_eq!(p("(abc,)"), Ok(("", vec!["abc"])));
        assert_eq!(p("(abc, abc)"), Ok(("", vec!["abc"; 2])));
        assert_eq!(
            p(r#"(
            abc,
            abc,
        )"#),
            Ok(("", vec!["abc"; 2]))
        );
    }
}
