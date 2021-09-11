use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, one_of},
    combinator::{eof, map, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::ast::{Def, Expr, FuncDef, StructDef, Term, Type, TypedDeclaration};
use nom::multi::separated_list1;

pub fn parse(input: &str) -> IResult<&str, Vec<Def>> {
    let (input, stmts) = many0(def)(input)?;
    let (input, _) = eof(input)?;
    Ok((input, stmts))
}

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char('#'), is_not("\r\n"))(input)
}

fn skip(input: &str) -> IResult<&str, ()> {
    let mut input = multispace0(input)?.0;
    loop {
        match comment(input) {
            Ok((i, _)) => input = i,
            Err(_) => break Ok((input, ())),
        }
        input = multispace0(input)?.0;
    }
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn def(input: &str) -> IResult<&str, Def> {
    map(struct_def, Def::StructDef)(input)
}

/// Parse struct definition
///
/// ```rust
/// # use basm_parser::struct_def;
/// let (input, s) = struct_def(r#"struct A {
///     a: Cell,
///     b: [C],
///     c: Cell,
/// }"#).unwrap();
/// assert_eq!(input, "");
/// assert_eq!(&s.name, "A");
/// ```
pub fn struct_def(input: &str) -> IResult<&str, StructDef> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("struct"), multispace1)(input)?;
    let (input, name) = identifier(input)?;
    let (input, fields) = comma_sp_fields('{', '}')(input)?;
    Ok((input, StructDef::new(name, fields)))
}

/// Parse function definition
///
/// ```rust
/// # use basm_parser::func_def;
/// assert!(func_def(r#"fn func(a: Cell, b: [C]) {
/// }"#).is_ok())
/// ```
pub fn func_def(input: &str) -> IResult<&str, FuncDef> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("fn"), multispace1)(input)?;
    let (input, name) = identifier(input)?;
    let (input, args) = comma_sp_fields('(', ')')(input)?;
    let (input, _) = skip(input)?;
    Ok((input, FuncDef::new(name, args, Vec::new())))
}

fn comma_sp_fields(
    open: char,
    close: char,
) -> impl FnMut(&str) -> IResult<&str, Vec<TypedDeclaration>> {
    move |i| {
        let trail_comma = value((), tuple((char(','), skip, char(close))));
        delimited(
            pair(skip, char(open)),
            separated_list0(pair(skip, char(',')), field),
            pair(skip, alt((value((), char(close)), trail_comma))),
        )(i)
    }
}

fn field(input: &str) -> IResult<&str, TypedDeclaration> {
    let (input, _) = skip(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char(':')(input)?;
    let (input, typ) = basm_type(input)?;
    Ok((input, TypedDeclaration::new(name, typ)))
}

fn basm_type(input: &str) -> IResult<&str, Type> {
    alt((
        map(preceded(skip, identifier), |s| Type::Value(s.to_string())),
        delimited(
            preceded(skip, char('[')),
            map(identifier, |s| Type::Array(s.to_string())),
            preceded(skip, char(']')),
        ),
    ))(input)
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    map(term, Expr::Term)(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((
        map(number, Term::Number),
        map(set, Term::Set),
        variable,
        preceded(char('*'), term),
    ))(input)
}

fn variable(input: &str) -> IResult<&str, Term> {
    let (mut input, ident) = identifier(input)?;
    let mut term = Term::Ident(ident.to_owned());
    while let Ok((i, c)) = one_of::<&str, &str, nom::error::Error<&str>>(".[")(input) {
        input = i;
        if c == '.' {
            let (i, attr) = identifier(i)?;
            term = Term::Attribute {
                target: Box::new(term),
                attr: attr.to_owned(),
            };
            input = i;
        } else {
            let (i, (index, _)) = pair(number, char(']'))(input)?;
            term = Term::Index {
                target: Box::new(term),
                index,
            };
            input = i;
        }
    }
    Ok((input, term))
}

fn set(input: &str) -> IResult<&str, Vec<Term>> {
    delimited(char('('), separated_list1(char('|'), term), char(')'))(input)
}

fn number(input: &str) -> IResult<&str, i32> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field() {
        assert_eq!(
            field("a: Cell"),
            Ok(("", TypedDeclaration::new("a", Type::value("Cell"))))
        );
        assert_eq!(
            field("_f: [A]"),
            Ok(("", TypedDeclaration::new("_f", Type::array("A"))))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(number("123"), Ok(("", 123)));
        assert_eq!(number("-987654"), Ok(("", -987654)));
        assert_eq!(number("0"), Ok(("", 0)));
    }

    #[test]
    fn test_term() {
        assert_eq!(term("1"), Ok(("", Term::Number(1))));
        assert_eq!(term("a {}"), Ok((" {}", Term::Ident("a".to_owned()))));
        assert_eq!(
            term("c.b.a"),
            Ok((
                "",
                Term::Attribute {
                    target: Box::new(Term::Attribute {
                        target: Box::new(Term::Ident("c".to_owned())),
                        attr: "b".to_owned(),
                    }),
                    attr: "a".to_owned(),
                }
            ))
        );
    }

    #[test]
    fn test_struct_two_fields() {
        let s = struct_def(
            r#"
        # This is Comment
        struct S0 {
            # A
            a: Cell,
            bbb: [O],
        }"#,
        );
        assert_eq!(
            s,
            Ok((
                "",
                StructDef::new(
                    "S0",
                    vec![
                        TypedDeclaration::new("a", Type::value("Cell")),
                        TypedDeclaration::new("bbb", Type::array("O")),
                    ]
                )
            ))
        )
    }

    #[test]
    fn test_struct_single_field() {
        let s = struct_def("struct S{a:A}");
        assert_eq!(
            s,
            Ok((
                "",
                StructDef::new("S", vec![TypedDeclaration::new("a", Type::value("A"))])
            ))
        )
    }
}
