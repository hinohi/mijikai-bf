use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1},
    combinator::{eof, map, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded},
    IResult,
};

use crate::ast::{Stmt, StructDef, Type, TypedDeclaration};

pub fn parse(input: &str) -> IResult<&str, Vec<Stmt>> {
    let (input, stmts) = many0(stmt)(input)?;
    let (input, _) = eof(input)?;
    Ok((input, stmts))
}

pub fn comment(input: &str) -> IResult<&str, &str> {
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

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn stmt(input: &str) -> IResult<&str, Stmt> {
    map(struct_def, Stmt::StructDef)(input)
}

fn struct_def(input: &str) -> IResult<&str, StructDef> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("struct"), multispace1)(input)?;
    let (input, name) = identifier(input)?;
    let trail_comma = value((), pair(char(','), pair(multispace0, char('}'))));
    let (input, fields) = delimited(
        pair(multispace0, char('{')),
        separated_list0(pair(multispace0, char(',')), field),
        pair(multispace0, alt((value((), char('}')), trail_comma))),
    )(input)?;
    Ok((
        input,
        StructDef {
            name: name.to_string(),
            fields,
        },
    ))
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
