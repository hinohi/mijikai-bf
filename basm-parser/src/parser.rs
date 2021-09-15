use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, one_of},
    combinator::{eof, map, opt, recognize, value},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::ast::{
    Def, Expr, FuncDef, Literal, Stmt, StructDef, Term, Type, TypedDeclaration, Variable,
};

pub fn parse(input: &str) -> IResult<&str, Vec<Def>> {
    let (input, stmts) = many0(def)(input)?;
    let (input, _) = skip(input)?;
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
///     a += 10;
///     while {b += 1; a} {
///         get(b);
///         a -= 1;
///     }
/// }"#).is_ok())
/// ```
pub fn func_def(input: &str) -> IResult<&str, FuncDef> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("fn"), multispace1)(input)?;
    let (input, name) = identifier(input)?;
    let (input, args) = comma_sp_fields('(', ')')(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char('{')(input)?;
    let (input, body) = terminated(many0(stmt), pair(skip, char('}')))(input)?;
    Ok((input, FuncDef::new(name, args, body)))
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
    alt((region, call, map(variable, Expr::Variable)))(input)
}

fn region(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('{')(input)?;
    let (input, _) = skip(input)?;
    let (input, (body, _, ret)) =
        terminated(tuple((many0(stmt), skip, expr)), pair(skip, char('}')))(input)?;
    Ok((
        input,
        Expr::Region {
            body,
            ret: Box::new(ret),
        },
    ))
}

fn call(input: &str) -> IResult<&str, Expr> {
    let (input, name) = identifier(input)?;
    let (input, _) = skip(input)?;
    let trail_comma = value((), tuple((char(','), skip, char(')'))));
    let (input, args) = delimited(
        pair(skip, char('(')),
        separated_list0(pair(skip, char(',')), variable),
        pair(skip, alt((value((), char(')')), trail_comma))),
    )(input)?;
    Ok((
        input,
        Expr::Call {
            name: name.to_string(),
            args,
        },
    ))
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((
        map(number, |n| Literal::Number(n).term()),
        map(variable, Term::Variable),
    ))(input)
}

fn variable(input: &str) -> IResult<&str, Variable> {
    alt((
        value_variable,
        map(preceded(char('*'), value_variable), |t| {
            Variable::Deref(Box::new(t))
        }),
    ))(input)
}

fn value_variable(input: &str) -> IResult<&str, Variable> {
    let (mut input, mut v) = alt((
        map(set, Variable::Set),
        map(identifier, |i| Variable::ident(i.to_owned())),
    ))(input)?;
    while let Ok((i, c)) = one_of::<&str, &str, nom::error::Error<&str>>(".[")(input) {
        input = i;
        if c == '.' {
            let (i, attr) = identifier(i)?;
            v = v.attr(attr);
            input = i;
        } else {
            let (i, (index, _)) = pair(number, char(']'))(input)?;
            v = v.index(index);
            input = i;
        }
    }
    Ok((input, v))
}

fn set(input: &str) -> IResult<&str, Vec<Variable>> {
    delimited(char('('), separated_list1(char('|'), variable), char(')'))(input)
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

pub fn stmt(input: &str) -> IResult<&str, Stmt> {
    alt((
        while_stmt,
        braket,
        assign,
        move_stmt,
        map(tuple((skip, expr, skip, char(';'))), |(_, e, _, _)| {
            Stmt::Expr(e)
        }),
    ))(input)
}

fn assign(input: &str) -> IResult<&str, Stmt> {
    let (input, _) = skip(input)?;
    let (input, target) = variable(input)?;
    let (input, _) = skip(input)?;
    let (input, (t, _)) = pair(one_of("+-"), char('='))(input)?;
    let (input, _) = skip(input)?;
    let (input, value) = term(input)?;
    let (input, _) = skip(input)?;
    let (input, factor) = opt(preceded(pair(char('*'), skip), number))(input)?;
    let (input, _) = pair(skip, char(';'))(input)?;
    let s = if t == '+' {
        Stmt::AssignAdd {
            target: Box::new(target),
            value: Box::new(value),
            factor: factor.unwrap_or(1),
        }
    } else {
        Stmt::AssignSub {
            target: Box::new(target),
            value: Box::new(value),
            factor: factor.unwrap_or(1),
        }
    };
    Ok((input, s))
}

fn while_stmt(input: &str) -> IResult<&str, Stmt> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("while"), multispace1)(input)?;
    let (input, _) = skip(input)?;
    let (input, condition) = expr(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char('{')(input)?;
    let (input, body) = terminated(many0(stmt), pair(skip, char('}')))(input)?;
    Ok((
        input,
        Stmt::While {
            condition: Box::new(condition),
            body,
        },
    ))
}

fn braket(input: &str) -> IResult<&str, Stmt> {
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("bra"), multispace1)(input)?;
    let (input, _) = skip(input)?;
    let (input, bra) = variable(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char('{')(input)?;
    let (input, (body, _, ret)) =
        terminated(tuple((many0(stmt), skip, expr)), pair(skip, char('}')))(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("ket"), multispace1)(input)?;
    let (input, _) = skip(input)?;
    let (input, ket) = variable(input)?;
    let (input, _) = pair(skip, char(';'))(input)?;
    Ok((
        input,
        Stmt::Braket {
            bra: Box::new(bra),
            body,
            ret: Box::new(ret),
            ket: Box::new(ket),
        },
    ))
}

fn move_stmt(input: &str) -> IResult<&str, Stmt> {
    let right = pair(skip, tag("->"));
    let item = pair(
        preceded(skip, variable),
        preceded(right, preceded(skip, variable)),
    );

    let (input, _) = skip(input)?;
    let (input, _) = pair(tag("move"), multispace1)(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char('{')(input)?;
    let (input, v) = many1(terminated(item, pair(skip, char(','))))(input)?;
    let (input, _) = skip(input)?;
    let (input, _) = char('}')(input)?;

    Ok((input, Stmt::Move(v)))
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
    fn test_variable() {
        assert_eq!(variable("a {}"), Ok((" {}", Variable::ident("a"))));
        assert_eq!(
            variable("c.b.a"),
            Ok(("", Variable::ident("c").attr("b").attr("a")))
        );
        assert_eq!(
            variable("*(a|b[1]|*c)[0].b[-1]"),
            Ok((
                "",
                Variable::Set(vec![
                    Variable::ident("a"),
                    Variable::ident("b").index(1),
                    Variable::ident("c").dereference(),
                ])
                .index(0)
                .attr("b")
                .index(-1)
                .dereference()
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

    #[test]
    fn test_expr() {
        assert_eq!(expr("a"), Ok(("", Variable::ident("a").expr())));
        assert_eq!(
            expr("f(a)"),
            Ok((
                "",
                Expr::Call {
                    name: "f".to_owned(),
                    args: vec![Variable::ident("a")],
                }
            ))
        );
        assert_eq!(
            expr("{a+=1;b}"),
            Ok((
                "",
                Expr::Region {
                    body: vec![Stmt::AssignAdd {
                        target: Box::new(Variable::ident("a")),
                        value: Box::new(Literal::Number(1).term()),
                        factor: 1,
                    }],
                    ret: Box::new(Variable::ident("b").expr()),
                }
            ))
        );
    }

    #[test]
    fn test_stmt_simple() {
        assert_eq!(stmt("a;"), Ok(("", Variable::ident("a").expr().stmt())));
        assert_eq!(
            stmt("a += b;"),
            Ok((
                "",
                Stmt::AssignAdd {
                    target: Box::new(Variable::ident("a")),
                    value: Box::new(Variable::ident("b").term()),
                    factor: 1,
                }
            ))
        );
        assert_eq!(
            stmt("a -= b * 3;"),
            Ok((
                "",
                Stmt::AssignSub {
                    target: Box::new(Variable::ident("a")),
                    value: Box::new(Variable::ident("b").term()),
                    factor: 3,
                }
            ))
        );
    }

    #[test]
    fn test_while() {
        assert_eq!(
            stmt("while a{}"),
            Ok((
                "",
                Stmt::While {
                    condition: Box::new(Variable::ident("a").expr()),
                    body: Vec::new(),
                }
            ))
        );
        assert_eq!(
            stmt(
                r#"
            # this is while
            while {
                # read
                get(a);
                # termnater is -1
                a += 1;
                # check input value
                a
            } {
                a -= 32;
                put(a);
            }"#
            ),
            Ok((
                "",
                Stmt::While {
                    condition: Box::new(Expr::Region {
                        body: vec![
                            Expr::call("get", vec![Variable::ident("a")]).stmt(),
                            Stmt::AssignAdd {
                                target: Box::new(Variable::ident("a")),
                                value: Box::new(Literal::Number(1).term()),
                                factor: 1,
                            },
                        ],
                        ret: Box::new(Variable::ident("a").expr())
                    }),
                    body: vec![
                        Stmt::AssignSub {
                            target: Box::new(Variable::ident("a")),
                            value: Box::new(Literal::Number(32).term()),
                            factor: 1,
                        },
                        Expr::call("put", vec![Variable::ident("a")]).stmt(),
                    ],
                }
            ))
        );
    }

    #[test]
    fn test_braket() {
        assert_eq!(
            stmt("bra a {a -= 1; b} ket (a|b);"),
            Ok((
                "",
                Stmt::Braket {
                    bra: Box::new(Variable::ident("a")),
                    body: vec![Stmt::AssignSub {
                        target: Box::new(Variable::ident("a")),
                        value: Box::new(Literal::Number(1).term()),
                        factor: 1,
                    }],
                    ret: Box::new(Variable::ident("b").expr()),
                    ket: Box::new(Variable::Set(vec![
                        Variable::ident("a"),
                        Variable::ident("b")
                    ])),
                }
            ))
        )
    }

    #[test]
    fn test_move() {
        assert_eq!(
            stmt(
                r#"
        move {
            # a から b
            a -> b,
            # c から d
            c -> d,
        }"#
            ),
            Ok((
                "",
                Stmt::Move(vec![
                    (Variable::ident("a"), Variable::ident("b")),
                    (Variable::ident("c"), Variable::ident("d")),
                ])
            ))
        )
    }
}
