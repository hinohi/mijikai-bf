use nom::{
    branch::alt,
    character::complete::{char, one_of},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, pair, preceded},
    IResult,
};

use super::simple::{identifier, number};
use crate::ast::Variable;

pub(crate) fn variable(input: &str) -> IResult<&str, Variable> {
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
