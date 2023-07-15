use parser::parse;

fn main() {
    println!("{:?}", parse("(1 + 2 == \"foo\" * bar)").unwrap());
}

mod parser {
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{char, i64, multispace0, one_of, satisfy},
        combinator::not,
        multi::{many0, many1},
        number::complete::double,
        sequence::{delimited, preceded, terminated},
        Finish, IResult,
    };
    use std::rc::Rc;

    #[derive(Debug, PartialEq)]
    pub enum AST {
        Nil,
        Bool(bool),
        Int(i64),
        Float(f64),
        String(Rc<String>),
        Symbol(Rc<String>),
        Expr(Vec<AST>),
    }
    fn parse_nil() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, _) = tag("nil")(i)?;
            Ok((r, AST::Nil))
        }
    }

    fn parse_bool() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = alt((tag("true"), tag("false")))(i)?;
            Ok((r, AST::Bool(o == "true")))
        }
    }

    fn parse_int() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = terminated(i64, not(char('.')))(i)?;
            Ok((r, AST::Int(o)))
        }
    }

    fn parse_float() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = double(i)?;
            Ok((r, AST::Float(o)))
        }
    }

    fn parse_string() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = delimited(
                char('"'),
                many0(alt((
                    preceded(char('\\'), char('"')),
                    satisfy(|x| x != '"'),
                ))),
                char('"'),
            )(i)?;
            Ok((r, AST::String(Rc::new(o.into_iter().collect()))))
        }
    }

    fn parse_symbol() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = alt((
                many1(one_of("<>!@#$%^&*-+/=?|\\;:~")),
                many1(satisfy(|x| x.is_ascii_alphabetic())),
            ))(i)?;
            Ok((r, AST::Symbol(Rc::new(o.into_iter().collect()))))
        }
    }

    fn parse_expr() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = delimited(
                char('('),
                many0(parse_ast()),
                preceded(multispace0, char(')')),
            )(i)?;
            Ok((r, AST::Expr(o)))
        }
    }

    fn parse_ast() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            preceded(
                multispace0,
                alt((
                    parse_nil(),
                    parse_bool(),
                    parse_int(),
                    parse_float(),
                    parse_string(),
                    parse_symbol(),
                    parse_expr(),
                )),
            )(i)
        }
    }

    pub fn parse(i: &str) -> Result<AST, String> {
        match parse_ast()(i).finish() {
            Ok((_, x)) => Ok(x),
            Err(e) => Err(format!("{:?}", e)),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_parse_nil() {
            assert_eq!(parse("nil"), Ok(AST::Nil));
        }

        #[test]
        fn test_parse_bool() {
            assert_eq!(parse("true"), Ok(AST::Bool(true)));
            assert_eq!(parse("false"), Ok(AST::Bool(false)));
        }

        #[test]
        fn test_parse_int() {
            assert_eq!(parse("0"), Ok(AST::Int(0)));
            assert_eq!(parse("10"), Ok(AST::Int(10)));
            assert_eq!(parse("-1"), Ok(AST::Int(-1)));
        }

        #[test]
        fn test_parse_float() {
            assert_eq!(parse("0.0"), Ok(AST::Float(0.0)));
            assert_eq!(parse("10.5"), Ok(AST::Float(10.5)));
            assert_eq!(parse("-1.1"), Ok(AST::Float(-1.1)));
        }

        #[test]
        fn test_parse_string() {
            assert_eq!(parse("\"\""), Ok(AST::String(Rc::new(String::from("")))));
            assert_eq!(
                parse("\"\\\"\""),
                Ok(AST::String(Rc::new(String::from("\""))))
            );
            // TO-DO: implement special characters in strings
            // assert_eq!(
            //     parse("\"\\n\""),
            //     Ok(("", AST::String(Rc::new(String::from("\n")))))
            // );
            assert_eq!(
                parse("\"nil\""),
                Ok(AST::String(Rc::new(String::from("nil"))))
            );
            assert_eq!(
                parse("\"foo bar !\""),
                Ok(AST::String(Rc::new(String::from("foo bar !"))))
            );
        }

        #[test]
        fn test_parse_symbol() {
            assert_eq!(parse("foo"), Ok(AST::Symbol(Rc::new(String::from("foo")))));
            assert_eq!(parse("++"), Ok(AST::Symbol(Rc::new(String::from("++")))));
        }

        #[test]
        fn test_parse_expr() {
            let expr1 = vec![
                AST::Symbol(Rc::new(String::from("+"))),
                AST::Int(1),
                AST::Int(2),
            ];
            assert_eq!(parse("(+ 1 2)"), Ok(AST::Expr(expr1)));

            let expr2 = vec![
                AST::Symbol(Rc::new(String::from("fn"))),
                AST::Expr(vec![AST::Symbol(Rc::new(String::from("x")))]),
                AST::Symbol(Rc::new(String::from("x"))),
            ];
            assert_eq!(parse("(fn (x) x)"), Ok(AST::Expr(expr2)));
        }
    }
}
