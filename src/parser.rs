use crate::ast;
use crate::ast::Rec;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, i64, multispace0, one_of, satisfy},
    combinator::not,
    error::ParseError,
    multi::{many0, many1},
    number::complete::double,
    sequence::tuple,
    sequence::{delimited, preceded, terminated},
    Finish, IResult, Parser,
};

type AST = ast::AST<ast::Fun>;

fn lexeme<'a, O, E: ParseError<&'a str>, F>(
    parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    preceded(multispace0, parser)
}

fn parse_int() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, o) = lexeme(terminated(i64, not(char('.'))))(i)?;
        Ok((r, AST::Int(o)))
    }
}

fn parse_float() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, o) = lexeme(double)(i)?;
        Ok((r, AST::Float(o)))
    }
}

fn parse_string() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, o) = delimited(
            lexeme(char('"')),
            lexeme(many0(alt((
                preceded(char('\\'), char('"')),
                satisfy(|x| x != '"'),
            )))),
            lexeme(char('"')),
        )(i)?;
        Ok((r, AST::String(o.into_iter().collect())))
    }
}

fn parse_symbol_string() -> impl Fn(&str) -> IResult<&str, String> {
    |i| {
        let (r, o) = lexeme(alt((
            many1(one_of("<>!@#$%^&*-+/=?|\\;:~")),
            many1(satisfy(|x| x.is_ascii_alphabetic())),
        )))(i)?;
        Ok((r, o.into_iter().collect()))
    }
}

fn parse_symbol() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, o) = parse_symbol_string()(i)?;
        Ok((r, AST::Symbol(o)))
    }
}

fn parse_expr() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, o) = delimited(lexeme(char('(')), many0(parse_ast()), lexeme(char(')')))(i)?;
        Ok((r, AST::Expr(o)))
    }
}

fn parse_let() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, (_, l, x, y, _)) = tuple((
            lexeme(char('(')),
            alt((lexeme(tag("letrec")), lexeme(tag("let")))),
            delimited(
                lexeme(char('(')),
                many1(delimited(
                    lexeme(char('(')),
                    tuple((parse_symbol_string(), parse_ast())),
                    lexeme(char(')')),
                )),
                lexeme(char(')')),
            ),
            parse_ast(),
            lexeme(char(')')),
        ))(i)?;
        Ok((
            r,
            AST::Let(
                if l == "letrec" { Rec::Rec } else { Rec::NonRec },
                x,
                Box::new(y),
            ),
        ))
    }
}

fn parse_fn() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        let (r, (_, _, args, body, _)) = tuple((
            lexeme(char('(')),
            lexeme(tag("fn")),
            delimited(
                lexeme(char('(')),
                many1(parse_symbol_string()),
                lexeme(char(')')),
            ),
            parse_ast(),
            lexeme(char(')')),
        ))(i)?;
        Ok((
            r,
            AST::Fn(ast::Fun {
                args,
                body: Box::new(body),
            }),
        ))
    }
}

fn parse_ast() -> impl Fn(&str) -> IResult<&str, AST> {
    |i| {
        alt((
            parse_int(),
            parse_float(),
            parse_string(),
            parse_symbol(),
            parse_let(),
            parse_fn(),
            parse_expr(),
        ))(i)
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
    use super::{ast, parse, Rec, AST};

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
        assert_eq!(parse("\"\""), Ok(AST::String(String::from(""))));
        assert_eq!(parse("\"\\\"\""), Ok(AST::String(String::from("\""))));
        // TO-DO: implement special characters in strings
        // assert_eq!(
        //     parse("\"\\n\""),
        //     Ok(("", AST::String(String::from("\n"))))
        // );
        assert_eq!(parse("\"nil\""), Ok(AST::String(String::from("nil"))));
        assert_eq!(
            parse("\"foo bar !\""),
            Ok(AST::String(String::from("foo bar !")))
        );
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(parse("foo"), Ok(AST::Symbol(String::from("foo"))));
        assert_eq!(parse("++"), Ok(AST::Symbol(String::from("++"))));
    }

    #[test]
    fn test_parse_expr() {
        let expr1 = vec![AST::Symbol(String::from("+")), AST::Int(1), AST::Int(2)];
        assert_eq!(parse("(+ 1 2)"), Ok(AST::Expr(expr1)));
    }

    #[test]
    fn test_parse_fn() {
        assert_eq!(
            parse("(fn (x) x)"),
            Ok(AST::Fn(ast::Fun {
                args: vec![String::from("x")],
                body: Box::new(AST::Symbol(String::from("x"))),
            }))
        );
    }

    #[test]
    fn test_parse_let() {
        assert_eq!(
            parse("(let ((x 1) (y 2)) x)"),
            Ok(AST::Let(
                Rec::NonRec,
                vec![
                    (String::from("x"), AST::Int(1)),
                    (String::from("y"), AST::Int(2))
                ],
                Box::new(AST::Symbol("x".into()))
            ))
        );
    }

    #[test]
    fn test_parse_letrec() {
        assert_eq!(
            parse("(letrec ((x 1)) x)"),
            Ok(AST::Let(
                Rec::Rec,
                vec![(String::from("x"), AST::Int(1))],
                Box::new(AST::Symbol("x".into()))
            ))
        );
    }
}
