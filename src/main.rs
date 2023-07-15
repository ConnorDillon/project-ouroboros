use compiler::compile;
use interpreter::Interpreter;
use parser::{parse, AST};
use vm::{ByteCode, Value, VM};

fn main() {
    let mut ip = Interpreter::new();
    println!("{:?}", ip.interpret("1").unwrap());
}

mod interpreter {
    use super::{compile, parse, Value, VM};

    pub struct Interpreter {
        pub vm: VM,
    }
    impl Interpreter {
        pub fn new() -> Self {
            Interpreter { vm: VM::new() }
        }
        pub fn interpret(&mut self, code: &str) -> Result<Value, String> {
            let ast = parse(code)?;
            let bc = compile(ast);
            self.vm.load(bc);
            Ok(self.vm.exec())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_interpret_consts() {
            let mut ip = Interpreter::new();
            assert_eq!(ip.interpret("1"), Ok(Value::Int(1)));
            assert_eq!(ip.interpret("1.5"), Ok(Value::Float(1.5)));
            assert_eq!(ip.interpret("nil"), Ok(Value::Nil));
            assert_eq!(ip.interpret("true"), Ok(Value::Bool(true)));
            assert_eq!(ip.interpret("false"), Ok(Value::Bool(false)));
            assert_eq!(ip.interpret("\"foo\""), Ok(Value::string("foo")));
        }
    }
}

mod vm {
    use std::rc::Rc;

    pub struct ByteCode {
        pub constants: Vec<Value>,
        pub bytecode: Vec<Op>,
    }

    impl ByteCode {
        pub fn new() -> ByteCode {
            ByteCode {
                constants: Vec::new(),
                bytecode: Vec::new(),
            }
        }
        pub fn add_const(&mut self, val: Value) {
            self.bytecode.push(Op::Const(self.constants.len()));
            self.constants.push(val)
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Value {
        Nil,
        Bool(bool),
        Int(i64),
        Float(f64),
        String(Rc<str>),
    }

    impl Value {
        pub fn string(s: &str) -> Value {
            Value::String(Rc::from(s))
        }
    }

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Op {
        Const(usize),
    }

    pub struct VM {
        pub ip: usize,
        pub sp: usize,
        pub bytecode: ByteCode,
        pub stack: Vec<Op>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                ip: 0,
                sp: 0,
                bytecode: ByteCode::new(),
                stack: Vec::new(),
            }
        }

        pub fn load(&mut self, bytecode: ByteCode) {
            self.ip = 0;
            self.sp = 0;
            self.bytecode = bytecode;
            self.stack = Vec::new();
        }

        pub fn exec(&mut self) -> Value {
            let op = self.bytecode.bytecode[self.ip];
            self.ip += 1;
            match op {
                Op::Const(i) => self.bytecode.constants[i].clone(),
            }
        }
    }
}

mod compiler {
    use super::{ByteCode, Value, AST};

    pub fn compile(ast: AST) -> ByteCode {
        let mut bc = ByteCode::new();
        match ast {
            AST::Int(x) => bc.add_const(Value::Int(x)),
            AST::Float(x) => bc.add_const(Value::Float(x)),
            AST::String(x) => bc.add_const(Value::string(&*x)),
            AST::Symbol(x) => match &*x {
                "nil" => bc.add_const(Value::Nil),
                "true" => bc.add_const(Value::Bool(true)),
                "false" => bc.add_const(Value::Bool(false)),
                _ => {}
            },
            _ => {}
        }
        bc
    }
}

mod parser {
    use nom::{
        branch::alt,
        character::complete::{char, i64, multispace0, one_of, satisfy},
        combinator::not,
        multi::{many0, many1},
        number::complete::double,
        sequence::{delimited, preceded, terminated},
        Finish, IResult,
    };

    #[derive(Debug, PartialEq)]
    pub enum AST {
        Int(i64),
        Float(f64),
        String(String),
        Symbol(String),
        Expr(Vec<AST>),
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
            Ok((r, AST::String(o.into_iter().collect())))
        }
    }

    fn parse_symbol() -> impl Fn(&str) -> IResult<&str, AST> {
        |i| {
            let (r, o) = alt((
                many1(one_of("<>!@#$%^&*-+/=?|\\;:~")),
                many1(satisfy(|x| x.is_ascii_alphabetic())),
            ))(i)?;
            Ok((r, AST::Symbol(o.into_iter().collect())))
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
        use super::{parse, AST};

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

            let expr2 = vec![
                AST::Symbol(String::from("fn")),
                AST::Expr(vec![AST::Symbol(String::from("x"))]),
                AST::Symbol(String::from("x")),
            ];
            assert_eq!(parse("(fn (x) x)"), Ok(AST::Expr(expr2)));
        }
    }
}
