use compiler::Compiler;
use interpreter::Interpreter;
use parser::{parse, AST};
use vm::{ByteCode, Op, Value, VM};

fn main() {
    let mut ip = Interpreter::new();
    println!("{:?}", ip.interpret("1").unwrap());
}

mod interpreter {
    use super::{parse, Compiler, Value, VM};

    pub struct Interpreter {
        pub vm: VM,
    }
    impl Interpreter {
        pub fn new() -> Self {
            Interpreter { vm: VM::new() }
        }

        pub fn interpret(&mut self, code: &str) -> Result<Value, String> {
            let ast = parse(code)?;
            let bc = Compiler::new().compile(ast);
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

        #[test]
        fn test_interpret_expr() {
            let mut ip = Interpreter::new();
            assert_eq!(ip.interpret("(+ 1 1)"), Ok(Value::Int(2)));
            assert_eq!(ip.interpret("(+ 3 (- 2 1))"), Ok(Value::Int(4)));
            assert_eq!(ip.interpret("(* 2 (/ 3 2.0))"), Ok(Value::Float(3.0)));
        }
    }
}

mod vm {
    use std::rc::Rc;

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Op {
        Const(usize),
        Add,
        Subtract,
        Multiply,
        Divide,
    }

    #[derive(Debug, PartialEq, Clone)]
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

        pub fn add_op(&mut self, op: Op) {
            self.bytecode.push(op);
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

    pub struct VM {
        pub ip: usize,
        pub bytecode: ByteCode,
        pub stack: Vec<Value>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                ip: 0,
                bytecode: ByteCode::new(),
                stack: Vec::new(),
            }
        }

        pub fn load(&mut self, bytecode: ByteCode) {
            self.ip = 0;
            self.bytecode = bytecode;
            self.stack = Vec::new();
        }

        pub fn exec(&mut self) -> Value {
            while self.ip < self.bytecode.bytecode.len() {
                let op = self.bytecode.bytecode[self.ip];
                self.ip += 1;
                match op {
                    Op::Const(i) => self.const_op(i),
                    Op::Add => self.add(),
                    Op::Subtract => self.substract(),
                    Op::Multiply => self.multiply(),
                    Op::Divide => self.divide(),
                }
            }
            self.stack.pop().unwrap()
        }

        fn const_op(&mut self, idx: usize) {
            self.stack.push(self.bytecode.constants[idx].clone())
        }

        fn add(&mut self) {
            let x = self.stack.pop().unwrap();
            let y = self.stack.pop().unwrap();
            let z = match (x, y) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 + y),
                (Value::Float(x), Value::Int(y)) => Value::Float(x + y as f64),
                (x, y) => panic!("Type Error: (+ {:?} {:?})", x, y),
            };
            self.stack.push(z);
        }

        fn substract(&mut self) {
            let x = self.stack.pop().unwrap();
            let y = self.stack.pop().unwrap();
            let z = match (x, y) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
                (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
                (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 - y),
                (Value::Float(x), Value::Int(y)) => Value::Float(x - y as f64),
                (x, y) => panic!("Type Error: (- {:?} {:?})", x, y),
            };
            self.stack.push(z);
        }

        fn multiply(&mut self) {
            let x = self.stack.pop().unwrap();
            let y = self.stack.pop().unwrap();
            let z = match (x, y) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
                (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
                (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
                (x, y) => panic!("Type Error: (* {:?} {:?})", x, y),
            };
            self.stack.push(z);
        }

        fn divide(&mut self) {
            let x = self.stack.pop().unwrap();
            let y = self.stack.pop().unwrap();
            let z = match (x, y) {
                (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
                (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
                (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 / y),
                (Value::Float(x), Value::Int(y)) => Value::Float(x / y as f64),
                (x, y) => panic!("Type Error: (/ {:?} {:?})", x, y),
            };
            self.stack.push(z);
        }
    }
}

mod compiler {
    use super::{ByteCode, Op, Value, AST};

    pub struct Compiler {
        bc: ByteCode,
    }

    impl Compiler {
        pub fn new() -> Self {
            Compiler {
                bc: ByteCode::new(),
            }
        }

        fn compile_part(&mut self, ast: AST) {
            match ast {
                AST::Int(x) => self.bc.add_const(Value::Int(x)),
                AST::Float(x) => self.bc.add_const(Value::Float(x)),
                AST::String(x) => self.bc.add_const(Value::string(&*x)),
                AST::Symbol(x) => match &*x {
                    "nil" => self.bc.add_const(Value::Nil),
                    "true" => self.bc.add_const(Value::Bool(true)),
                    "false" => self.bc.add_const(Value::Bool(false)),
                    "+" => self.bc.add_op(Op::Add),
                    "-" => self.bc.add_op(Op::Subtract),
                    "*" => self.bc.add_op(Op::Multiply),
                    "/" => self.bc.add_op(Op::Divide),
                    _ => panic!("Unrecognized symbol: {}", x),
                },
                AST::Expr(x) => {
                    for a in x.into_iter().rev() {
                        self.compile_part(a);
                    }
                }
            }
        }

        pub fn compile(mut self, ast: AST) -> ByteCode {
            self.compile_part(ast);
            self.bc
        }
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
