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
        vm: VM,
    }
    impl Interpreter {
        pub fn new() -> Self {
            Interpreter { vm: VM::new() }
        }

        pub fn interpret(&mut self, code: &str) -> Result<Value, String> {
            let ast = parse(code)?;
            let code = Compiler::new().compile(ast);
            self.vm.load(code);
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

        #[test]
        fn test_interpret_let_expr() {
            let mut ip = Interpreter::new();
            assert_eq!(
                ip.interpret("(let ((x 1) (y 2)) (- y x))"),
                Ok(Value::Int(1))
            );
            assert_eq!(
                ip.interpret("(+ 3 (let ((x 1) (y 2)) (- y x)))"),
                Ok(Value::Int(4))
            );
            assert_eq!(
                ip.interpret("(+ (let ((x 1) (y 2)) (- y x)) 3)"),
                Ok(Value::Int(4))
            );
            assert_eq!(
                ip.interpret("(let ((x 1) (y 2)) (let ((x 3) (z 4)) (+ x (+ y z))))"),
                Ok(Value::Int(9))
            );
            assert_eq!(
                ip.interpret("(let ((x 1) (y 2)) (+ (let ((x 3) (z 4)) (+ x (+ y z))) 5))"),
                Ok(Value::Int(14))
            );
        }
    }
}

mod vm {
    use std::rc::Rc;

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Op {
        Const(usize),
        BeginFrame,
        EndFrame,
        GetVar(usize, usize),
        Add,
        Subtract,
        Multiply,
        Divide,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Function {
        name: String,
        args: u8,
        code: ByteCode,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct ByteCode {
        constants: Vec<Value>,
        bytecode: Vec<Op>,
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
        ip: usize,
        bytecode: ByteCode,
        stack: Vec<Value>,
        stack_frames: Vec<usize>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                ip: 0,
                bytecode: ByteCode::new(),
                stack: Vec::new(),
                stack_frames: Vec::new(),
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
                    Op::GetVar(depth, slot) => {
                        let abs_slot = self.stack_frames[depth] + slot;
                        self.stack.push(self.stack[abs_slot].clone());
                    }
                    Op::BeginFrame => self.stack_frames.push(self.stack.len()),
                    Op::EndFrame => {
                        let result = self.stack.pop().unwrap();
                        self.stack.truncate(self.stack_frames.pop().unwrap());
                        self.stack.push(result);
                    }
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

    #[derive(Debug, PartialEq, Clone)]
    struct Var {
        name: String,
        depth: usize,
        slot: usize,
    }

    pub struct Compiler {
        code: ByteCode,
        vars: Vec<Var>,
        depth: usize,
    }

    impl Compiler {
        pub fn new() -> Self {
            Compiler {
                code: ByteCode::new(),
                vars: Vec::new(),
                depth: 0,
            }
        }

        fn compile_part(&mut self, ast: AST) {
            match ast {
                AST::Int(x) => self.code.add_const(Value::Int(x)),
                AST::Float(x) => self.code.add_const(Value::Float(x)),
                AST::String(x) => self.code.add_const(Value::string(&*x)),
                AST::Symbol(x) => match &*x {
                    "nil" => self.code.add_const(Value::Nil),
                    "true" => self.code.add_const(Value::Bool(true)),
                    "false" => self.code.add_const(Value::Bool(false)),
                    "+" => self.code.add_op(Op::Add),
                    "-" => self.code.add_op(Op::Subtract),
                    "*" => self.code.add_op(Op::Multiply),
                    "/" => self.code.add_op(Op::Divide),
                    _ => {
                        let mut found = false;
                        for var in self.vars.iter().rev() {
                            if var.name == x {
                                self.code.add_op(Op::GetVar(var.depth - 1, var.slot));
                                found = true;
                                break;
                            };
                        }
                        if !found {
                            panic!("Symbol {} not found in {:?} {}", x, self.vars, self.depth)
                        }
                    }
                },
                AST::Expr(x) => {
                    for a in x.into_iter().rev() {
                        self.compile_part(a);
                    }
                }
                AST::Let(vars, expr) => {
                    self.depth = self.depth + 1;
                    self.code.add_op(Op::BeginFrame);
                    let var_count = vars.len();
                    for (slot, (name, vexpr)) in vars.into_iter().enumerate() {
                        self.vars.push(Var {
                            name,
                            slot,
                            depth: self.depth,
                        });
                        self.compile_part(vexpr)
                    }
                    self.compile_part(*expr);
                    self.vars.truncate(var_count);
                    self.code.add_op(Op::EndFrame);
                    self.depth = self.depth - 1;
                }
            }
        }

        pub fn compile(mut self, ast: AST) -> ByteCode {
            self.compile_part(ast);
            self.code
        }
    }
}

mod parser {
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

    #[derive(Debug, PartialEq, Clone)]
    pub enum AST {
        Int(i64),
        Float(f64),
        String(String),
        Symbol(String),
        Expr(Vec<AST>),
        Let(Vec<(String, AST)>, Box<AST>),
    }

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
            let (r, (_, _, x, y, _)) = tuple((
                lexeme(char('(')),
                lexeme(tag("let")),
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
            Ok((r, AST::Let(x, Box::new(y))))
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

        #[test]
        fn test_parse_let() {
            assert_eq!(
                parse("(let ((x 1) (y 2)) x)"),
                Ok(AST::Let(
                    vec![
                        (String::from("x"), AST::Int(1)),
                        (String::from("y"), AST::Int(2))
                    ],
                    Box::new(AST::Symbol("x".into()))
                ))
            );
        }
    }
}
