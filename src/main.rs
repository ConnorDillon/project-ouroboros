use crate::parser::parse;
use compiler::Compiler;
use interpreter::Interpreter;

fn main() {
    let mut ip = Interpreter::new();
    println!("{:?}", ip.interpret("1").unwrap());
    let ast = parse("((fn (x) (fn (y) (+ 1 y))) 1 2)").unwrap();
    let code = Compiler::new().compile(ast);
    println!("{:?}", code)
}

mod interpreter {
    use crate::compiler::Compiler;
    use crate::parser::parse;
    use crate::vm::{Value, VM};

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

        #[test]
        fn test_interpret_fn() {
            let mut ip = Interpreter::new();
            assert_eq!(ip.interpret("((fn (x) (+ x 1)) 2)"), Ok(Value::Int(3)));
            assert_eq!(ip.interpret("((fn (x y) (+ x y)) 1 2)"), Ok(Value::Int(3)));
            assert_eq!(
                ip.interpret("(((fn (x y) ((+ x) y)) 1) 2)"),
                Ok(Value::Int(3))
            );
            assert_eq!(
                ip.interpret("((fn (f x) (f x)) (fn (x) (+ 1 x)) 2)"),
                Ok(Value::Int(3))
            );
            assert_eq!(
                ip.interpret("((fn (f x y) (f x y)) + 1 2)"),
                Ok(Value::Int(3))
            );
        }
    }
}

mod builtin {
    use crate::vm::Value;

    pub fn add(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
            (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
            (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 + y),
            (Value::Float(x), Value::Int(y)) => Value::Float(x + y as f64),
            (x, y) => panic!("Type Error: (+ {:?} {:?})", x, y),
        }
    }

    pub fn subtract(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
            (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
            (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 - y),
            (Value::Float(x), Value::Int(y)) => Value::Float(x - y as f64),
            (x, y) => panic!("Type Error: (+ {:?} {:?})", x, y),
        }
    }

    pub fn multiply(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
            (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
            (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
            (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
            (x, y) => panic!("Type Error: (+ {:?} {:?})", x, y),
        }
    }

    pub fn divide(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
            (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
            (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 / y),
            (Value::Float(x), Value::Int(y)) => Value::Float(x / y as f64),
            (x, y) => panic!("Type Error: (+ {:?} {:?})", x, y),
        }
    }
}

mod vm {
    use std::rc::Rc;

    use crate::builtin;

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Op {
        Const(usize),
        BeginFrame,
        EndFrame,
        GetVar(usize, usize),
        Return,
        Apply(u8),
        Function(u8, Function),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct FunctionDef {
        pub entry: usize,
        pub args: u8,
    }

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum Function {
        Add,
        Subtract,
        Multiply,
        Divide,
        Defined(usize),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct ByteCode {
        pub consts: Vec<Value>,
        pub funs: Vec<FunctionDef>,
        pub code: Vec<Op>,
        pub entry: usize,
    }

    impl ByteCode {
        pub fn new() -> ByteCode {
            ByteCode {
                consts: Vec::new(),
                funs: Vec::new(),
                code: Vec::new(),
                entry: 0,
            }
        }

        pub fn add_op(&mut self, op: Op) {
            self.code.push(op);
        }

        pub fn add_const(&mut self, val: Value) {
            self.code.push(Op::Const(self.consts.len()));
            self.consts.push(val)
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Closure {
        closed_vals: Rc<Vec<Value>>,
        fun_args: u8,
        fun: Function,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Value {
        Nil,
        Bool(bool),
        Int(i64),
        Float(f64),
        String(Rc<str>),
        Closure(Closure),
    }

    impl Value {
        pub fn closure(vals: Vec<Value>, fun_args: u8, fun: Function) -> Value {
            Value::Closure(Closure {
                closed_vals: Rc::new(vals),
                fun_args,
                fun,
            })
        }

        pub fn string(s: &str) -> Value {
            Value::String(Rc::from(s))
        }

        pub fn to_closure(&self) -> Closure {
            if let Value::Closure(c) = self {
                c.clone()
            } else {
                panic!("Expected closure but found: {:?}", self)
            }
        }
    }

    pub struct VM {
        ip: usize,
        code: ByteCode,
        stack: Vec<Value>,
        call_frames: Vec<CallFrame>,
    }

    struct CallFrame {
        stack_ptr: usize,
        return_ptr: usize,
        closed_args: u8,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                ip: 0,
                code: ByteCode::new(),
                stack: Vec::new(),
                call_frames: Vec::new(),
            }
        }

        pub fn load(&mut self, code: ByteCode) {
            self.ip = code.entry;
            self.code = code;
            self.stack = Vec::new();
        }

        fn pop_val(&mut self, closure: &Closure, slot: usize) -> Value {
            closure
                .closed_vals
                .get(slot)
                .cloned()
                .or(self.stack.pop())
                .unwrap()
        }

        fn get_var(&mut self, depth: usize, slot: usize) {
            let frame = &self.call_frames[self.call_frames.len() - 1 - depth];
            let var = if frame.closed_args == 0 {
                self.stack[frame.stack_ptr + slot].clone()
            } else if slot < (frame.closed_args as usize) {
                let closure = self.stack[frame.stack_ptr - 1].to_closure();
                closure.closed_vals[slot].clone()
            } else {
                let abs_slot = frame.stack_ptr + (slot - frame.closed_args as usize);
                self.stack[abs_slot].clone()
            };
            self.stack.push(var);
        }

        fn apply2(&mut self, closure: &Closure, f: fn(Value, Value) -> Value) {
            let y = self.pop_val(closure, 1);
            let x = self.pop_val(closure, 0);
            self.stack.pop();
            self.stack.push(f(x, y));
        }

        fn apply(&mut self, ap_args: u8) {
            let stack_ptr = self.stack.len() - (ap_args as usize);
            let cl = self.stack[stack_ptr - 1].to_closure();
            let cur_args = cl.closed_vals.len() as u8 + ap_args;
            if cur_args < cl.fun_args {
                let mut vals = Vec::with_capacity(cur_args as usize);
                vals.extend(
                    cl.closed_vals
                        .iter()
                        .cloned()
                        .chain(self.stack.drain(stack_ptr..)),
                );
                self.stack.pop();
                self.stack.push(Value::closure(vals, cl.fun_args, cl.fun));
            } else if cur_args > cl.fun_args {
                // multiple applies
            } else {
                match cl.fun {
                    Function::Add => self.apply2(&cl, builtin::add),
                    Function::Subtract => self.apply2(&cl, builtin::subtract),
                    Function::Multiply => self.apply2(&cl, builtin::multiply),
                    Function::Divide => self.apply2(&cl, builtin::divide),
                    Function::Defined(ip) => {
                        self.call_frames.push(CallFrame {
                            stack_ptr,
                            return_ptr: self.ip,
                            closed_args: cl.closed_vals.len() as u8,
                        });
                        self.ip = ip;
                    }
                }
            }
        }

        pub fn exec(&mut self) -> Value {
            while self.ip < self.code.code.len() {
                let op = self.code.code[self.ip];
                self.ip += 1;
                match op {
                    Op::Const(i) => self.const_op(i),
                    Op::Function(a, f) => self.stack.push(Value::closure(Vec::new(), a, f)),
                    Op::Apply(ap_args) => self.apply(ap_args),
                    Op::Return => {
                        let frame = self.call_frames.pop().unwrap();
                        let result = self.stack.pop().unwrap();
                        self.stack.truncate(frame.stack_ptr - 1);
                        self.stack.push(result);
                        self.ip = frame.return_ptr;
                    }
                    Op::GetVar(depth, slot) => self.get_var(depth, slot),
                    Op::BeginFrame => self.call_frames.push(CallFrame {
                        stack_ptr: self.stack.len(),
                        return_ptr: 0,
                        closed_args: 0,
                    }),
                    Op::EndFrame => {
                        let result = self.stack.pop().unwrap();
                        self.stack
                            .truncate(self.call_frames.pop().unwrap().stack_ptr);
                        self.stack.push(result);
                    }
                }
            }
            self.stack.pop().unwrap()
        }

        fn const_op(&mut self, idx: usize) {
            self.stack.push(self.code.consts[idx].clone())
        }
    }
}

mod compiler {
    use crate::ast::{Fun, AST};
    use crate::vm::{ByteCode, Function, FunctionDef, Op, Value};

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

        fn compile_part(&mut self, ast: AST<usize>) {
            match ast {
                AST::Int(x) => self.code.add_const(Value::Int(x)),
                AST::Float(x) => self.code.add_const(Value::Float(x)),
                AST::String(x) => self.code.add_const(Value::string(&*x)),
                AST::Symbol(x) => match &*x {
                    "nil" => self.code.add_const(Value::Nil),
                    "true" => self.code.add_const(Value::Bool(true)),
                    "false" => self.code.add_const(Value::Bool(false)),
                    "+" => self.code.add_op(Op::Function(2, Function::Add)),
                    "-" => self.code.add_op(Op::Function(2, Function::Subtract)),
                    "*" => self.code.add_op(Op::Function(2, Function::Multiply)),
                    "/" => self.code.add_op(Op::Function(2, Function::Divide)),
                    _ => {
                        let mut found = false;
                        for var in self.vars.iter().rev() {
                            if var.name == x {
                                self.code
                                    .add_op(Op::GetVar(self.depth - var.depth, var.slot));
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
                    let args = x.len() - 1;
                    for a in x.into_iter() {
                        self.compile_part(a);
                    }
                    self.code.add_op(Op::Apply(args as u8));
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
                AST::Fn(i) => {
                    let f = &self.code.funs[i];
                    self.code
                        .add_op(Op::Function(f.args, Function::Defined(f.entry)))
                }
            }
        }

        pub fn compile(mut self, ast: AST<Fun>) -> ByteCode {
            let lifted_ast = ast.lift_lambdas();
            for fun in lifted_ast.funs {
                let entry = self.code.code.len();
                self.code.funs.push(FunctionDef {
                    args: fun.args.len() as u8,
                    entry,
                });
                for (slot, name) in fun.args.into_iter().enumerate() {
                    self.vars.push(Var {
                        name,
                        slot,
                        depth: 0,
                    });
                }
                self.compile_part(fun.body);
                self.code.add_op(Op::Return);
                self.vars.truncate(0);
            }
            let entry = self.code.code.len();
            self.compile_part(lifted_ast.ast);
            self.code.entry = entry;
            self.code
        }
    }
}

mod ast {
    #[derive(Debug, PartialEq, Clone)]
    pub enum AST<T> {
        Int(i64),
        Float(f64),
        String(String),
        Symbol(String),
        Expr(Vec<AST<T>>),
        Let(Vec<(String, AST<T>)>, Box<AST<T>>),
        Fn(T),
    }

    impl AST<Fun> {
        pub fn lift_lambdas(self) -> LiftedAST {
            let mut funs = Vec::new();
            let ast = lift(&mut funs, self);
            LiftedAST { funs, ast }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Fun {
        pub args: Vec<String>,
        pub body: Box<AST<Fun>>,
    }

    pub struct LiftedAST {
        pub funs: Vec<LiftedFun>,
        pub ast: AST<usize>,
    }

    pub struct LiftedFun {
        pub args: Vec<String>,
        pub body: AST<usize>,
    }

    fn lift(funs: &mut Vec<LiftedFun>, ast: AST<Fun>) -> AST<usize> {
        match ast {
            AST::Fn(f) => {
                let body = lift(funs, *f.body);
                let lf = LiftedFun { args: f.args, body };
                let idx = funs.len();
                funs.push(lf);
                AST::Fn(idx)
            }
            AST::Expr(x) => AST::Expr(x.into_iter().map(|x| lift(funs, x)).collect()),
            AST::Let(vars, expr) => AST::Let(
                vars.into_iter().map(|(x, y)| (x, lift(funs, y))).collect(),
                Box::new(lift(funs, *expr)),
            ),
            AST::Int(x) => AST::Int(x),
            AST::Float(x) => AST::Float(x),
            AST::String(x) => AST::String(x),
            AST::Symbol(x) => AST::Symbol(x),
        }
    }
}

mod parser {
    use crate::ast;
    type AST = ast::AST<ast::Fun>;
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
        use super::{ast, parse, AST};

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
