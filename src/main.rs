mod ast;
mod builtin;
mod bytecode;
mod compiler;
mod interpreter;
mod parser;
mod vm;
mod stack;

use compiler::Compiler;
use interpreter::Interpreter;
use parser::parse;
use vm::Value;

fn main() {
    let mut ip = Interpreter::new();
    ip.interpret("1").unwrap();
    let expr = "
      (letrec ((f (fn (x) (if (cond x) (g (succ x)) x)))
               (g (fn (x) (if (cond x) (f (succ x)) x)))
               (foo (fn (x) (+ x 1)))
               (succ (fn (x) (let ((y x)) (foo y))))
               (cond (fn (x) (& (>= x 0) (< (succ x) 11)))))
        (f 0))
    ";
    let ast = parse(expr).unwrap();
    let code = Compiler::new().compile(ast);
    println!("{}", code);
    assert_eq!(ip.interpret(expr), Ok(Value::Int(10)));
}
