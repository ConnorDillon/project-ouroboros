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
    println!("{:?}", ip.interpret("(if true 1 2)").unwrap());
    println!("{:?}", ip.vm.stack);
    let expr = "(letrec ((f (fn (x) (if (> x 9) x (f (+ x 1)))))) (f 0))";
    let ast = parse(expr).unwrap();
    let code = Compiler::new().compile(ast);
    println!("{}", code);
    assert_eq!(ip.interpret(expr), Ok(Value::Int(10)));
}
