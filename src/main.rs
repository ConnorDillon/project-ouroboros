mod ast;
mod builtin;
mod bytecode;
mod compiler;
mod interpreter;
mod parser;
mod vm;

use compiler::Compiler;
use interpreter::Interpreter;
use parser::parse;

fn main() {
    let mut ip = Interpreter::new();
    println!("{:?}", ip.interpret("(if true 1 2)").unwrap());
    println!("{:?}", ip.vm.stack);
    let expr = "(let ((f (fn (x) (+ x 2)))) (if (< 1 2) (f 3 4) (+ \"foo\" \"bar\")))";
    let ast = parse(expr).unwrap();
    let code = Compiler::new().compile(ast);
    println!("{}", code);
}
