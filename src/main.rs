mod ast;
mod builtin;
mod compiler;
mod interpreter;
mod parser;
mod vm;

use compiler::Compiler;
use interpreter::Interpreter;
use parser::parse;

fn main() {
    let mut ip = Interpreter::new();
    println!("{:?}", ip.interpret("1").unwrap());
    let ast = parse("((fn (x) (+ x 1)) 2)").unwrap();
    println!("{:?}", ast.free_vars());
    let code = Compiler::new().compile(ast);
    println!("{:?}", code)
}
