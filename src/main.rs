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
    println!("{:?}", ip.interpret("(if true 1 2)").unwrap());
    println!("{:?}", ip.vm.stack);
    let ast = parse("(if true 1 2)").unwrap();
    println!("{:?}", ast.free_vars());
    let code = Compiler::new().compile(ast);
    println!("{:?}", code)
}
