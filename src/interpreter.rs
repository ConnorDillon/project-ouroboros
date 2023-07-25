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
        assert_eq!(ip.interpret("((fn (x) (fn (y) y)) 1 2)"), Ok(Value::Int(2)));
        assert_eq!(
            ip.interpret("((fn (x) (fn (y) (+ x y))) 1 2)"),
            Ok(Value::Int(3))
        );
    }
}
