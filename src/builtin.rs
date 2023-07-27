use crate::vm::Value;
use std::collections::hash_set::HashSet;

pub fn symbols() -> HashSet<String> {
    vec![
        "+", "*", "-", "/", "==", "!=", ">", ">=", "<", "<=", "!", "&", "|", "if", "true", "false",
        "nil",
    ]
    .into_iter()
    .map(String::from)
    .collect()
}

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
        (x, y) => panic!("Type Error: (- {:?} {:?})", x, y),
    }
}

pub fn multiply(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
        (x, y) => panic!("Type Error: (* {:?} {:?})", x, y),
    }
}

pub fn divide(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 / y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x / y as f64),
        (x, y) => panic!("Type Error: (/ {:?} {:?})", x, y),
    }
}

pub fn eq(x: Value, y: Value) -> Value {
    match (x, y) {
        (x @ Value::Closure(_), y @ Value::Closure(_)) => {
            panic!("Type Error: (== {:?} {:?})", x, y)
        }
        (x, y) => Value::Bool(x == y),
    }
}

pub fn neq(x: Value, y: Value) -> Value {
    match (x, y) {
        (x @ Value::Closure(_), y @ Value::Closure(_)) => {
            panic!("Type Error: (!= {:?} {:?})", x, y)
        }
        (x, y) => Value::Bool(x != y),
    }
}

pub fn gt(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x > y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x > y),
        (Value::Int(x), Value::Float(y)) => Value::Bool(x as f64 > y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x > y as f64),
        (Value::String(x), Value::String(y)) => Value::Bool(x > y),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(x > y),
        (x, y) => panic!("Type Error: (> {:?} {:?})", x, y),
    }
}

pub fn gte(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x >= y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x >= y),
        (Value::Int(x), Value::Float(y)) => Value::Bool(x as f64 >= y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x >= y as f64),
        (Value::String(x), Value::String(y)) => Value::Bool(x >= y),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(x >= y),
        (x, y) => panic!("Type Error: (>= {:?} {:?})", x, y),
    }
}

pub fn lt(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x < y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x < y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) < y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x < y as f64),
        (Value::String(x), Value::String(y)) => Value::Bool(x < y),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(x < y),
        (x, y) => panic!("Type Error: (< {:?} {:?})", x, y),
    }
}

pub fn lte(x: Value, y: Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x <= y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x <= y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) <= y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x <= y as f64),
        (Value::String(x), Value::String(y)) => Value::Bool(x <= y),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(x <= y),
        (x, y) => panic!("Type Error: (<= {:?} {:?})", x, y),
    }
}

pub fn not(x: Value) -> Value {
    match x {
        Value::Bool(x) => Value::Bool(!x),
        Value::Nil => Value::Bool(true),
        _ => Value::Nil,
    }
}
