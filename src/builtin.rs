use crate::vm::Value;
use std::collections::hash_set::HashSet;

pub fn symbols() -> HashSet<String> {
    vec!["+", "*", "-", "/"]
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
