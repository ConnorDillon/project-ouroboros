use std::fmt;
use std::rc::Rc;

use crate::builtin;
use crate::bytecode::{ByteCode, Function, Op};
use crate::stack::{Item, Stack};

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub closed_vals: Rc<Vec<Item>>,
    pub fun_args: u8,
    pub fun: Function,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Closure(closed_vals={}, fun_args={}, fun={})",
            self.closed_vals.len(),
            self.fun_args,
            self.fun
        )
    }
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Int(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Closure(x) => write!(f, "{}", x),
        }
    }
}

impl Value {
    pub fn closure(vals: Vec<Item>, fun_args: u8, fun: Function) -> Value {
        Value::Closure(Closure {
            closed_vals: Rc::new(vals),
            fun_args,
            fun,
        })
    }

    pub fn string(s: &str) -> Value {
        Value::String(Rc::from(s))
    }

    pub fn to_closure(self) -> Closure {
        if let Value::Closure(c) = self {
            c.clone()
        } else {
            panic!("Expected closure but found: {}", self)
        }
    }

    pub fn to_bool(&self) -> bool {
        match &self {
            Value::Nil => false,
            Value::Bool(false) => false,
            _ => true,
        }
    }
}

pub struct VM {
    pub ip: usize,
    pub code: ByteCode<Value>,
    pub stack: Stack,
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: 0,
            code: ByteCode::new(),
            stack: Stack::new(),
        }
    }

    pub fn load(&mut self, code: ByteCode<Value>) {
        self.ip = code.entry;
        self.code = code;
        self.stack = Stack::new();
    }

    fn apply1(&mut self, closure: Closure, f: fn(Value) -> Value) {
        let mut cs = self.stack.closed_stack(closure);
        let x = cs.pop_val();
        self.stack.push_val(f(x));
    }

    fn apply2(&mut self, closure: Closure, f: fn(Value, Value) -> Value) {
        let mut cs = self.stack.closed_stack(closure);
        let x = cs.pop_val();
        let y = cs.pop_val();
        self.stack.push_val(f(x, y));
    }

    fn apply(&mut self, tail: Option<usize>, ap_args: u8) {
        let cl = self.stack.pop_val().to_closure();
        let cur_args = cl.closed_vals.len() as u8 + ap_args;
        if cur_args < cl.fun_args {
            let stack_ptr = self.stack.len() - (ap_args as usize);
            let mut vals = Vec::with_capacity(cur_args as usize);
            vals.extend(
                self.stack
                    .items
                    .drain(stack_ptr..)
                    .chain(cl.closed_vals.iter().cloned()),
            );
            self.stack
                .push_val(Value::closure(vals, cl.fun_args, cl.fun));
        } else {
            match cl.fun {
                Function::Add => self.apply2(cl, builtin::add),
                Function::Subtract => self.apply2(cl, builtin::subtract),
                Function::Multiply => self.apply2(cl, builtin::multiply),
                Function::Divide => self.apply2(cl, builtin::divide),
                Function::EQ => self.apply2(cl, builtin::eq),
                Function::NEQ => self.apply2(cl, builtin::neq),
                Function::GT => self.apply2(cl, builtin::gt),
                Function::GTE => self.apply2(cl, builtin::gte),
                Function::LT => self.apply2(cl, builtin::lt),
                Function::LTE => self.apply2(cl, builtin::lte),
                Function::Not => self.apply1(cl, builtin::not),
                Function::Defined(ip) => {
                    let extra_args = cur_args - cl.fun_args;
                    match tail {
                        Some(depth) => {
                            self.stack
                                .reuse_frame(depth, ap_args as usize, extra_args, cl)
                        }
                        None => {
                            let stack_ptr = self.stack.len() - (ap_args - extra_args) as usize;
                            self.stack.push_frame(stack_ptr, self.ip, extra_args, cl);
                        }
                    }
                    self.ip = ip;
                }
            }
        }
    }

    fn return_op(&mut self) {
        let frame = self.stack.pop_frame();
        self.ip = frame.return_ptr;
        if frame.extra_args > 0 {
            self.apply(None, frame.extra_args)
        }
    }

    pub fn exec(&mut self) -> Value {
        while let Some(op) = self.code.ops.get(self.ip) {
            if cfg!(debug_assertions) {
                print!("{:0>8} {:25} STACK TOP:", self.ip, op.to_string());
                if let Some(top) = self.stack.items.last() {
                    print!(" {:60}", top.to_string());
                } else {
                    print!(" {:60}", String::from("<none>"));
                }
                print!(" TOP FRAME {}: ", self.stack.frames.len());
                if let Some(top) = self.stack.frames.last() {
                    println!("{}", top);
                } else {
                    println!("<none>");
                }
            }
            self.ip += 1;
            match *op {
                Op::Const(i) => self.const_op(i),
                Op::Function(a, f) => self.stack.push_val(Value::closure(Vec::new(), a, f)),
                Op::Apply(ap_args) => self.apply(None, ap_args),
                Op::TailApply(depth, ap_args) => self.apply(Some(depth), ap_args),
                Op::Return => self.return_op(),
                Op::GetVar(depth, slot) => self.stack.get_push(depth, slot),
                Op::BeginFrame => self.stack.begin_frame(),
                Op::EndFrame => {
                    self.stack.pop_frame();
                }
                Op::JumpIfTrue(i) => {
                    if self.stack.top_val().to_bool() {
                        self.ip += i - 1;
                    }
                }
                Op::JumpIfFalse(i) => {
                    if !self.stack.top_val().to_bool() {
                        self.ip += i - 1;
                    }
                }
                Op::Jump(i) => self.ip += i - 1,
                Op::Pop => {
                    self.stack.pop_item();
                }
                Op::EmptyVar => self.stack.push_empty_var(),
                Op::InitVar(slot) => self.stack.init_var(slot),
            }
        }
        self.stack.pop_val()
    }

    fn const_op(&mut self, idx: usize) {
        self.stack.push_val(self.code.consts[idx].clone())
    }
}
