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
    Pop,
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Jump(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef {
    pub entry: usize,
    pub args: u8,
}

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub enum Function {
    Add,
    Subtract,
    Multiply,
    Divide,
    EQ,
    NEQ,
    GT,
    GTE,
    LT,
    LTE,
    Not,
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

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub struct Closure {
    closed_vals: Rc<Vec<Value>>,
    fun_args: u8,
    fun: Function,
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
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

    pub fn to_closure(self) -> Closure {
        if let Value::Closure(c) = self {
            c.clone()
        } else {
            panic!("Expected closure but found: {:?}", self)
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
    pub code: ByteCode,
    pub stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
}

struct CallFrame {
    stack_ptr: usize,
    return_ptr: usize,
    extra_args: u8,
    closure: Option<Closure>,
}

struct Popper<'a> {
    closure: &'a Closure,
    stack: &'a mut Vec<Value>,
    top: usize,
}

impl<'a> Popper<'a> {
    fn new(closure: &'a Closure, stack: &'a mut Vec<Value>) -> Self {
        Popper {
            closure,
            stack,
            top: closure.closed_vals.len(),
        }
    }

    fn pop(&mut self) -> Value {
        if self.top > 0 {
            self.top -= 1;
            self.closure.closed_vals[self.top].clone()
        } else {
            self.stack.pop().unwrap()
        }
    }
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

    fn get_var(&mut self, depth: usize, slot: usize) {
        let frame = &self.call_frames[self.call_frames.len() - 1 - depth];
        let var = if let Some(closure) = &frame.closure {
            let stack_args = closure.fun_args as usize - closure.closed_vals.len();
            if slot >= stack_args {
                closure.closed_vals[slot - stack_args].clone()
            } else {
                self.stack[frame.stack_ptr + slot].clone()
            }
        } else {
            self.stack[frame.stack_ptr + slot].clone()
        };
        self.stack.push(var);
    }

    fn apply1(&mut self, closure: Closure, f: fn(Value) -> Value) {
        let mut popper = Popper::new(&closure, &mut self.stack);
        let x = popper.pop();
        self.stack.push(f(x));
    }

    fn apply2(&mut self, closure: Closure, f: fn(Value, Value) -> Value) {
        let mut popper = Popper::new(&closure, &mut self.stack);
        let x = popper.pop();
        let y = popper.pop();
        self.stack.push(f(x, y));
    }

    fn apply(&mut self, ap_args: u8) {
        let cl = self.stack.pop().unwrap().to_closure();
        let cur_args = cl.closed_vals.len() as u8 + ap_args;
        if cur_args < cl.fun_args {
            let stack_ptr = self.stack.len() - (ap_args as usize);
            let mut vals = Vec::with_capacity(cur_args as usize);
            vals.extend(
                self.stack
                    .drain(stack_ptr..)
                    .chain(cl.closed_vals.iter().cloned()),
            );
            self.stack.push(Value::closure(vals, cl.fun_args, cl.fun));
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
                    let stack_ptr = self.stack.len() - (ap_args - extra_args) as usize;
                    self.call_frames.push(CallFrame {
                        stack_ptr,
                        return_ptr: self.ip,
                        extra_args,
                        closure: Some(cl),
                    });
                    self.ip = ip;
                }
            }
        }
    }

    fn return_op(&mut self) {
        let frame = self.call_frames.pop().unwrap();
        let result = self.stack.pop().unwrap();
        self.stack.truncate(frame.stack_ptr);
        self.stack.push(result);
        self.ip = frame.return_ptr;
        if frame.extra_args > 0 {
            self.apply(frame.extra_args)
        }
    }

    pub fn exec(&mut self) -> Value {
        while let Some(op) = self.code.code.get(self.ip) {
            self.ip += 1;
            match *op {
                Op::Const(i) => self.const_op(i),
                Op::Function(a, f) => self.stack.push(Value::closure(Vec::new(), a, f)),
                Op::Apply(ap_args) => self.apply(ap_args),
                Op::Return => self.return_op(),
                Op::GetVar(depth, slot) => self.get_var(depth, slot),
                Op::BeginFrame => self.call_frames.push(CallFrame {
                    stack_ptr: self.stack.len(),
                    return_ptr: 0,
                    extra_args: 0,
                    closure: None,
                }),
                Op::EndFrame => {
                    let result = self.stack.pop().unwrap();
                    self.stack
                        .truncate(self.call_frames.pop().unwrap().stack_ptr);
                    self.stack.push(result);
                }
                Op::JumpIfTrue(i) => {
                    if self.stack.last().unwrap().to_bool() {
                        self.ip = i;
                    }
                }
                Op::JumpIfFalse(i) => {
                    if !self.stack.last().unwrap().to_bool() {
                        self.ip = i;
                    }
                }
                Op::Jump(i) => self.ip = i,
                Op::Pop => {
		    self.stack.pop();
		}
            }
        }
        self.stack.pop().unwrap()
    }

    fn const_op(&mut self, idx: usize) {
        self.stack.push(self.code.consts[idx].clone())
    }
}
