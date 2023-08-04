use crate::vm::{Closure, Value};
use std::cell::OnceCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Value(Value),
    InitVar(Rc<OnceCell<Value>>),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
	match self {
	    Item::Value(x) => write!(f, "{}", x),
	    Item::InitVar(x) => {
		match x.get() {
		    Some(x) => write!(f, "InitVar({})", x),
		    None => write!(f, "InitVar(Empty)"),
		}
	    }
	}
        
    }
}

impl Item {
    pub fn to_value(self) -> Value {
        match self {
            Item::Value(x) => x,
            Item::InitVar(x) => x.get().expect("Expected Value but found emtpy var").clone(),
        }
    }

    pub fn value_ref(&self) -> &Value {
        match self {
            Item::Value(x) => &x,
            Item::InitVar(x) => x.get().expect("Expected Value but found emtpy var"),
        }
    }

    pub fn set(&self, val: Value) {
        match self {
            Item::InitVar(x) => x
                .set(val)
                .expect(&format!("InitVar already set: {}", self)),
            _ => panic!("Expected InitVar but found: {}", self),
        }
    }
}

#[derive(Debug)]
pub struct CallFrame {
    pub stack_ptr: usize,
    pub return_ptr: usize,
    pub extra_args: u8,
    pub closure: Option<Closure>,
}

#[derive(Debug)]
pub struct Stack {
    pub items: Vec<Item>,
    pub frames: Vec<CallFrame>,
}

pub struct ClosedStack<'a> {
    closure: Closure,
    items: &'a mut Vec<Item>,
    closed_vals: usize,
}

impl<'a> ClosedStack<'a> {
    pub fn pop_val(&mut self) -> Value {
        if self.closed_vals > 0 {
            self.closed_vals -= 1;
            self.closure.closed_vals[self.closed_vals]
                .clone()
                .to_value()
        } else {
            self.items.pop().expect("Unbalanced stack error").to_value()
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            items: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn pop_item(&mut self) -> Item {
        self.items.pop().expect("Unbalanced stack error")
    }

    pub fn pop_val(&mut self) -> Value {
        self.pop_item().to_value()
    }

    pub fn top_val(&self) -> &Value {
        self.items
            .last()
            .expect("Unbalanced stack error")
            .value_ref()
    }

    pub fn get_push(&mut self, frame: usize, slot: usize) {
        let frame = &self.frames[self.frames.len() - 1 - frame];
        let var = if let Some(closure) = &frame.closure {
            let stack_args = closure.fun_args as usize - closure.closed_vals.len();
            if slot >= stack_args {
                closure.closed_vals[slot - stack_args].clone()
            } else {
                self.items[frame.stack_ptr + slot].clone()
            }
        } else {
            self.items[frame.stack_ptr + slot].clone()
        };
        self.items.push(var);
    }

    pub fn push_val(&mut self, val: Value) {
        self.items.push(Item::Value(val))
    }

    pub fn push_empty_var(&mut self) {
        self.items.push(Item::InitVar(Rc::new(OnceCell::new())))
    }

    pub fn init_var(&mut self, slot: usize) {
        let sp = self
            .frames
            .last()
            .expect("Unbalanced stack error")
            .stack_ptr;
        let val = self.pop_val();
        self.items[sp + slot].set(val);
    }

    pub fn begin_frame(&mut self) {
        self.frames.push(CallFrame {
            stack_ptr: self.len(),
            return_ptr: 0,
            extra_args: 0,
            closure: None,
        })
    }

    pub fn push_frame(
        &mut self,
        stack_ptr: usize,
        return_ptr: usize,
        extra_args: u8,
        closure: Closure,
    ) {
        self.frames.push(CallFrame {
            stack_ptr,
            return_ptr,
            extra_args,
            closure: Some(closure),
        })
    }

    pub fn pop_frame(&mut self) -> CallFrame {
        let frame = self.frames.pop().expect("Unbalanced stack error");
        let result = self.pop_item();
        self.items.truncate(frame.stack_ptr);
        self.items.push(result);
        frame
    }

    // pub fn reuse_frame(&mut self, ap_args: usize, closure: Closure) {
    //     let frame = self.frames.last_mut().expect("Unbalanced stack error");
    //     frame.closure = Some(closure);
    //     let stack_ptr = frame.stack_ptr;
    //     for slot in (0..ap_args).rev() {
    //         self.items[stack_ptr + slot] = self.pop_item();
    //     }
    //     self.items.truncate(stack_ptr + ap_args);
    // }

    pub fn closed_stack(&mut self, closure: Closure) -> ClosedStack {
        let closed_vals = closure.closed_vals.len();
        ClosedStack {
            closure,
            items: &mut self.items,
            closed_vals,
        }
    }
}
