use std::{collections::HashMap, fmt};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Op {
    Const(usize),
    BeginFrame,
    EndFrame,
    GetVar(usize, usize),
    Return,
    Apply(u8),
    TailApply(usize, u8),
    Function(u8, Function),
    Pop,
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Jump(usize),
    EmptyVar,
    InitVar(usize),
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Const(x) => write!(f, "CONST: {}", x),
            Op::BeginFrame => write!(f, "BEGIN FRAME"),
            Op::EndFrame => write!(f, "END FRAME"),
            Op::GetVar(x, y) => write!(f, "GET VAR: {} {}", x, y),
            Op::Return => write!(f, "RETURN"),
            Op::Apply(x) => write!(f, "APPLY: {}", x),
            Op::TailApply(x, y) => write!(f, "TAIL APPLY: {} {}", x, y),
            Op::Function(x, y) => write!(f, "FUNCTION: {} {}", y, x),
            Op::Pop => write!(f, "POP"),
            Op::JumpIfTrue(x) => write!(f, "JUMP IF TRUE: {}", x),
            Op::JumpIfFalse(x) => write!(f, "JUMP IF FALSE: {}", x),
            Op::Jump(x) => write!(f, "JUMP: {}", x),
            Op::EmptyVar => write!(f, "EMPTY VAR"),
            Op::InitVar(x) => write!(f, "INIT VAR: {}", x),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionMeta {
    pub name: String,
    pub entry: usize,
    pub args: u8,
}

impl fmt::Display for FunctionMeta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function(name=")?;
        if self.name.len() == 0 {
            write!(f, "<anonymous>")?;
        } else {
            write!(f, "{}", self.name)?;
        }
        write!(f, ", args={}, entry={})", self.args, self.entry)
    }
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Add => write!(f, "ADD"),
            Function::Subtract => write!(f, "SUBTRACT"),
            Function::Multiply => write!(f, "MULTIPLY"),
            Function::Divide => write!(f, "DIVIDE"),
            Function::EQ => write!(f, "EQ"),
            Function::NEQ => write!(f, "NEQ"),
            Function::GT => write!(f, "GT"),
            Function::GTE => write!(f, "GTE"),
            Function::LT => write!(f, "LT"),
            Function::LTE => write!(f, "LTE"),
            Function::Not => write!(f, "NOT"),
            Function::Defined(x) => write!(f, "DEFINED({})", x),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ByteCode<T> {
    pub consts: Vec<T>,
    pub funs: Vec<FunctionMeta>,
    pub ops: Vec<Op>,
    pub entry: usize,
}

impl<T: fmt::Debug> fmt::Display for ByteCode<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fun_map: HashMap<usize, &FunctionMeta> =
            self.funs.iter().map(|x| (x.entry, x)).collect();
        writeln!(f, "### SUMMARY ###")?;
        writeln!(f, "ENTRY: {}", self.entry)?;
        writeln!(f, "FUNCTIONS: {}", self.funs.len())?;
        writeln!(f, "CONSTANTS: {}", self.consts.len())?;
        writeln!(f, "OPERATIONS: {}", self.ops.len())?;
        writeln!(f, "")?;
        writeln!(f, "### FUNCTIONS ###")?;
        if self.funs.len() == 0 {
            writeln!(f, "<NONE>")?;
        }
        for (idx, fun) in self.funs.iter().enumerate() {
            writeln!(f, "{:0>8} {}", idx, fun)?;
        }
        writeln!(f, "")?;
        writeln!(f, "### CONSTANTS ###")?;
        if self.consts.len() == 0 {
            writeln!(f, "<NONE>")?;
        }
        for (idx, con) in self.consts.iter().enumerate() {
            writeln!(f, "{:0>8} {:?}", idx, con)?;
        }
        writeln!(f, "")?;
        writeln!(f, "### OPERATIONS ###")?;
        if self.ops.len() == 0 {
            writeln!(f, "<NONE>")?;
        }
        for (idx, op) in self.ops.iter().enumerate() {
            if idx == self.entry {
                writeln!(f, "## MAIN ##")?;
            } else if let Some(x) = fun_map.get(&idx) {
                writeln!(f, "## {} ##", x)?;
            }
            writeln!(f, "{:0>8} {}", idx, op)?;
        }
        write!(f, "### END ###")
    }
}

impl<T> ByteCode<T> {
    pub fn new() -> ByteCode<T> {
        ByteCode {
            consts: Vec::new(),
            funs: Vec::new(),
            ops: Vec::new(),
            entry: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    pub fn add_op(&mut self, op: Op) {
        self.ops.push(op);
    }

    pub fn add_const(&mut self, val: T) {
        self.ops.push(Op::Const(self.consts.len()));
        self.consts.push(val)
    }
}
