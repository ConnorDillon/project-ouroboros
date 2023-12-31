use crate::ast::{Fun, Rec, AST};
use crate::bytecode::{ByteCode, Function, FunctionMeta, Op};
use crate::vm::Value;

#[derive(Debug, PartialEq, Clone)]
struct Var {
    name: String,
    depth: usize,
    slot: usize,
}

pub struct Compiler {
    code: ByteCode<Value>,
    vars: Vec<Var>,
    depth: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            code: ByteCode::new(),
            vars: Vec::new(),
            depth: 0,
        }
    }

    fn compile_part(&mut self, tail: bool, ast: AST<usize>) {
        match ast {
            AST::Int(x) => self.code.add_const(Value::Int(x)),
            AST::Float(x) => self.code.add_const(Value::Float(x)),
            AST::String(x) => self.code.add_const(Value::string(&*x)),
            AST::Symbol(x) => match &*x {
                "nil" => self.code.add_const(Value::Nil),
                "true" => self.code.add_const(Value::Bool(true)),
                "false" => self.code.add_const(Value::Bool(false)),
                "+" => self.code.add_op(Op::Function(2, Function::Add)),
                "-" => self.code.add_op(Op::Function(2, Function::Subtract)),
                "*" => self.code.add_op(Op::Function(2, Function::Multiply)),
                "/" => self.code.add_op(Op::Function(2, Function::Divide)),
                "==" => self.code.add_op(Op::Function(2, Function::EQ)),
                "!=" => self.code.add_op(Op::Function(2, Function::NEQ)),
                ">" => self.code.add_op(Op::Function(2, Function::GT)),
                ">=" => self.code.add_op(Op::Function(2, Function::GTE)),
                "<" => self.code.add_op(Op::Function(2, Function::LT)),
                "<=" => self.code.add_op(Op::Function(2, Function::LTE)),
                "!" => self.code.add_op(Op::Function(1, Function::Not)),
                _ => {
                    let mut found = false;
                    for var in self.vars.iter().rev() {
                        if var.name == x {
                            self.code
                                .add_op(Op::GetVar(self.depth - var.depth, var.slot));
                            found = true;
                            break;
                        };
                    }
                    if !found {
                        panic!("Symbol {} not found in {:?} {}", x, self.vars, self.depth)
                    }
                }
            },
            AST::Expr(x) => match &x[0] {
                AST::Symbol(s) => match s.as_str() {
                    "if" => self.compile_if(tail, x),
                    "|" => self.compile_or(tail, x),
                    "&" => self.compile_and(tail, x),
                    _ => self.compile_appl(tail, x),
                },
                _ => self.compile_appl(tail, x),
            },
            AST::Let(rec, vars, expr) => {
                self.depth = self.depth + 1;
                self.code.add_op(Op::BeginFrame);
                let var_count = vars.len();
                if rec == Rec::Rec {
                    for slot in 0..var_count {
                        self.code.add_op(Op::EmptyVar);
                        self.vars.push(Var {
                            name: vars[slot].0.clone(),
                            slot,
                            depth: self.depth,
                        })
                    }
                }
                for (slot, (name, vexpr)) in vars.into_iter().enumerate() {
                    if rec == Rec::NonRec {
                        self.vars.push(Var {
                            name,
                            slot,
                            depth: self.depth,
                        });
                    }
                    self.compile_part(false, vexpr);
                    if rec == Rec::Rec {
                        self.code.add_op(Op::InitVar(slot));
                    }
                }
                self.compile_part(tail, *expr);
                self.vars.truncate(var_count);
                self.code.add_op(Op::EndFrame);
                self.depth = self.depth - 1;
            }
            AST::Fn(i) => {
                let f = &self.code.funs[i];
                self.code
                    .add_op(Op::Function(f.args, Function::Defined(f.entry)))
            }
        }
    }

    fn compile_if(&mut self, tail: bool, mut expr: Vec<AST<usize>>) {
        if expr.len() != 4 {
            panic!("Bad if expression: {:?}", expr);
        }
        let else_expr = expr.pop().unwrap();
        let then_expr = expr.pop().unwrap();
        let cond_expr = expr.pop().unwrap();
        self.compile_part(false, cond_expr);
        let jump_if_op = self.code.len();
        self.code.add_op(Op::JumpIfFalse(0));
        self.code.add_op(Op::Pop);
        self.compile_part(tail, then_expr);
        let jump_op = self.code.len();
        self.code.add_op(Op::Jump(0));
        self.code.ops[jump_if_op] = Op::JumpIfFalse(self.code.len() - jump_if_op);
        self.code.add_op(Op::Pop);
        self.compile_part(tail, else_expr);
        self.code.ops[jump_op] = Op::Jump(self.code.len() - jump_op);
    }

    fn compile_or(&mut self, tail: bool, mut expr: Vec<AST<usize>>) {
        if expr.len() != 3 {
            panic!("Bad or expression: {:?}", expr);
        }
        let expr2 = expr.pop().unwrap();
        let expr1 = expr.pop().unwrap();
        self.compile_part(false, expr1);
        let jump_if_op = self.code.len();
        self.code.add_op(Op::JumpIfTrue(0));
        self.compile_part(tail, expr2);
        self.code.ops[jump_if_op] = Op::JumpIfTrue(self.code.len() - jump_if_op);
    }

    fn compile_and(&mut self, tail: bool, mut expr: Vec<AST<usize>>) {
        if expr.len() != 3 {
            panic!("Bad and expression: {:?}", expr);
        }
        let expr2 = expr.pop().unwrap();
        let expr1 = expr.pop().unwrap();
        self.compile_part(false, expr1);
        let jump_if_op = self.code.len();
        self.code.add_op(Op::JumpIfFalse(0));
        self.compile_part(tail, expr2);
        self.code.ops[jump_if_op] = Op::JumpIfFalse(self.code.len() - jump_if_op);
    }

    fn compile_appl(&mut self, tail: bool, expr: Vec<AST<usize>>) {
        let args = expr.len() - 1;
        for a in expr.into_iter().rev() {
            self.compile_part(false, a);
        }
        self.code.add_op(if tail {
            Op::TailApply(self.depth, args as u8)
        } else {
            Op::Apply(args as u8)
        });
    }

    pub fn compile(mut self, ast: AST<Fun>) -> ByteCode<Value> {
        let lifted_ast = ast.lift_lambdas();
        for fun in lifted_ast.funs {
            let entry = self.code.len();
            self.code.funs.push(FunctionMeta {
                name: fun.name,
                args: fun.args.len() as u8,
                entry,
            });
            for (slot, name) in fun.args.into_iter().rev().enumerate() {
                self.vars.push(Var {
                    name,
                    slot,
                    depth: 0,
                });
            }
            self.compile_part(true, fun.body);
            self.code.add_op(Op::Return);
            self.vars.truncate(0);
        }
        let entry = self.code.len();
        self.compile_part(false, lifted_ast.ast);
        self.code.entry = entry;
        self.code
    }
}
