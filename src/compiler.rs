use crate::ast::{Fun, AST};
use crate::vm::{ByteCode, Function, FunctionDef, Op, Value};

#[derive(Debug, PartialEq, Clone)]
struct Var {
    name: String,
    depth: usize,
    slot: usize,
}

pub struct Compiler {
    code: ByteCode,
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

    fn compile_part(&mut self, ast: AST<usize>) {
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
                "bool" => self.code.add_op(Op::Function(1, Function::Not)),
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
            AST::Expr(x) => {
                let args = x.len() - 1;
                for a in x.into_iter().rev() {
                    self.compile_part(a);
                }
                self.code.add_op(Op::Apply(args as u8));
            }
            AST::Let(vars, expr) => {
                self.depth = self.depth + 1;
                self.code.add_op(Op::BeginFrame);
                let var_count = vars.len();
                for (slot, (name, vexpr)) in vars.into_iter().enumerate() {
                    self.vars.push(Var {
                        name,
                        slot,
                        depth: self.depth,
                    });
                    self.compile_part(vexpr)
                }
                self.compile_part(*expr);
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

    pub fn compile(mut self, ast: AST<Fun>) -> ByteCode {
        let lifted_ast = ast.lift_lambdas();
        for fun in lifted_ast.funs {
            let entry = self.code.code.len();
            self.code.funs.push(FunctionDef {
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
            self.compile_part(fun.body);
            self.code.add_op(Op::Return);
            self.vars.truncate(0);
        }
        let entry = self.code.code.len();
        self.compile_part(lifted_ast.ast);
        self.code.entry = entry;
        self.code
    }
}
