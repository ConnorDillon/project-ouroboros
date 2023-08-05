use crate::builtin::symbols;
use std::collections::hash_set::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum Rec {
    Rec,
    NonRec,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AST<T> {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Expr(Vec<AST<T>>),
    Let(Rec, Vec<(String, AST<T>)>, Box<AST<T>>),
    Fn(T),
}

impl AST<Fun> {
    pub fn lift_lambdas(self) -> LiftedAST {
        let mut funs = Vec::new();
        let ast = lift(&mut funs, self);
        LiftedAST { funs, ast }
    }

    pub fn free_vars(&self) -> HashSet<String> {
        match self {
            AST::Symbol(s) => {
                let mut hs = HashSet::with_capacity(1);
                if !symbols().contains(s) {
                    hs.insert(s.clone());
                }
                hs
            }
            AST::Fn(f) => f.free_vars(),
            AST::Let(r, bs, e) => {
                let mut bound: HashSet<String> = match r {
                    Rec::NonRec => HashSet::new(),
                    Rec::Rec => bs.iter().map(|(v, _)| v).cloned().collect(),
                };
                let mut free: HashSet<String> = HashSet::new();
                for (v, e) in bs {
                    if *r == Rec::NonRec {
                        bound.insert(v.clone());
                    }
                    for f in e.free_vars().difference(&bound) {
                        free.insert(f.clone());
                    }
                }
                for f in e.free_vars().difference(&bound) {
                    free.insert(f.clone());
                }
                free
            }
            AST::Expr(es) => es.iter().map(|e| e.free_vars()).flatten().collect(),
            _ => HashSet::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    pub args: Vec<String>,
    pub body: Box<AST<Fun>>,
}

impl Fun {
    fn free_vars(&self) -> HashSet<String> {
        let bound = self.args.iter().cloned().collect();
        self.body.free_vars().difference(&bound).cloned().collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiftedAST {
    pub funs: Vec<LiftedFun>,
    pub ast: AST<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiftedFun {
    pub args: Vec<String>,
    pub body: AST<usize>,
}

fn lift(funs: &mut Vec<LiftedFun>, ast: AST<Fun>) -> AST<usize> {
    match ast {
        AST::Fn(f) => {
            let free_vars = f.free_vars();
            let mut args = Vec::with_capacity(free_vars.len() + f.args.len());
            args.extend(free_vars.iter().cloned().chain(f.args.into_iter()));
            let body = lift(funs, *f.body);
            let lf = LiftedFun { args, body };
            let idx = funs.len();
            funs.push(lf);
            let fun = AST::Fn(idx);
            if free_vars.len() > 0 {
                let mut expr = vec![fun];
                expr.extend(free_vars.into_iter().map(AST::Symbol));
                AST::Expr(expr)
            } else {
                fun
            }
        }
        AST::Expr(x) => AST::Expr(x.into_iter().map(|x| lift(funs, x)).collect()),
        AST::Let(rec, vars, expr) => AST::Let(
            rec,
            vars.into_iter().map(|(x, y)| (x, lift(funs, y))).collect(),
            Box::new(lift(funs, *expr)),
        ),
        AST::Int(x) => AST::Int(x),
        AST::Float(x) => AST::Float(x),
        AST::String(x) => AST::String(x),
        AST::Symbol(x) => AST::Symbol(x),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{LiftedAST, LiftedFun, Rec, AST},
        parser::parse,
    };

    #[test]
    fn test_free_vars() {
        assert_eq!(parse("(fn (x) x))").unwrap().free_vars().len(), 0);
        assert_eq!(parse("(fn (x) (+ x y))").unwrap().free_vars().len(), 1);
        assert_eq!(parse("(if true false nil)").unwrap().free_vars().len(), 0);
        assert_eq!(
            parse("(fn (x) (let ((y 1)) y))").unwrap().free_vars().len(),
            0
        );
    }

    #[test]
    fn test_lift() {
        let ast = parse("(fn (x) (let ((y 1)) y))").unwrap().lift_lambdas();
        let lifted = LiftedAST {
            funs: vec![LiftedFun {
                args: vec!["x".into()],
                body: AST::Let(
                    Rec::NonRec,
                    vec![("y".into(), AST::Int(1))],
                    Box::new(AST::Symbol("y".into())),
                ),
            }],
            ast: AST::Fn(0),
        };
        assert_eq!(ast, lifted);
    }
}
