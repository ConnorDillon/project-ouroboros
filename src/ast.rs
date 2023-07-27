use crate::builtin::symbols;
use std::collections::hash_set::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum AST<T> {
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Expr(Vec<AST<T>>),
    Let(Vec<(String, AST<T>)>, Box<AST<T>>),
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
            AST::Let(bs, e) => {
                let fvs: HashSet<String> =
                    bs.iter().map(|(_, e)| e.free_vars()).flatten().collect();
                fvs.union(&e.free_vars()).cloned().collect()
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

pub struct LiftedAST {
    pub funs: Vec<LiftedFun>,
    pub ast: AST<usize>,
}

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
        AST::Let(vars, expr) => AST::Let(
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
    use crate::parser::parse;

    #[test]
    fn test_free_vars() {
        assert_eq!(parse("(fn (x) x))").unwrap().free_vars().len(), 0);
        assert_eq!(parse("(fn (x) (+ x y))").unwrap().free_vars().len(), 1);
        assert_eq!(parse("(if true false nil)").unwrap().free_vars().len(), 0);
    }
}
