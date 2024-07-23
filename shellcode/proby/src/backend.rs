use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
};

use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};

pub mod x64;
// pub mod wasm;
pub mod parse;
pub mod ssa;
// pub mod rat_fe;
pub use rat_ir::util::BinOp;
#[derive(Clone, Debug, Hash, Serialize, Deserialize)]
pub enum CondType {
    Eq,
    Greater,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum Plat {
    Linux,
    WASM,
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum Size {
    _8,
    _16,
    _32,
    _64,
}

#[derive(Clone, Debug, Hash, Serialize, Deserialize)]
pub enum Ast {
    Call(Box<Ast>, Vec<Ast>),
    Jmp(Box<Ast>, Vec<Ast>),
    Sym(String),
    Var(String),
    Assign(String, Box<Ast>),
    Alloca(Box<Ast>),
    Load(Box<Ast>, Size),
    Store(Box<Ast>, Box<Ast>, Size),
    Const(u64),
    Bin(BinOp, Box<Ast>, Box<Ast>),
    If {
        cond: Box<Ast>,
        cty: CondType,
        if_true: Box<Ast>,
        if_false: Box<Ast>,
    },
    Param(usize),
    Plat(Plat, Vec<Ast>),
    OS,
    Ret(Box<Ast>),
    DoWhile {
        body: Box<Ast>,
        cond: Box<Ast>,
        cty: CondType,
    },
    Many(Box<NonEmpty<Ast>>),
    Data(Cow<'static, [u8]>),
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Mod<T = Funct> {
    pub funcs: BTreeMap<String, T>,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Funct {
    pub body: Ast,
}
impl Ast {
    pub fn fold<E>(&self, go: &mut impl FnMut(Ast) -> Result<Ast, E>) -> Result<Ast, E> {
        let r = match self {
            Ast::Call(a, b) => Ast::Call(
                Box::new(a.fold(go)?),
                b.iter()
                    .map(|c| c.fold(go))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Ast::Jmp(_, _) => todo!(),
            Ast::Sym(_) => todo!(),
            Ast::Var(_) => todo!(),
            Ast::Assign(_, _) => todo!(),
            Ast::Alloca(_) => todo!(),
            Ast::Load(_, _) => todo!(),
            Ast::Store(_, _, _) => todo!(),
            Ast::Const(_) => todo!(),
            Ast::Bin(_, _, _) => todo!(),
            Ast::If {
                cond,
                cty,
                if_true,
                if_false,
            } => todo!(),
            Ast::Param(_) => todo!(),
            Ast::Plat(_, _) => todo!(),
            Ast::OS => todo!(),
            Ast::Ret(_) => todo!(),
            Ast::DoWhile { body, cond, cty } => todo!(),
            Ast::Many(_) => todo!(),
            Ast::Data(_) => todo!(),
        };
        return go(r);
    }
    pub fn size(&self, go: &mut usize) {
        match self {
            Ast::Call(a, b) => {
                a.size(go);
                for a in b {
                    a.size(go);
                }
            }
            Ast::Jmp(a, b) => {
                a.size(go);
                for a in b {
                    a.size(go);
                }
            }
            Ast::Sym(_) => {}
            Ast::Var(_) => {}
            Ast::Assign(_, a) => a.size(go),
            Ast::Alloca(a) => a.size(go),
            Ast::Load(a, _) => {
                a.size(go);
            }
            Ast::Store(a, b, _) => {
                a.size(go);
                b.size(go);
            }
            Ast::Const(_) => {}
            Ast::Bin(_, a, b) => {
                a.size(go);
                b.size(go);
            }
            Ast::If {
                cond,
                cty,
                if_true,
                if_false,
            } => {
                cond.size(go);
                if_true.size(go);
                if_false.size(go);
            }
            Ast::Param(p) => {
                *go = (*go).max(*p);
            }
            Ast::Plat(_, a) => {
                for b in a {
                    b.size(go)
                }
            }
            Ast::OS => {}
            Ast::Ret(a) => a.size(go),
            Ast::DoWhile { body, cond, cty } => {
                body.size(go);
                cond.size(go);
            }
            Ast::Many(a) => {
                for a in a.iter() {
                    a.size(go)
                }
            }
            Ast::Data(_) => {}
        }
    }
    pub fn vars(&self, go: &mut BTreeSet<String>) {
        match self {
            Ast::Call(a, b) => {
                a.vars(go);
                for c in b {
                    c.vars(go)
                }
            }
            Ast::Jmp(a, b) => {
                a.vars(go);
                for c in b {
                    c.vars(go)
                }
            }
            Ast::Sym(_) => {}
            Ast::Var(a) => {
                go.insert(a.clone());
            }
            Ast::Assign(a, b) => {
                go.insert(a.clone());
                b.vars(go);
            }
            Ast::Alloca(a) => a.vars(go),
            Ast::Load(a, _) => {
                a.vars(go);
            }
            Ast::Store(a, b, _) => {
                a.vars(go);
                b.vars(go);
            }
            Ast::Const(_) => {}
            Ast::Bin(_, a, b) => {
                a.vars(go);
                b.vars(go);
            }
            Ast::If {
                cond,
                if_true,
                if_false,
                cty,
            } => {
                cond.vars(go);
                if_true.vars(go);
                if_false.vars(go);
            }
            Ast::Param(_) => {}
            Ast::Plat(_, a) => {
                for a in a {
                    a.vars(go)
                }
            }
            Ast::OS => {}
            Ast::Ret(a) => a.vars(go),
            Ast::DoWhile { body, cond, cty } => {
                body.vars(go);
                cond.vars(go);
            }
            Ast::Many(m) => {
                for n in m.iter() {
                    n.vars(go);
                }
            }
            Ast::Data(_) => {}
        }
    }
}
pub trait ToAst {
    fn to_ast(&self) -> Ast;
}

#[cfg(test)]
mod tests {
    use super::*;
}
