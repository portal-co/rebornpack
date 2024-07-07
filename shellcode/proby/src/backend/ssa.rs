use std::{
    borrow::Cow, collections::{BTreeMap, BTreeSet}, f32::consts::E, hash::{DefaultHasher, Hasher}
};

use super::{ Ast, BinOp, CondType, Plat, Size, ToAst};
use id_arena::{Arena, Id};
use indexmap::IndexMap;
use nonempty::{nonempty, NonEmpty};
use serde::{Deserialize, Serialize};
pub mod backend;
pub mod map;
pub mod rat;
pub mod rat_fe;
use std::hash::Hash;
pub fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct ValueId(String);
impl ToAst for ValueId {
    fn to_ast(&self) -> Ast {
        return Ast::Var(self.0.clone());
    }
}
#[derive(Clone, Debug)]
pub struct SSAFunc {
    pub blocks: id_arena::Arena<Block>,
    pub entry: Id<Block>,
}
impl SSAFunc {
    pub fn from_ast(ast: &Ast) -> anyhow::Result<SSAFunc> {
        let mut params = 0;
        let mut vars = BTreeSet::new();
        ast.size(&mut params);
        ast.vars(&mut vars);
        let mut n = Arena::new();
        let k = n.alloc(Default::default());
        let mut n = SSAFunc {
            blocks: n,
            entry: k,
        };
        let params = (0..=params)
            .map(|x| Instr::Param(x))
            .map(|x| n.blocks[n.entry].need(x))
            .collect::<Vec<_>>();
        let v2 = n.blocks[n.entry].need(Instr::Const(0));
        let mut vars = vars.iter().map(|v| (v.clone(), v2.clone())).collect();
        let e = n.entry;
        ast.ssa(&mut n, e, &mut vars, &params, &mut |v, f, b| {
            f.blocks[b].term = Term::Ret(v);
            return Ok(());
        })?;
        return Ok(n);
    }
    pub fn via_ast(&self, go: impl FnOnce(Ast) -> anyhow::Result<Ast>) -> anyhow::Result<SSAFunc> {
        let ast = self.to_ast();
        let ast = go(ast)?;
        return Self::from_ast(&ast);
    }
    pub fn flatten_maxssa(&self) -> anyhow::Result<SSAFunc> {
        self.via_ast(Ok)
    }
}
impl ToAst for SSAFunc {
    fn to_ast(&self) -> Ast {
        let mut b = Ast::Const(0);
        for (ki, k) in self.blocks.iter() {
            b = Ast::If {
                cond: Box::new(Ast::Bin(
                    BinOp::Sub,
                    Box::new(Ast::Var(format!("ssa_target"))),
                    Box::new(Ast::Const(ki.index() as u64)),
                )),
                cty: CondType::Eq,
                if_true: Box::new(k.to_ast()),
                if_false: Box::new(b),
            };
        }
        let mut e = nonempty![Ast::Assign(
            format!("ssa_target"),
            Box::new(Ast::Const(self.entry.index() as u64)),
        )];
        let mut max = 0;
        for v in self.blocks[self.entry].insts.values() {
            if let Instr::Param(a) = v {
                max = max.max(*a)
            }
        }
        for i in 0..=max {
            e.push(Ast::Assign(format!("${i}"), Box::new(Ast::Param(i))))
        }
        e.push(Ast::DoWhile {
            body: Box::new(b),
            cond: Box::new(Ast::Const(0)),
            cty: CondType::Eq,
        });
        return Ast::Many(Box::new(e));
    }
}
#[derive(Clone, Debug, Default)]
pub struct Block {
    pub insts: IndexMap<ValueId, Instr>,
    pub term: Term,
}
impl Block {
    pub fn need(&mut self, i: Instr) -> ValueId {
        let mut j = 0;
        while self
            .insts
            .contains_key(&ValueId(format!("z{j}${}", calculate_hash(&i))))
        {
            j += 1
        }
        let h = ValueId(format!("z{j}${}", calculate_hash(&i)));
        self.insts.insert(h.clone(), i);
        return h;
    }
    pub fn params(&self) -> usize{
        return self.insts.iter().filter_map(|(_,b)|match b{
            Instr::Param(a) => Some(*a),
            _ => None
        }).max().map(|a|a + 1).unwrap_or(0);
    }
}
impl ToAst for Block {
    fn to_ast(&self) -> Ast {
        let mut r = vec![];
        for (i, j) in self.insts.iter() {
            r.push(Ast::Assign(i.0.clone(), Box::new(j.to_ast())))
        }
        r.push(self.term.to_ast());
        return Ast::Many(Box::new(NonEmpty::from_vec(r).unwrap()));
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Instr {
    Bin(BinOp, ValueId, ValueId),
    Addrof(String),
    Call(ValueId, Vec<ValueId>),
    Param(usize),
    Const(u64),
    Load(ValueId,Size),
    Store(ValueId, ValueId,Size),
    Alloca(ValueId),
    Plat(Plat, Vec<ValueId>),
    Data(Cow<'static,[u8]>),
    OS,
}

impl ValMap for Instr {
    fn map<E>(&self, val: &mut impl FnMut(ValueId) -> Result<ValueId, E>) -> Result<Self, E> {
        return Ok(match self {
            Instr::Bin(a, b, c) => Instr::Bin(a.clone(), val(b.clone())?, val(c.clone())?),
            Instr::Call(a, b) => Instr::Call(
                val(a.clone())?,
                b.iter()
                    .map(|a| val(a.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Instr::Load(a,b) => Instr::Load(val(a.clone())?,b.clone()),
            Instr::Store(a, b,c) => Instr::Store(val(a.clone())?, val(b.clone())?,c.clone()),
            Instr::Alloca(a) => Instr::Alloca(val(a.clone())?),
            Instr::Plat(p, b) => Instr::Plat(
                p.clone(),
                b.iter()
                    .map(|a| val(a.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            _ => self.clone(),
        });
    }
}
impl ToAst for Instr {
    fn to_ast(&self) -> Ast {
        match self {
            Instr::Bin(a, b, c) => Ast::Bin(a.clone(), Box::new(b.to_ast()), Box::new(c.to_ast())),
            Instr::Addrof(s) => Ast::Sym(s.clone()),
            Instr::Call(a, b) => {
                Ast::Call(Box::new(a.to_ast()), b.iter().map(|b| b.to_ast()).collect())
            }
            Instr::Param(a) => Ast::Var(format!("${a}")),
            Instr::Const(a) => Ast::Const(a.clone()),
            Instr::Load(a,b) => Ast::Load(Box::new(a.to_ast()),b.clone()),
            Instr::Store(a, b,c) => Ast::Store(Box::new(a.to_ast()), Box::new(b.to_ast()),c.clone()),
            Instr::Alloca(a) => Ast::Alloca(Box::new(a.to_ast())),
            Instr::Plat(a, l) => Ast::Plat(a.clone(), l.iter().map(|a| a.to_ast()).collect()),
            Instr::OS => Ast::OS,
            Instr::Data(a) => Ast::Data(a.clone()),
        }
    }
}
#[derive(Clone, Debug, Default)]
pub enum Term {
    ReturnCall(ValueId, Vec<ValueId>),
    Br(Target),
    BrIf {
        cond: ValueId,
        cty: CondType,
        if_true: Target,
        if_false: Target,
    },
    Ret(ValueId),
    #[default]
    Null,
}
pub trait ValMap: Sized {
    fn map<E>(&self, val: &mut impl FnMut(ValueId) -> Result<ValueId, E>) -> Result<Self, E>;
}
impl ValMap for Term {
    fn map<E>(&self, val: &mut impl FnMut(ValueId) -> Result<ValueId, E>) -> Result<Self, E> {
        return Ok(match self {
            Term::ReturnCall(a, b) => Term::ReturnCall(
                val(a.clone())?,
                b.iter()
                    .map(|a| val(a.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Term::Br(t) => Term::Br(t.map(val)?),
            Term::BrIf {
                cond,
                cty,
                if_true,
                if_false,
            } => Term::BrIf {
                cond: val(cond.clone())?,
                cty: cty.clone(),
                if_true: if_true.map(val)?,
                if_false: if_false.map(val)?,
            },
            Term::Ret(v) => Term::Ret(val(v.clone())?),
            Term::Null => Term::Null,
        });
    }
}
impl ToAst for Term {
    fn to_ast(&self) -> Ast {
        match self {
            Term::ReturnCall(a, b) => {
                Ast::Jmp(Box::new(a.to_ast()), b.iter().map(|a| a.to_ast()).collect())
            }
            Term::Br(t) => t.to_ast(),
            Term::BrIf {
                cond,
                cty,
                if_true,
                if_false,
            } => Ast::If {
                cond: Box::new(cond.to_ast()),
                cty: cty.clone(),
                if_true: Box::new(if_true.to_ast()),
                if_false: Box::new(if_false.to_ast()),
            },
            Term::Ret(v) => Ast::Ret(Box::new(v.to_ast())),
            Term::Null => Ast::Const(0),
        }
    }
}
impl ValMap for Target {
    fn map<E>(&self, val: &mut impl FnMut(ValueId) -> Result<ValueId, E>) -> Result<Self, E> {
        return Ok(Target {
            id: self.id,
            params: self
                .params
                .iter()
                .map(|a| val(a.clone()))
                .collect::<Result<Vec<_>, _>>()?,
        });
    }
}
#[derive(Clone, Debug)]
pub struct Target {
    pub id: Id<Block>,
    pub params: Vec<ValueId>,
}
impl ToAst for Target {
    fn to_ast(&self) -> super::Ast {
        return Ast::Many(Box::new(NonEmpty {
            head: Ast::Assign(
                format!("ssa_target"),
                Box::new(Ast::Const(self.id.index() as u64)),
            ),
            tail: self
                .params
                .iter()
                .map(|a| a.to_ast())
                .enumerate()
                .map(|(a, b)| {
                    let param = format!("${a}");
                    return Ast::Assign(param, Box::new(b));
                })
                .collect(),
        }));
    }
}
