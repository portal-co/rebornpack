use std::{collections::BTreeMap, iter::once};

use id_arena::{Arena, Id};

use crate::{
    detect::{AddCmul, Clear, Detect},
    Block, Scope, Var, I,
};
pub mod rat;
#[derive(Clone)]
pub struct SSAScope {
    pub wrapped: Scope,
    pub ssavals: Arena<SSAVal>,
    pub blocks: Arena<SSABlock>,
    pub in_transcache: BTreeMap<Id<Block>, Id<SSABlock>>,
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Param {
    Var(Id<Var>),
    JustDec,
}

#[derive(Clone)]
pub enum SSAVal {
    I(I),
    P(Param),
    Add(Vec<Id<SSAVal>>),
    KMul(Id<SSAVal>, I),
    HyCall(String, Vec<Id<SSAVal>>),
    Select(Id<SSAVal>, usize),
}
#[derive(Clone)]
pub struct SSATarget {
    pub block: Id<SSABlock>,
    pub params: BTreeMap<Param, Id<SSAVal>>,
}

#[derive(Clone)]
pub enum SSABranch {
    Just(SSATarget),
    If(Id<SSAVal>, SSATarget, SSATarget),
    Halt,
}
#[derive(Clone)]
pub struct SSABlock {
    pub vals: Vec<Id<SSAVal>>,
    pub branch: SSABranch,
}
impl SSAScope {
    pub fn all_params<'a>(&'a self) -> impl Iterator<Item = Param> + 'a {
        return once(Param::JustDec).chain(self.wrapped.vars.iter().map(|a| Param::Var(a.0)));
    }
    pub fn fuse(&mut self, t: Id<SSABlock>, v: Param) -> Id<SSABlock> {
        let mut sk = SSABlock {
            vals: vec![],
            branch: SSABranch::Halt,
        };
        // let st = self.blocks.alloc(sk.clone());
        // self.in_transcache.insert(k, st);
        let mut r = BTreeMap::new();
        for i in self.wrapped.vars.iter().map(|a| a.0).collect::<Vec<_>>() {
            let v = self.ssavals.alloc(SSAVal::P(Param::Var(i)));
            r.insert(Param::Var(i), v);
            sk.vals.push(v);
        }
        let w = self.ssavals.alloc(SSAVal::P(Param::JustDec));
        sk.vals.push(w);
        r.insert(v, w);
        sk.branch = SSABranch::Just(SSATarget {
            block: t,
            params: r,
        });
        return self.blocks.alloc(sk);
    }
    pub fn blockify(&mut self, mut k: Id<Block>) -> Id<SSABlock> {
        if let Some(b) = self.in_transcache.get(&k) {
            return *b;
        }
        let mut sk = SSABlock {
            vals: vec![],
            branch: SSABranch::Halt,
        };
        let st = self.blocks.alloc(sk.clone());
        self.in_transcache.insert(k, st);
        let mut r = BTreeMap::new();
        for i in self.wrapped.vars.iter().map(|a| a.0).collect::<Vec<_>>() {
            let v = self.ssavals.alloc(SSAVal::P(Param::Var(i)));
            r.insert(Param::Var(i), v);
            sk.vals.push(v);
        }
        loop {
            if let Some((a, b)) = Clear::detect(&self.wrapped, k) {
                let v = self.ssavals.alloc(SSAVal::I(0u8.into()));
                r.insert(Param::Var(a.x), v);
                k = b;
                continue;
            }
            if let Some((a, b)) = AddCmul::detect(&self.wrapped, k) {
                let v = self.ssavals.alloc(SSAVal::KMul(
                    *r.get(&Param::Var(a.from)).unwrap(),
                    a.ko.clone(),
                ));
                sk.vals.push(v);
                r.insert(Param::Var(a.to), v);
                let v = self.ssavals.alloc(SSAVal::Add(vec![
                    *r.get(&Param::Var(a.from)).unwrap(),
                    *r.get(&Param::Var(a.tmp)).unwrap(),
                ]));
                sk.vals.push(v);
                r.insert(Param::Var(a.from), v);
                let v = self.ssavals.alloc(SSAVal::I(0u8.into()));
                r.insert(Param::Var(a.tmp), v);
                k = b;
                continue;
            }
            match self.wrapped.blocks[k].clone() {
                Block::Br(i, j) => {
                    for (a, b) in i {
                        let a = Param::Var(a);
                        let v = self.ssavals.alloc(SSAVal::I(b));
                        sk.vals.push(v);
                        let v = self
                            .ssavals
                            .alloc(SSAVal::Add(vec![*r.get(&a).unwrap(), v]));
                        sk.vals.push(v);
                        r.insert(a, v);
                    }
                    k = j;
                }
                Block::BrDec(v, b, c) => {
                    let b = self.blockify(b);
                    let c = self.blockify(c);
                    let b = self.fuse(b, Param::Var(v.clone()));
                    let w = *r.get(&Param::Var(v)).unwrap();
                    sk.branch = SSABranch::If(
                        w,
                        SSATarget {
                            block: b,
                            params: r.clone(),
                        },
                        SSATarget {
                            block: c,
                            params: r,
                        },
                    );
                    break;
                }
                Block::HyCall(h, t, nb) => {
                    let mut m = t.iter().map(|t| r.get(&Param::Var(*t)).unwrap()).cloned();
                    let n = self.ssavals.alloc(SSAVal::HyCall(h, m.clone().collect()));
                    sk.vals.push(n);
                    for (o, i) in t.iter().enumerate() {
                        let v = self.ssavals.alloc(SSAVal::Select(n, o));
                        sk.vals.push(v);
                        r.insert(Param::Var(*i), v);
                    }
                    k = nb;
                    // let nb = self.blockify(nb);
                    // sk.branch = SSABranch::Just(SSATarget { block: nb, params: r.clone() })
                }
                Block::Todo => break,
            }
        }
        self.blocks[st] = sk;
        return st;
    }
}
