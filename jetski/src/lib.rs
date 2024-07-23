use std::collections::BTreeMap;

use id_arena::{Arena, Id};
use relooper::{BranchMode, MultipleBlock, ShapedBlock, SimpleBlock};
pub mod bopt;
pub mod detect;
pub mod ssa;
pub type I = num_bigint::BigUint;
pub fn clean(m: &mut BTreeMap<Id<Var>, I>) {
    for (k, v) in m.clone().into_iter() {
        if v == 0u32.into() {
            m.remove(&k);
        }
    }
}
#[derive(Clone)]
pub struct Var {
    pub initial: I,
}
#[derive(Clone)]
pub struct Scope {
    pub blocks: Arena<Block>,
    pub vars: Arena<Var>,
}
#[derive(Clone)]
pub enum Block {
    Br(BTreeMap<Id<Var>, I>, Id<Block>),
    BrDec(Id<Var>, Id<Block>, Id<Block>),
    HyCall(String, Vec<Id<Var>>, Id<Block>),
    Todo,
}
impl Block {
    pub fn modify_targets(&mut self, mut a: impl FnMut(&mut Id<Block>)) {
        match self {
            Block::Br(_, n) => a(n),
            Block::BrDec(_, nt, nf) => {
                a(nt);
                a(nf);
            }
            Block::HyCall(_, _, b) => a(b),
            Block::Todo => {}
        }
    }
    pub fn cff(&self, idx: Id<Var>) -> PMMNCmds {
        match self {
            Block::Br(a, i) => {
                let mut b = a.clone();
                b.insert(idx, I::from(i.index()) + I::from(1usize));
                return PMMNCmds(vec![PMMNCmd::Inc(b)]);
            }
            Block::BrDec(a, t, e) => {
                let mut ba = BTreeMap::new();
                let mut bb = BTreeMap::new();
                ba.insert(idx, I::from(t.index()) + I::from(1usize));
                bb.insert(idx, I::from(e.index()) + I::from(1usize));
                return PMMNCmds(vec![PMMNCmd::If(
                    PMMNTest::Dec(*a),
                    PMMNCmds(vec![PMMNCmd::Inc(ba)]),
                    PMMNCmds(vec![PMMNCmd::Inc(bb)]),
                )]);
            }
            Block::Todo => PMMNCmds(vec![]),
            Block::HyCall(a, v, i) => {
                let mut b = BTreeMap::new();
                b.insert(idx, I::from(i.index()) + I::from(1usize));
                return PMMNCmds(vec![PMMNCmd::HyCall(a.clone(), v.clone()), PMMNCmd::Inc(b)]);
            }
        }
    }
}
impl Scope {
    pub fn add_cmul(&mut self, from: Id<Var>, to: Id<Var>, k: I) -> PMMNCmds {
        let tmp = self.vars.alloc(Var {
            initial: I::from(0usize),
        });
        let mut x = vec![];
        let a = vec![PMMNCmd::Inc(
            vec![(tmp, I::from(1usize)), (to, k)].into_iter().collect(),
        )];
        x.push(PMMNCmd::While(
            PMMNTest::Dec(from),
            format!("tmp_add_cmul"),
            PMMNCmds(a),
        ));
        let a = vec![PMMNCmd::Inc(
            vec![(from, I::from(1usize))].into_iter().collect(),
        )];
        x.push(PMMNCmd::While(
            PMMNTest::Dec(tmp),
            format!("tmp_add_cmul"),
            PMMNCmds(a),
        ));
        return PMMNCmds(x);
    }
    pub fn traverse(&self, b: &mut BTreeMap<Id<Var>, I>, k: &mut Id<Block>) -> bool {
        let mut did = false;
        while let Block::Br(a, c) = &self.blocks[*k] {
            for (d, e) in a.iter() {
                *b.entry(*d).or_insert(I::from(0usize)) *= e.clone();
            }
            *k = *c;
            did = true;
        }
        return did;
    }
    pub fn traversal_pass(&mut self) -> bool {
        let mut did = false;
        for (i, mut b) in self
            .blocks
            .iter()
            .map(|(a, b)| (a, b.clone()))
            .collect::<Vec<_>>()
        {
            if let Block::Br(b, k) = &mut b {
                did = did || self.traverse(b, k);
            }
            self.blocks[i] = b;
        }
        return did;
    }
    pub fn is_alias(&self, b: Id<Block>) -> Option<Id<Block>> {
        let b2 = self.blocks[b].clone();
        if let Block::Br(i, t) = b2.clone() {
            if i == BTreeMap::new() {
                return Some(t);
            }
        }
        return None;
    }
    pub fn alias_opt(&mut self) -> bool {
        let mut did = false;
        for (i, mut b) in self
            .blocks
            .iter()
            .map(|(a, b)| (a, b.clone()))
            .collect::<Vec<_>>()
        {
            // let mut b = b.clone();
            b.modify_targets(|i| {
                while let Some(j) = self.is_alias(*i) {
                    *i = j;
                    did = true;
                }
            });
            self.blocks[i] = b;
        }
        return did;
    }
    pub fn reloop(&self, e: Id<Block>) -> ShapedBlock<Id<Block>> {
        let mut m = BTreeMap::new();
        for (i, j) in self.blocks.iter() {
            m.insert(
                i,
                match j {
                    Block::Br(_, a) => vec![*a],
                    Block::BrDec(_, b, c) => vec![*b, *c],
                    Block::Todo => vec![],
                    Block::HyCall(_, _, a) => vec![*a],
                },
            );
        }
        return *relooper::reloop(m.into_iter().collect(), e);
    }
    pub fn emit_relooped(&mut self, e: Id<Block>, collect: &mut PMMNCmds) {
        let r = self.reloop(e);
        let v = self.vars.alloc(Var {
            initial: I::from(0u8),
        });
        self.emit_reloop(&r, v, collect);
    }
    pub fn emit_br(&self, l: BranchMode, idx: Id<Var>, collect: &mut PMMNCmds, b: Id<Block>) {
        match l {
            BranchMode::LoopBreak(l) => collect.0.push(PMMNCmd::Break(format!("loop{l}"))),
            BranchMode::LoopBreakIntoMulti(l) => {
                let mut m = BTreeMap::new();
                m.insert(idx, b.index().into());
                collect.0.push(PMMNCmd::Inc(m));
                collect.0.push(PMMNCmd::Break(format!("loop{l}")))
            }
            BranchMode::LoopContinue(l) => collect.0.push(PMMNCmd::Continue(format!("loop{l}"))),
            BranchMode::LoopContinueIntoMulti(l) => {
                let mut m = BTreeMap::new();
                m.insert(idx, b.index().into());
                collect.0.push(PMMNCmd::Inc(m));
                collect.0.push(PMMNCmd::Continue(format!("loop{l}")))
            }
            BranchMode::MergedBranch => {}
            BranchMode::MergedBranchIntoMulti => {
                let mut m = BTreeMap::new();
                m.insert(idx, b.index().into());
                collect.0.push(PMMNCmd::Inc(m));
            }
            BranchMode::SetLabelAndBreak => {
                let mut m = BTreeMap::new();
                m.insert(idx, b.index().into());
                collect.0.push(PMMNCmd::Inc(m));
                collect.0.push(PMMNCmd::Break(format!("$cff")));
            }
        }
    }
    pub fn emit_target(
        &self,
        s: &SimpleBlock<Id<Block>>,
        idx: Id<Var>,
        b: Id<Block>,
        collect: &mut PMMNCmds,
    ) {
        let l = s
            .branches
            .get(&b)
            .map(|a| a.clone())
            .unwrap_or(BranchMode::MergedBranch);
        if l == BranchMode::MergedBranch {
            if let Some(i) = &s.immediate {
                self.emit_reloop(&*i, idx, collect);
            }
            if let Some(i) = &s.next {
                self.emit_reloop(&*i, idx, collect);
            }
            return;
        }
        self.emit_br(l, idx, collect, b);
    }
    pub fn gather(
        &self,
        k: &MultipleBlock<Id<Block>>,
        idx: Id<Var>,
        m: &mut BTreeMap<Id<Block>, PMMNCmds>,
        s: usize,
    ) {
        for (i, h) in k.handled.iter().skip(s).enumerate() {
            // let h = h.clone();
            let mut x = PMMNCmds(vec![]);
            self.emit_reloop(&h.inner, idx, &mut x);
            if !h.break_after {
                self.gather(k, idx, m, i + 1);
                if let Some(a) = k.handled.get(i + 1) {
                    let mut n = m.get(&a.labels[0]).unwrap().clone();
                    x.0.append(&mut n.0);
                }
            }
            for l in &h.labels {
                m.insert(l.clone(), x.clone());
            }
        }
    }
    pub fn emit_reloop(&self, s: &ShapedBlock<Id<Block>>, idx: Id<Var>, collect: &mut PMMNCmds) {
        match s {
            ShapedBlock::Simple(s) => match self.blocks[s.label].clone() {
                Block::Br(a, b) => {
                    collect.0.push(PMMNCmd::Inc(a));
                    self.emit_target(s, idx, b, collect);
                }
                Block::BrDec(a, b, c) => {
                    let mut ax = PMMNCmds(vec![]);
                    let mut bx = PMMNCmds(vec![]);
                    self.emit_target(s, idx, b, &mut ax);
                    self.emit_target(s, idx, c, &mut bx);
                    collect.0.push(PMMNCmd::If(PMMNTest::Dec(a), ax, bx))
                }
                Block::Todo => todo!(),
                Block::HyCall(a, v, b) => {
                    collect.0.push(PMMNCmd::HyCall(a, v));
                    self.emit_target(s, idx, b, collect);
                }
            },
            ShapedBlock::Loop(l) => {
                let mut n = PMMNCmds(vec![]);
                self.emit_reloop(&*l.inner, idx, &mut n);
                collect.0.push(PMMNCmd::While(
                    PMMNTest::True,
                    format!("loop{}", l.loop_id),
                    n,
                ));
                if let Some(i) = &l.next {
                    self.emit_reloop(&*i, idx, collect);
                }
            }
            ShapedBlock::Multiple(k) => {
                let mut m = BTreeMap::new();
                self.gather(k, idx, &mut m, 0);
                let mut v = PMMNCmds(vec![]);
                for b in self.blocks.iter().rev() {
                    v = PMMNCmds(vec![PMMNCmd::If(
                        PMMNTest::Dec(idx),
                        v,
                        m.get(&b.0).map(|a| a.clone()).unwrap_or(PMMNCmds(vec![])),
                    )])
                }
                v.0.push(PMMNCmd::Break("$cff".to_owned()));
                collect
                    .0
                    .push(PMMNCmd::While(PMMNTest::Dec(idx), "$cff".to_owned(), v));
            }
        }
    }
    pub fn cff(&mut self, e: Id<Block>) -> PMMNCmds {
        let idx = self.vars.alloc(Var {
            initial: I::from(e.index()) + I::from(1u8),
        });
        let mut v = PMMNCmds(vec![]);
        for b in self.blocks.iter().rev() {
            v = PMMNCmds(vec![PMMNCmd::If(PMMNTest::Dec(idx), v, b.1.cff(idx))])
        }
        return PMMNCmds(vec![PMMNCmd::While(PMMNTest::Dec(idx), "$".to_owned(), v)]);
    }
}
#[derive(Clone, PartialEq, PartialOrd, Ord, Eq)]
pub enum PMMNTest {
    Dec(Id<Var>),
    True,
}
#[derive(Clone, PartialEq, PartialOrd, Ord, Eq)]
pub enum PMMNCmd {
    Test(PMMNTest),
    Inc(BTreeMap<Id<Var>, I>),
    If(PMMNTest, PMMNCmds, PMMNCmds),
    While(PMMNTest, String, PMMNCmds),
    Break(String),
    Continue(String),
    HyCall(String, Vec<Id<Var>>),
}
#[derive(Clone)]
pub struct LoopData {
    pub r#break: Id<Block>,
    pub r#continue: Id<Block>,
}
impl PMMNCmd {
    pub fn render(
        &self,
        s: &mut Scope,
        s2: &BTreeMap<String, LoopData>,
        r#in: Id<Block>,
    ) -> Id<Block> {
        let n = s.blocks.alloc(Block::Todo);
        match self {
            PMMNCmd::Test(t) => match t {
                PMMNTest::Dec(v) => s.blocks[r#in] = Block::BrDec(*v, n, n),
                PMMNTest::True => s.blocks[r#in] = Block::Br(BTreeMap::new(), n),
            },
            PMMNCmd::Inc(v) => {
                s.blocks[r#in] = Block::Br(v.clone(), n);
            }
            PMMNCmd::If(t, tr, fa) => {
                let ta = s.blocks.alloc(Block::Todo);
                let tb = s.blocks.alloc(Block::Todo);
                match t {
                    PMMNTest::Dec(v) => s.blocks[r#in] = Block::BrDec(*v, ta, tb),
                    PMMNTest::True => s.blocks[r#in] = Block::Br(BTreeMap::new(), ta),
                }
                let ta = tr.render(s, s2, ta);
                let tb = fa.render(s, s2, tb);
                s.blocks[ta] = Block::Br(BTreeMap::new(), n);
                s.blocks[tb] = Block::Br(BTreeMap::new(), n);
            }
            PMMNCmd::While(t, name, d) => {
                let l = s.blocks.alloc(Block::Todo);
                s.blocks[r#in] = match t {
                    PMMNTest::Dec(v) => Block::BrDec(*v, l, n),
                    PMMNTest::True => Block::Br(BTreeMap::new(), l),
                };
                let ol = l;
                let mut s3 = s2.clone();
                s3.insert(
                    name.clone(),
                    LoopData {
                        r#break: n,
                        r#continue: ol,
                    },
                );
                let l = d.render(s, &s3, l);
                s.blocks[l] = match t {
                    PMMNTest::Dec(v) => Block::BrDec(*v, ol, n),
                    PMMNTest::True => Block::Br(BTreeMap::new(), ol),
                }
            }
            PMMNCmd::Break(s3) => {
                s.blocks[r#in] = Block::Br(BTreeMap::new(), s2.get(s3).unwrap().r#break)
            }
            PMMNCmd::Continue(s3) => {
                s.blocks[r#in] = Block::Br(BTreeMap::new(), s2.get(s3).unwrap().r#continue)
            }
            PMMNCmd::HyCall(a, b) => s.blocks[r#in] = Block::HyCall(a.clone(), b.clone(), n),
        };
        n
    }
}
#[derive(Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct PMMNCmds(pub Vec<PMMNCmd>);
impl PMMNCmds {
    pub fn render(
        &self,
        s: &mut Scope,
        s2: &BTreeMap<String, LoopData>,
        mut r#in: Id<Block>,
    ) -> Id<Block> {
        for a in self.0.iter() {
            r#in = a.render(s, s2, r#in);
        }
        return r#in;
    }
}
#[cfg(test)]
mod tests {
    use super::*;
}
