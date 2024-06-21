use std::collections::BTreeMap;

use id_arena::Id;

use crate::{Block, Scope, Var, I};
pub trait Detect {
    fn detect(s: &Scope, k: Id<Block>) -> Option<(Box<Self>, Id<Block>)>;
}
pub struct Clear {
    pub x: Id<Var>, // `x` <- 0
}
impl Detect for Clear {
    fn detect(s: &Scope, k: Id<Block>) -> Option<(Box<Self>, Id<Block>)> {
        let k2 = s.blocks[k].clone();
        let Block::BrDec(x, k3, k4) = k2 else {
            return None;
        };
        if k3 != k {
            return None;
        }
        return Some((Box::new(Clear { x }), k4));
    }
}
pub struct Decr {
    pub x: Id<Var>, // `x`--
}
impl Detect for Decr {
    fn detect(s: &Scope, k: Id<Block>) -> Option<(Box<Self>, Id<Block>)> {
        let k2 = s.blocks[k].clone();
        let Block::BrDec(x, k3, k4) = k2 else {
            return None;
        };
        if k3 != k4 {
            return None;
        }
        return Some((Box::new(Decr { x }), k3));
    }
}
/// `to` <- `from` * `ko`
/// `from` += `tmp`
/// `tmp` <- 0
pub struct AddCmul {
    pub ko: I,
    pub from: Id<Var>,
    pub to: Id<Var>,
    pub tmp: Id<Var>,
}
impl Detect for AddCmul {
    fn detect(s: &Scope, k: Id<Block>) -> Option<(Box<Self>, Id<Block>)> {
        let k2 = s.blocks[k].clone();
        let Block::Br(mut i, k3) = k2 else {
            return None;
        };
        let k2 = s.blocks[k3].clone();
        let Block::BrDec(from, k4, k5) = k2 else {
            return None;
        };
        if k4 != k {
            return None;
        }
        let k2 = s.blocks[k5].clone();
        let Block::Br(i2, k3) = k2 else {
            return None;
        };
        let k2 = s.blocks[k3].clone();
        let Block::BrDec(tmp, k6, k7) = k2 else {
            return None;
        };
        if k6 != k5 {
            return None;
        }
        let im = i.remove(&tmp)?;
        if im != 1u32.into() {
            return None;
        }
        if i.len() != 1 {
            return None;
        }
        let (to, ko) = i.into_iter().next()?;
        let mut c = BTreeMap::new();
        c.insert(from, 1u32.into());
        if i2 != c {
            return None;
        }
        return Some((Box::new(AddCmul { ko, from, to, tmp }), k7));
    }
}
/// {targets.0} += `clear` * {targets.1}
/// `clear` <- 0
pub struct Divert{
    pub clear: Id<Var>,
    pub targets: BTreeMap<Id<Var>,u64>
}