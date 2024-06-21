use std::{collections::BTreeMap, marker::PhantomData};

use id_arena::Id;
use rat_ir::{
    util::{If, Push},
    Block, BlockTarget, Use, Value,
};

use crate::I;

use super::{SSABlock, SSAScope, SSATarget, SSAVal};
#[derive(Clone, Debug)]
pub enum SpecialOp {
    Add,
    KMul(I),
}

pub fn ratify<
    O: Push<I> + Push<String> + Push<SpecialOp>,
    T: Default + Push<BlockTarget<O, T, Y, S>> + Push<If<O, T, Y, S, BlockTarget<O, T, Y, S>>>,
    Y: Default + Clone,
    S: Default + Push<usize>,
>(
    s: &SSAScope,
    r: &mut rat_ir::Func<O, T, Y, S>,
) -> (
    BTreeMap<Id<SSAVal>, Id<Value<O, T, Y, S>>>,
    BTreeMap<Id<SSABlock>, Id<Block<O, T, Y, S>>>,
) {
    let ks = s
        .blocks
        .iter()
        .map(|a| (a.0, r.blocks.alloc(Default::default())))
        .collect::<BTreeMap<_, _>>();
    let is = s.all_params().collect::<Vec<_>>();
    let ps = ks
        .iter()
        .map(|a| {
            (
                a.0.clone(),
                is.iter()
                    .map(|p| (p.clone(), r.add_blockparam(a.1.clone(), Y::default())))
                    .collect::<BTreeMap<_, _>>(),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let mut vs = s
        .ssavals
        .iter()
        .map(|(a, _)| {
            (
                a,
                r.opts.alloc(rat_ir::Value::Operator(
                    O::push(I::new(vec![])).unwrap_left(),
                    vec![],
                    Y::default(),
                    PhantomData,
                )),
            )
        })
        .collect::<BTreeMap<_, _>>();
    for (i, x) in s.ssavals.iter() {
        let (b, k) = s.blocks.iter().find(|a| a.1.vals.contains(&i)).unwrap();
        let rb = *ks.get(&b).unwrap();
        let v = match x {
            super::SSAVal::I(i) => rat_ir::Value::Operator(
                O::push(i.clone()).unwrap_left(),
                vec![],
                Y::default(),
                PhantomData,
            ),
            super::SSAVal::P(p) => rat_ir::Value::Alias(
                Use {
                    value: *ps.get(&b).unwrap().get(p).unwrap(),
                    select: S::default(),
                },
                Y::default(),
            ),
            super::SSAVal::Add(i) => rat_ir::Value::Operator(
                O::push(SpecialOp::Add).unwrap_left(),
                i.iter()
                    .map(|x| vs.get(x).copied().unwrap())
                    .map(|a| Use {
                        value: a,
                        select: S::default(),
                    })
                    .collect(),
                Y::default(),
                PhantomData,
            ),
            super::SSAVal::KMul(a, i) => rat_ir::Value::Operator(
                O::push(SpecialOp::KMul(i.clone())).unwrap_left(),
                vec![Use {
                    value: vs.get(a).copied().unwrap(),
                    select: S::default(),
                }],
                Y::default(),
                PhantomData,
            ),
            super::SSAVal::HyCall(h, i) => rat_ir::Value::Operator(
                O::push(h.clone()).unwrap_left(),
                i.iter()
                    .map(|x| vs.get(x).copied().unwrap())
                    .map(|a| Use {
                        value: a,
                        select: S::default(),
                    })
                    .collect(),
                Y::default(),
                PhantomData,
            ),
            super::SSAVal::Select(a, u) => rat_ir::Value::Alias(
                Use {
                    value: vs.get(a).copied().unwrap(),
                    select: S::push(*u).unwrap_left(),
                },
                Y::default(),
            ),
        };
        r.opts[vs.get(&i).copied().unwrap()] = v;
        r.blocks[rb].insts.push(vs.get(&i).copied().unwrap());
        // vs.insert(i, rb);
    }
    for (a, b) in ks.iter() {
        let target = |t: &SSATarget, d: bool| {
            let mut ps = ps.get(&t.block).unwrap().clone();
            for (a, b) in t.params.iter() {
                ps.insert(a.clone(), vs.get(b).copied().unwrap());
            }
            BlockTarget {
                block: ks.get(&t.block).copied().unwrap(),
                args: is
                    .iter()
                    .skip(if d { 1 } else { 0 })
                    .map(|a| ps.get(a).unwrap().clone())
                    .map(|a| Use {
                        value: a,
                        select: S::default(),
                    })
                    .collect(),
                prepend: if d { vec![Y::default()] } else { vec![] },
            }
        };
        match &s.blocks[*a].branch {
            super::SSABranch::Just(a) => {
                r.blocks[*b].term = T::push(target(a, false)).map_right(|_| ()).unwrap_left();
            }
            super::SSABranch::If(a, b2, c) => {
                r.blocks[*b].term = T::push(If {
                    val: Use {
                        value: vs.get(a).copied().unwrap(),
                        select: S::default(),
                    },
                    then: target(b2, false),
                    r#else: Some(target(c, true)),
                })
                .map_right(|_| ())
                .unwrap_left()
            }
            super::SSABranch::Halt => {}
        }
    }
    return (vs, ks);
}
