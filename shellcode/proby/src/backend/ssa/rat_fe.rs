use std::{borrow::Cow, collections::BTreeMap, iter::once, marker::PhantomData};

use id_arena::Id;
use rat_ir::{
    no_push, util::{BinOp, If, PerID, Push}, BlockTarget, Use
};

use crate::backend::{CondType, Plat};

use super::{
    rat::{Call, Fptr},
    Target,
};
#[derive(Clone)]
#[repr(transparent)]
pub struct Data(pub Cow<'static,[u8]>);
no_push!(type Data;);
pub fn copy_block<
    C,
    O: Push<BinOp> + Push<Fptr<String>> + Push<Call> + Push<u64> + Push<CondType> + Push<Plat> + Push<Data>,
    T: Push<BlockTarget<O, T, Y, S>> + Push<If<O, T, Y, S, BlockTarget<O, T, Y, S>>>,
    Y: Default + Clone,
    S: Default,
>(
    k: Id<super::Block>,
    m: &PerID<super::Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>,
    old: &super::SSAFunc,
    new: &mut rat_ir::Func<O, T, Y, S>,
    ctx: &mut C,
) -> anyhow::Result<()> {
    let mut tk = *m[k].as_ref().unwrap();
    let params = (0..(old.blocks[k].params()))
        .map(|_| new.add_blockparam(tk, Y::default()))
        .collect::<Vec<_>>();
    let mut valmap = BTreeMap::new();
    for (a, b) in old.blocks[k].insts.iter() {
        valmap.insert(
            a.clone(),
            match b {
                super::Instr::Bin(o, a, b) => {
                    let a = valmap.get(a).cloned().unwrap();
                    let b = valmap.get(b).cloned().unwrap();
                    let o = O::push(o.clone()).unwrap_left();
                    let v = new.opts.alloc(rat_ir::Value::Operator(
                        o,
                        vec![
                            Use {
                                value: a,
                                select: S::default(),
                            },
                            Use {
                                value: b,
                                select: S::default(),
                            },
                        ],
                        Y::default(),
                        PhantomData,
                    ));
                    new.blocks[tk].insts.push(v);
                    v
                }
                super::Instr::Addrof(a) => {
                    let o = O::push(Fptr { fptr: a.clone() })
                        .map_right(|_| ())
                        .unwrap_left();
                    let v = new.opts.alloc(rat_ir::Value::Operator(
                        o,
                        vec![],
                        Y::default(),
                        PhantomData,
                    ));
                    new.blocks[tk].insts.push(v);
                    v
                }
                super::Instr::Call(a, b) => {
                    let r = once(a)
                        .chain(b.iter())
                        .filter_map(|x| valmap.get(x))
                        .copied()
                        .map(|a| Use {
                            value: a,
                            select: S::default(),
                        })
                        .collect::<Vec<_>>();
                    let o = O::push(Call {}).map_right(|_| ()).unwrap_left();
                    let v =
                        new.opts
                            .alloc(rat_ir::Value::Operator(o, r, Y::default(), PhantomData));
                    new.blocks[tk].insts.push(v);
                    v
                }
                super::Instr::Param(p) => params[*p],
                super::Instr::Const(k) => {
                    let o = O::push(*k).map_right(|_| ()).unwrap_left();
                    let v = new.opts.alloc(rat_ir::Value::Operator(
                        o,
                        vec![],
                        Y::default(),
                        PhantomData,
                    ));
                    new.blocks[tk].insts.push(v);
                    v
                }
                super::Instr::Load(_, _) => todo!(),
                super::Instr::Store(_, _, _) => todo!(),
                super::Instr::Alloca(_) => todo!(),
                super::Instr::Plat(p, b) => {
                    let r = b
                        .iter()
                        .filter_map(|x| valmap.get(x))
                        .copied()
                        .map(|a| Use {
                            value: a,
                            select: S::default(),
                        })
                        .collect::<Vec<_>>();
                    let o = O::push(p.clone()).map_right(|_| ()).unwrap_left();
                    let v =
                        new.opts
                            .alloc(rat_ir::Value::Operator(o, r, Y::default(), PhantomData));
                    new.blocks[tk].insts.push(v);
                    v
                }
                super::Instr::Data(d) => {
                    let o = O::push(Data(d.clone())).map_right(|_| ()).unwrap_left();
                    let v = new.opts.alloc(rat_ir::Value::Operator(
                        o,
                        vec![],
                        Y::default(),
                        PhantomData,
                    ));
                    new.blocks[tk].insts.push(v);
                    v
                },
                super::Instr::OS => todo!(),
            },
        );
    }
    let target = |x: &Target| BlockTarget {
        block: m[x.id].as_ref().unwrap().clone(),
        args: x
            .params
            .iter()
            .filter_map(|x| valmap.get(x))
            .map(|a| Use {
                value: *a,
                select: S::default(),
            })
            .collect(),
        prepend: vec![],
    };
    match &old.blocks[k].term {
        super::Term::ReturnCall(_, _) => todo!(),
        super::Term::Br(t) => {
            new.blocks[tk].term = T::push(target(t)).map_right(|_| ()).unwrap_left();
        }
        super::Term::BrIf {
            cond,
            cty,
            if_true,
            if_false,
        } => {
            let cond = *valmap.get(cond).unwrap();
            let o = O::push(cty.clone()).map_right(|_| ()).unwrap_left();
            let v = new.opts.alloc(rat_ir::Value::Operator(
                o,
                vec![Use {
                    value: cond,
                    select: S::default(),
                }],
                Y::default(),
                PhantomData,
            ));
            new.blocks[tk].insts.push(v);
            new.blocks[tk].term = T::push(If {
                val: Use {
                    value: v,
                    select: S::default(),
                },
                then: target(if_true),
                r#else: Some(target(if_false)),
            })
            .map_right(|_| ())
            .unwrap_left();
        }
        super::Term::Ret(_) => todo!(),
        super::Term::Null => {}
    }
    return Ok(());
}
pub fn export_func<
    C,
    O: Push<BinOp> + Push<Fptr<String>> + Push<Call> + Push<u64> + Push<CondType> + Push<Plat> +Push<Data>,
    T: Default + Push<BlockTarget<O, T, Y, S>> + Push<If<O, T, Y, S, BlockTarget<O, T, Y, S>>>,
    Y: Default + Clone,
    S: Default,
>(
    i: &mut rat_ir::Func<O, T, Y, S>,
    d: &super::SSAFunc,
    ctx: &mut C,
) -> anyhow::Result<PerID<super::Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>> {
    let mut m = PerID::default();
    for k in d.blocks.iter().map(|a| a.0) {
        m[k] = Some(i.blocks.alloc(Default::default()));
    }
    for k in d.blocks.iter().map(|a| a.0) {
        copy_block(k, &m, d, i, ctx)?;
    }
    return Ok(m);
}
