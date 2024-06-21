use std::{marker::PhantomData, mem::take};

use crate::{
    dom::{self, dominates},
    maxssa::MaxSSA,
    util::{DropGuest, Push},
    BlockTarget, Func, SaneTerminator, Use,
};

pub fn droppify<
    O: Push<DropGuest>,
    T: Default + SaneTerminator<O, T, Y, S> + Push<BlockTarget<O, T, Y, S>>,
    Y: Clone + Default,
    S: Clone + Default,
>(
    f: &mut Func<O, T, Y, S>,
) where
    Func<O, T, Y, S>: MaxSSA,
{
    f.maxssa();
    let cfg = f.domap_ref();
    let d = f.def_blocks();
    for k in f.blocks.iter().map(|a| a.0).collect::<Vec<_>>() {
        let mut t = take(&mut f.blocks[k].term);
        for r in t.t2s_mut() {
            let shim = f.blocks.alloc(Default::default());
            let ps = r
                .prepend
                .iter()
                .map(|a| f.add_blockparam(shim, a.clone()))
                .map(|a| crate::Use {
                    value: a,
                    select: S::default(),
                })
                .collect::<Vec<_>>();
            f.blocks[shim].term = T::push(BlockTarget {
                block: r.block,
                args: ps.into_iter().chain(r.args.iter().cloned()).collect(),
                prepend: vec![],
            })
            .map_right(|_| ())
            .unwrap_left();
            for i in f.blocks[k]
                .insts
                .iter()
                .map(|a| *a)
                .filter(|a| r.args.iter().find(|r| r.value == *a).is_none())
                .collect::<Vec<_>>()
            {
                let j = f.opts.alloc(crate::Value::Operator(
                    O::push(DropGuest {}).map_right(|_| ()).unwrap_left(),
                    vec![Use {
                        value: i,
                        select: S::default(),
                    }],
                    Y::default(),
                    PhantomData,
                ));
                f.blocks[shim].insts.push(j);
            }
            r.block = shim;
            r.args = vec![];
        }
        f.blocks[k].term = t;
    }
}
