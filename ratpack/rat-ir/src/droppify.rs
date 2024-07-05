use std::{marker::PhantomData, mem::take};

use either::Either;

use crate::{
    dom::{self, dominates}, maxssa::MaxSSA, util::Push, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator, Use
};
pub trait DropGuest<C, Y>: Sized {
    fn drg(ctx: &mut C, y: &Y) -> anyhow::Result<Self>;
}
impl<C,Y,A: DropGuest<C,Y>,B: DropGuest<C,Y>> DropGuest<C,Y> for Either<A,B>{
    fn drg(ctx: &mut C, y: &Y) -> anyhow::Result<Self> {
        let ae = match A::drg(ctx, y){
            Ok(a) => return Ok(Either::Left(a)),
            Err(e) => e,
        };
        let be = match B::drg(ctx, y){
            Ok(a) => return Ok(Either::Right(a)),
            Err(e) => e,
        };
        return Err(anyhow::anyhow!("{ae}; {be}"));
    }
}
impl<C,B: Bound> DropGuest<C,BoundType<B>> for BoundOp<B> where B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: DropGuest<C,B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>{
    fn drg(ctx: &mut C, y: &BoundType<B>) -> anyhow::Result<Self> {
        Ok(BoundOp(B::O::<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>::drg(ctx, &y.0)?))
    }
}
pub fn droppify<
    C,
    O: DropGuest<C, Y>,
    T: Default + SaneTerminator<O, T, Y, S> + Push<BlockTarget<O, T, Y, S>>,
    Y: Clone + Default,
    S: Clone + Default,
>(
    f: &mut Func<O, T, Y, S>,
    ctx: &mut C,
) -> anyhow::Result<()>
where
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
                    O::drg(ctx, f.opts[i].ty())?,
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
    Ok(())
}
