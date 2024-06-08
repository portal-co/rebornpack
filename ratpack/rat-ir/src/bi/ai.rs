use std::marker::PhantomData;

use anyhow::Context;
use either::Either;
use id_arena::Id;

use crate::{
    util::Extract, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator, Use, Value
};

use super::{NormalTermBi, Tracer};
pub trait Infer<D, R, Y> {
    fn infer(&self, ctx: &mut D, input: &Y) -> anyhow::Result<(Y, R)>;
}
impl<D,R,Y,A: Infer<D,R,Y>,B: Infer<D,R,Y>> Infer<D,R,Y> for Either<A,B>{
    fn infer(&self, ctx: &mut D, input: &Y) -> anyhow::Result<(Y, R)> {
        match self{
            Either::Left(a) => a.infer(ctx, input),
            Either::Right(b) => b.infer(ctx, input),
        }
    }
}
impl<D, B: Bound, C: Bound> Infer<D, BoundSelect<C>, BoundType<C>> for BoundSelect<B>
where
    B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Infer<
        D,
        C::S<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
        C::Y<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
    >,
{
    fn infer(
        &self,
        ctx: &mut D,
        input: &BoundType<C>,
    ) -> anyhow::Result<(BoundType<C>, BoundSelect<C>)> {
        let (a, b) = self.0.infer(ctx, &input.0)?;
        Ok((BoundType(a), BoundSelect(b)))
    }
}
impl<D, B: Bound, C: Bound> InferOp<D, BoundOp<C>, BoundType<C>> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: InferOp<
        D,
        C::O<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
        C::Y<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
    >,
{
    fn infer<'a>(
        &self,
        ctx: &mut D,
        input: impl Iterator<Item = &'a BoundType<C>>,
    ) -> anyhow::Result<(BoundType<C>, BoundOp<C>)>
    where
        BoundType<C>: 'a,
    {
        let (a, b) = self.0.infer(ctx, input.map(|a| &a.0))?;
        Ok((BoundType(a), BoundOp(b)))
    }
}
pub trait InferOp<D, R, Y> {
    fn infer<'a>(&self, ctx: &mut D, input: impl Iterator<Item = &'a Y>) -> anyhow::Result<(Y, R)>
    where
        Y: 'a;
}
impl<D,R,Y,A: InferOp<D,R,Y>,B: InferOp<D,R,Y>> InferOp<D,R,Y> for Either<A,B>{
    fn infer<'a>(&self, ctx: &mut D, input: impl Iterator<Item = &'a Y>) -> anyhow::Result<(Y, R)>
    where
        Y: 'a {
        match self{
            Either::Left(a) => a.infer(ctx, input),
            Either::Right(b) => b.infer(ctx, input),
        }
    }
}
pub struct AI<'a, D> {
    pub ctx: &'a mut D,
}
impl<
        'a,
        D,
        O: InferOp<D, O2, Y2>,
        T: SaneTerminator<O, T, Y, S> + NormalTermBi<O, T, Y, S, O2, T2, Y2, S2, Then = T2>,
        Y,
        S: Infer<D, S2, Y2>,
        O2,
        T2,
        Y2: Clone + Ord,
        S2: Default,
    > Tracer<O, T, Y, S, O2, T2, Y2, S2> for AI<'a, D>
{
    type Instance = Vec<Y2>;

    type Meta = Id<Value<O2, T2, Y2, S2>>;

    fn meta_param(
        &mut self,
        i: &Self::Instance,
        idx: usize,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        Ok((new.add_blockparam(k, i[idx].clone()), k))
    }

    fn select(
        &mut self,
        i: &Self::Instance,
        m: &Self::Meta,
        s: &S,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        let (i, e) = s.infer(&mut self.ctx, new.opts[*m].ty())?;
        // let e = s.extract();
        let v = new.opts.alloc(Value::Alias(
            Use {
                value: *m,
                select: e,
            },
            i,
        ));
        new.blocks[k].insts.push(v);
        return Ok((v, k));
    }

    fn op(
        &mut self,
        i: &Self::Instance,
        o: &O,
        y: &Y,
        args: &[Self::Meta],
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        let (i, p) = o.infer(&mut self.ctx, args.iter().map(|x| new.opts[*x].ty()))?;
        // let p = o.extract();
        let v = new.opts.alloc(Value::Operator(
            p,
            args.iter()
                .map(|a| Use {
                    value: *a,
                    select: S2::default(),
                })
                .collect(),
            i,
            PhantomData,
        ));
        new.blocks[k].insts.push(v);
        return Ok((v, k));
    }

    fn term(
        state: &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>,
        i: &Self::Instance,
        t: &T,
        mut go: impl FnMut(
            &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>,
            &mut crate::Func<O2, T2, Y2, S2>,
            id_arena::Id<crate::Block<O, T, Y, S>>,
            Self::Instance,
        ) -> anyhow::Result<id_arena::Id<crate::Block<O2, T2, Y2, S2>>>,
        valmap: &std::collections::BTreeMap<
            id_arena::Id<crate::Value<O, T, Y, S>>,
            std::sync::Arc<Self::Meta>,
        >,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        mut k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
        old: &Func<O,T,Y,S>,
    ) -> anyhow::Result<()> {
        let t2 = t.norm(
            |a| {
                let v = a
                    .args
                    .iter()
                    .map(|u| {
                        let a = &**valmap.get(&u.value).context("in getting the value")?;
                        let (s, b) = Tracer::<O, T, Y, S, O2, T2, Y2, S2>::select(
                            &mut state.tracer,
                            i,
                            a,
                            &u.select,
                            old.opts[u.value].ty(),
                            new,
                            k,
                        )?;
                        k = b;
                        Ok(s)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let v = v.iter().map(|w| new.opts[*w].ty().clone()).collect();
                let g = go(state, new, a.block, v)?;
                let r = a
                    .args
                    .iter()
                    .map(|a| {
                        let v = **valmap.get(&a.value).unwrap();
                        Ok(Use {
                            value: v,
                            select: a.select.infer(&mut state.tracer.ctx, new.opts[v].ty())?.1,
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                Ok(crate::BlockTarget {
                    block: g,
                    args: r,
                    prepend: vec![],
                })
            },
            |v| Ok(**valmap.get(&v).unwrap()),
        )?;
        new.blocks[k].term = t2;
        Ok(())
    }
}
