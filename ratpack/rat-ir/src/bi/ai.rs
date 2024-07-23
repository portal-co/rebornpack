use std::{marker::PhantomData, sync::Arc};

use anyhow::Context;
use either::Either;
use id_arena::Id;
use rat_debug::Span;

use crate::{
    util::Extract, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func,
    SaneTerminator, Use, Value,
};

use super::{NormalTermBi, Tracer};
pub trait Infer<D, Y> {
    fn infer(&self, ctx: &mut D, input: &Y) -> anyhow::Result<(Y, Self::Result)>;
    type Result;
}
impl<D, Y, A: Infer<D, Y>, B: Infer<D, Y>> Infer<D, Y> for Either<A, B> {
    type Result = Either<A::Result, B::Result>;
    fn infer(&self, ctx: &mut D, input: &Y) -> anyhow::Result<(Y, Self::Result)> {
        match self {
            Either::Left(a) => a.infer(ctx, input).map(|(a, b)| (a, Either::Left(b))),
            Either::Right(b) => b.infer(ctx, input).map(|(a, b)| (a, Either::Right(b))),
        }
    }
}
impl<D, B: Bound, C: Bound> Infer<D, BoundType<C>> for BoundSelect<B>
where
    B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Infer<
        D,
        C::Y<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
        Result = C::S<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
    >,
{
    type Result = BoundSelect<C>;
    fn infer(
        &self,
        ctx: &mut D,
        input: &BoundType<C>,
    ) -> anyhow::Result<(BoundType<C>, BoundSelect<C>)> {
        let (a, b) = self.0.infer(ctx, &input.0)?;
        Ok((BoundType(a), BoundSelect(b)))
    }
}
impl<D, B: Bound, C: Bound> InferOp<D, BoundType<C>> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: InferOp<
        D,
        C::Y<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
        Result = C::O<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
    >,
{
    type Result = BoundOp<C>;
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
pub trait InferOp<D, Y> {
    type Result;
    fn infer<'a>(
        &self,
        ctx: &mut D,
        input: impl Iterator<Item = &'a Y>,
    ) -> anyhow::Result<(Y, Self::Result)>
    where
        Y: 'a;
}
impl<D, Y, A: InferOp<D, Y>, B: InferOp<D, Y>> InferOp<D, Y> for Either<A, B> {
    type Result = Either<A::Result, B::Result>;
    fn infer<'a>(
        &self,
        ctx: &mut D,
        input: impl Iterator<Item = &'a Y>,
    ) -> anyhow::Result<(Y, Self::Result)>
    where
        Y: 'a,
    {
        match self {
            Either::Left(a) => a.infer(ctx, input).map(|(a, b)| (a, Either::Left(b))),
            Either::Right(b) => b.infer(ctx, input).map(|(a, b)| (a, Either::Right(b))),
        }
    }
}
#[repr(transparent)]
pub struct AI<D, E, Y3, U> {
    pub ctx: D,
    pub phantom: PhantomData<fn(&Y3, &U)>,
    pub ph2: PhantomData<E>,
}

impl<D, E, Y3, U> AI<D, E, Y3, U> {
    pub fn new(d: D) -> Self {
        Self {
            ctx: d,
            phantom: PhantomData,
            ph2: PhantomData,
        }
    }
}
impl<
        D: Tracer<O::Result, U, Y3, S::Result, O2, T2, Y2, S2, Meta: Clone> + AsMut<E>,
        O: InferOp<E, Y3>,
        T: SaneTerminator<O, T, Y, S> + NormalTermBi<O, T, Y, S, O::Result, U, Y3, S::Result>,
        Y,
        Y3: Clone + Ord,
        S: Infer<E, Y3, Result: Default> + Default,
        O2,
        T2,
        Y2: Clone + Ord,
        S2: Default,
        U: From<T::Then>,
        E,
    > Tracer<O, T, Y, S, O2, T2, Y2, S2> for AI<D, E, Y3, U>
{
    type Instance = (Vec<Y3>, Arc<D::Instance>);

    type Meta = (Arc<D::Meta>, Y3);

    fn meta_param(
        &mut self,
        i: &Self::Instance,
        idx: usize,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        let y = i.0[idx].clone();
        let (i, k) = self.ctx.meta_param(i.1.as_ref(), idx, &y, new, k)?;
        Ok(((Arc::new(i), y), k))
    }

    fn select(
        &mut self,
        i: &Self::Instance,
        m: &Self::Meta,
        s: &S,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        let (y, e) = s.infer(self.ctx.as_mut(), &m.1)?;
        // let e = s.extract();
        let (a, b) = self
            .ctx
            .select(i.1.as_ref(), m.0.as_ref(), &e, &y, new, k, span)?;
        Ok(((Arc::new(a), y), b))
    }

    fn op(
        &mut self,
        i: &Self::Instance,
        o: &O,
        y: &Y,
        args: &[Self::Meta],
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        let (y, p) = o.infer(self.ctx.as_mut(), args.iter().map(|x| &x.1))?;
        // let p = o.extract();
        let (v, k) = self.ctx.op(
            i.1.as_ref(),
            &p,
            &y,
            args.iter()
                .map(|a| D::Meta::clone(a.0.as_ref()))
                .collect::<Vec<_>>()
                .as_slice(),
            new,
            k,
            span,
        )?;
        return Ok(((Arc::new(v), y), k));
    }

    fn term(
        state: &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>,
        i: &Self::Instance,
        t: &T,
        mut go: impl FnMut(
            &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>,
            &mut crate::Func<O2, T2, Y2, S2>,
            &BlockTarget<O, T, Y, S>,
            Self::Instance,
        ) -> anyhow::Result<id_arena::Id<crate::Block<O2, T2, Y2, S2>>>,
        valmap: &std::collections::BTreeMap<
            id_arena::Id<crate::Value<O, T, Y, S>>,
            std::sync::Arc<Self::Meta>,
        >,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        mut k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
        // old: &Func<O, T, Y, S>,
        span: Option<Span>,
    ) -> anyhow::Result<()> {
        let t2: U = t
            .norm(
                |a| {
                    Ok(crate::BlockTarget {
                        block: unsafe { a.block.transmute() },
                        args: a
                            .args
                            .iter()
                            .map(|a| {
                                Ok(Use {
                                    value: unsafe { a.value.transmute() },
                                    select: Default::default(), //TODO
                                })
                            })
                            .collect::<anyhow::Result<Vec<_>>>()?,
                        prepend: vec![], //TODO
                    })
                },
                |v| Ok(unsafe { v.transmute() }),
            )?
            .into();
        let st2 = unsafe {
            std::mem::transmute::<
                _,
                &mut super::State<O::Result, U, Y3, S::Result, O2, T2, Y2, S2, D>,
            >(state)
        };
        return D::term(
            st2,
            i.1.as_ref(),
            &t2,
            |s, new, k, i2| {
                let state = unsafe {
                    std::mem::transmute::<_, &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>>(s)
                };
                let i2 = Arc::new(i2);
                let y2 = k
                    .args
                    .iter()
                    .map(|r| {
                        valmap
                            .get(&unsafe { r.value.transmute() })
                            .unwrap()
                            .1
                            .clone()
                    })
                    .collect::<Vec<_>>();
                let i2 = (y2, i2);
                let x = go(
                    state,
                    new,
                    &crate::BlockTarget {
                        block: unsafe { k.block.transmute() },
                        args: k
                            .args
                            .iter()
                            .map(|a| {
                                Ok(Use {
                                    value: unsafe { a.value.transmute() },
                                    select: Default::default(), //TODO
                                })
                            })
                            .collect::<anyhow::Result<Vec<_>>>()?,
                        prepend: vec![], //TODO
                    },
                    i2,
                )?;
                return Ok(x);
            },
            &valmap
                .iter()
                .map(|(a, b)| (unsafe { a.transmute() }, b.0.clone()))
                .collect(),
            new,
            k,
            span,
        );
        // let t2 = t.norm(
        //     |a| {
        //         let v = a
        //             .args
        //             .iter()
        //             .map(|u| {
        //                 let a = &**valmap.get(&u.value).context("in getting the value")?;
        //                 let (s, b) = Tracer::<O, T, Y, S, O2, T2, Y2, S2>::select(
        //                     &mut state.tracer,
        //                     i,
        //                     a,
        //                     &u.select,
        //                     old.opts[u.value].ty(),
        //                     new,
        //                     k,
        //                     old.spans[u.value].clone()
        //                 )?;
        //                 k = b;
        //                 Ok(s)
        //             })
        //             .collect::<anyhow::Result<Vec<_>>>()?;
        //         let v = v.iter().map(|w| new.opts[*w].ty().clone()).collect();
        //         let g = go(state, new, a.block, v)?;
        //         let r = a
        //             .args
        //             .iter()
        //             .map(|a| {
        //                 let v = **valmap.get(&a.value).unwrap();
        //                 Ok(Use {
        //                     value: v,
        //                     select: a.select.infer(&mut state.tracer.ctx, new.opts[v].ty())?.1,
        //                 })
        //             })
        //             .collect::<anyhow::Result<Vec<_>>>()?;
        //         Ok(crate::BlockTarget {
        //             block: g,
        //             args: r,
        //             prepend: vec![],
        //         })
        //     },
        //     |v| Ok(**valmap.get(&v).unwrap()),
        // )?;
        // new.blocks[k].term = t2;
        // new.blocks[k].term_span = span;
        // Ok(())
    }
}
