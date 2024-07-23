use std::{collections::BTreeMap, hash::Hash, sync::Arc};

use anyhow::Context;
use either::Either;
use id_arena::Id;
use once_map::OnceMap;
use rat_debug::Span;

use crate::{Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, Value};
pub mod ai;
pub mod license;
pub mod simple;

pub trait NormalTermBi<O, T, Y, S, O2, T2, Y2, S2> {
    type Then;
    fn norm(
        &self,
        to_dst: impl FnMut(&BlockTarget<O, T, Y, S>) -> anyhow::Result<BlockTarget<O2, T2, Y2, S2>>,
        m: impl FnMut(Id<Value<O, T, Y, S>>) -> anyhow::Result<Id<Value<O2, T2, Y2, S2>>>,
    ) -> anyhow::Result<Self::Then>;
}
impl<
        O,
        T,
        Y,
        S,
        O2,
        T2,
        Y2,
        S2,
        A: NormalTermBi<O, T, Y, S, O2, T2, Y2, S2>,
        B: NormalTermBi<O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTermBi<O, T, Y, S, O2, T2, Y2, S2> for Either<A, B>
{
    type Then = Either<A::Then, B::Then>;

    fn norm(
        &self,
        to_dst: impl FnMut(&BlockTarget<O, T, Y, S>) -> anyhow::Result<BlockTarget<O2, T2, Y2, S2>>,
        m: impl FnMut(Id<Value<O, T, Y, S>>) -> anyhow::Result<Id<Value<O2, T2, Y2, S2>>>,
    ) -> anyhow::Result<Self::Then> {
        Ok(match self {
            Either::Left(a) => Either::Left(a.norm(to_dst, m)?),
            Either::Right(b) => Either::Right(b.norm(to_dst, m)?),
        })
    }
}
impl<O, T, Y, S, O2, T2, Y2, S2> NormalTermBi<O, T, Y, S, O2, T2, Y2, S2>
    for BlockTarget<O, T, Y, S>
{
    type Then = BlockTarget<O2, T2, Y2, S2>;

    fn norm(
        &self,
        mut to_dst: impl FnMut(&BlockTarget<O, T, Y, S>) -> anyhow::Result<BlockTarget<O2, T2, Y2, S2>>,
        m: impl FnMut(Id<Value<O, T, Y, S>>) -> anyhow::Result<Id<Value<O2, T2, Y2, S2>>>,
    ) -> anyhow::Result<Self::Then> {
        to_dst(self)
    }
}
impl<B: Bound, C: Bound>
    NormalTermBi<
        BoundOp<B>,
        BoundTerm<B>,
        BoundType<B>,
        BoundSelect<B>,
        BoundOp<C>,
        BoundTerm<C>,
        BoundType<C>,
        BoundSelect<C>,
    > for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: NormalTermBi<
        BoundOp<B>,
        BoundTerm<B>,
        BoundType<B>,
        BoundSelect<B>,
        BoundOp<C>,
        BoundTerm<C>,
        BoundType<C>,
        BoundSelect<C>,
        Then = C::T<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
    >,
{
    type Then = BoundTerm<C>;

    fn norm(
        &self,
        to_dst: impl FnMut(
            &BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        ) -> anyhow::Result<
            BlockTarget<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
        >,
        m: impl FnMut(
            Id<Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
        ) -> anyhow::Result<
            Id<Value<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>>,
        >,
    ) -> anyhow::Result<Self::Then> {
        return Ok(BoundTerm(self.0.norm(to_dst, m)?));
    }
}
pub trait Tracer<O, T, Y, S, O2, T2, Y2, S2>: Sized {
    type Instance: Ord + Eq + Clone;
    type Meta;
    fn meta_param(
        &mut self,
        i: &Self::Instance,
        idx: usize,
        y: &Y,
        new: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, Id<Block<O2, T2, Y2, S2>>)>;
    fn select(
        &mut self,
        i: &Self::Instance,
        m: &Self::Meta,
        s: &S,
        y: &Y,
        new: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, Id<Block<O2, T2, Y2, S2>>)>;
    fn op(
        &mut self,
        i: &Self::Instance,
        o: &O,
        y: &Y,
        args: &[Self::Meta],
        new: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, Id<Block<O2, T2, Y2, S2>>)>;
    fn term(
        state: &mut State<O, T, Y, S, O2, T2, Y2, S2, Self>,
        i: &Self::Instance,
        t: &T,
        go: impl FnMut(
            &mut State<O, T, Y, S, O2, T2, Y2, S2, Self>,
            &mut Func<O2, T2, Y2, S2>,
            &BlockTarget<O, T, Y, S>,

            Self::Instance,
        ) -> anyhow::Result<Id<Block<O2, T2, Y2, S2>>>,
        valmap: &BTreeMap<Id<crate::Value<O, T, Y, S>>, Arc<Self::Meta>>,
        new: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        // old: &Func<O, T, Y, S>,
        span: Option<Span>,
    ) -> anyhow::Result<()>;
}
pub struct State<O, T, Y, S, O2, T2, Y2, S2, C: Tracer<O, T, Y, S, O2, T2, Y2, S2>> {
    pub tracer: C,
    pub in_map: BTreeMap<(Id<Block<O, T, Y, S>>, C::Instance), Id<Block<O2, T2, Y2, S2>>>,
}
pub fn trace_func<O, T, Y, S, O2, T2: Default, Y2, S2, C: Tracer<O, T, Y, S, O2, T2, Y2, S2>>(
    tracer: &mut C,
    old: &Func<O, T, Y, S>,
    new: &mut Func<O2, T2, Y2, S2>,
    k: Id<Block<O, T, Y, S>>,
    i: C::Instance,
) -> anyhow::Result<()> {
    return replace_with::replace_with_or_abort_and_return(tracer, move |tracer| {
        // let mut x = Default::default();
        let mut state = State {
            tracer,
            in_map: BTreeMap::new(),
        };
        let k = match trace_block(&mut state, old, new, k, i) {
            Ok(a) => a,
            Err(e) => return (Err(e), state.tracer),
        };
        new.entry = k;
        return (Ok(()), state.tracer);
    });
}
pub fn trace_block<O, T, Y, S, O2, T2: Default, Y2, S2, C: Tracer<O, T, Y, S, O2, T2, Y2, S2>>(
    state: &mut State<O, T, Y, S, O2, T2, Y2, S2, C>,
    old: &Func<O, T, Y, S>,
    new: &mut Func<O2, T2, Y2, S2>,
    k: Id<Block<O, T, Y, S>>,
    i: C::Instance,
) -> anyhow::Result<Id<Block<O2, T2, Y2, S2>>> {
    if let Some(v) = state.in_map.get(&(k, i.clone())) {
        return Ok(*v);
    }
    let v = new.blocks.alloc(Default::default());
    state.in_map.insert((k, i.clone()), v);
    let mut w = v;
    let params = old.blocks[k]
        .params
        .iter()
        .enumerate()
        .map(|(idx, y)| {
            let (a, b) = state.tracer.meta_param(&i, idx, y, new, w)?;
            w = b;
            Ok(a)
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let params = params.into_iter().map(Arc::new).collect::<Vec<_>>();
    let mut values: BTreeMap<
        Id<crate::Value<O, T, Y, S>>,
        Arc<<C as Tracer<O, T, Y, S, O2, T2, Y2, S2>>::Meta>,
    > = BTreeMap::new();
    for inst in old.blocks[k].insts.iter() {
        values.insert(
            *inst,
            match &old.opts[*inst] {
                crate::Value::Operator(o, u, y, _) => {
                    let ms = u
                        .iter()
                        .map(|u| {
                            let a = &**values.get(&u.value).context("in getting the value")?;
                            let (s, b) = state.tracer.select(
                                &i,
                                a,
                                &u.select,
                                y,
                                new,
                                w,
                                old.spans[u.value].clone(),
                            )?;
                            w = b;
                            Ok(s)
                        })
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    let (s, b) =
                        state
                            .tracer
                            .op(&i, o, y, &ms, new, w, old.spans[*inst].clone())?;
                    w = b;
                    Arc::new(s)
                }
                crate::Value::BlockParam(i, _, _) => params[*i].clone(),
                crate::Value::Alias(u, y) => {
                    let a = &**values.get(&u.value).context("in getting the value")?;
                    let (s, b) = state.tracer.select(
                        &i,
                        a,
                        &u.select,
                        y,
                        new,
                        w,
                        old.spans[u.value].clone(),
                    )?;
                    w = b;
                    Arc::new(s)
                }
            },
        );
    }
    if let Some(t) = old.blocks[k].term.as_ref() {
        C::term(
            state,
            &i,
            t,
            |a, new, b, c| trace_block(a, old, new, b.block, c),
            &values,
            new,
            w,
            // old,
            old.blocks[k].term_span.clone(),
        )?;
    }

    Ok(v)
}
