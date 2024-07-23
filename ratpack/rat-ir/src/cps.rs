use std::iter::once;

use id_arena::Id;

use crate::{util::Push, Block, BlockTarget, Builder, Func, Use, Value};
pub trait Static<O, T, Y, S> {
    fn to_op<'a>(&'a self) -> impl Builder<O, T, Y, S, Result = Id<Value<O, T, Y, S>>> + 'a;
}

pub trait Finite<O, T, Y, S>: Static<O, T, Y, S> {
    fn br<R: Clone>(v: Id<Value<O, T, Y, S>>) -> impl CpsManyBuilder<O, T, Y, S, R, Result = Self>;
}

pub trait CpsManyBuilder<O, T, Y, S, R> {
    type Result;
    fn build<'b: 'd, 'c: 'd, 'd, U: 'd>(
        self,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        go: impl FnMut(
                R,
                Self::Result,
                &mut Func<O, T, Y, S>,
                Id<Block<O, T, Y, S>>,
            ) -> anyhow::Result<U>
            + 'c,
    ) -> anyhow::Result<impl Iterator<Item = U> + 'd>
    where
        Self::Result: 'd,
        R: 'd,
        Self: 'd;
    fn then<F>(self, go: F) -> Then<Self, F>
    where
        Self: Sized,
    {
        return Then { base: self, go };
    }
}
pub struct FromBuilder<F> {
    pub go: F,
}
impl<O, T, Y, S, F: Builder<O, T, Y, S>, R> CpsManyBuilder<O, T, Y, S, R> for FromBuilder<F> {
    type Result = F::Result;

    fn build<'b: 'd, 'c: 'd, 'd, U: 'd>(
        self,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        mut go: impl FnMut(
                R,
                Self::Result,
                &mut Func<O, T, Y, S>,
                Id<Block<O, T, Y, S>>,
            ) -> anyhow::Result<U>
            + 'c,
    ) -> anyhow::Result<impl Iterator<Item = U> + 'd>
    where
        Self::Result: 'd,
        R: 'd,
    {
        let x = Box::new(self.go);
        let (v, k) = x.build(func, root)?;
        let x = go(relay, v, func, k)?;
        Ok(once(x))
    }
}
pub struct ToBlock<X, Y> {
    pub wrapped: X,
    pub r#type: Y,
}
impl<
        O,
        T: Default + Push<BlockTarget<O, T, Y, S>>,
        Y: Clone,
        S: Default,
        X: CpsManyBuilder<O, T, Y, S, (), Result = Id<Value<O, T, Y, S>>>,
    > Builder<O, T, Y, S> for ToBlock<X, Y>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build(
        mut self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let k = func.blocks.alloc(Default::default());
        for _ in self.wrapped.build(func, root, (), |_, v, f, b| {
            f.blocks[b].term = Some(T::push(BlockTarget {
                block: k,
                args: vec![Use {
                    value: v,
                    select: S::default(),
                }],
                prepend: vec![],
            })
            .map_right(|_| ())
            .unwrap_left());
            Ok(())
        })? {}
        let p = func.add_blockparam(k, self.r#type);
        return Ok((p, k));
    }
}

pub struct Then<A, F> {
    pub base: A,
    pub go: F,
}

impl<
        O,
        T,
        Y,
        S,
        R,
        A: CpsManyBuilder<O, T, Y, S, R>,
        F: FnMut(A::Result) -> G,
        G: CpsManyBuilder<O, T, Y, S, R>,
    > CpsManyBuilder<O, T, Y, S, R> for Then<A, F>
{
    type Result = G::Result;

    fn build<'b: 'd, 'c: 'd, 'd, U: 'd>(
        self,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        mut go: impl FnMut(
                R,
                Self::Result,
                &mut Func<O, T, Y, S>,
                Id<Block<O, T, Y, S>>,
            ) -> anyhow::Result<U>
            + 'c,
    ) -> anyhow::Result<impl Iterator<Item = U> + 'd>
    where
        Self::Result: 'd,
        R: 'd,
        Self: 'd,
    {
        let Then { base, go: mut go2 } = self;
        return base
            .build(func, root, relay, move |r, a, f, b| {
                Ok((go2)(a)
                    .build(f, b, r, &mut go)?
                    .collect::<Vec<_>>()
                    .into_iter())
            })
            .map(|a| a.flatten());
    }
}
pub unsafe trait DynCpsManyBuilder<O, T, Y, S, R> {
    type Result;
    fn dyn_build<'b: 'd, 'c: 'd, 'd>(
        self: Box<Self>,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        go: Box<
            dyn FnMut(
                    R,
                    Self::Result,
                    &mut Func<O, T, Y, S>,
                    Id<Block<O, T, Y, S>>,
                ) -> anyhow::Result<*mut ()>
                + 'c,
        >,
    ) -> anyhow::Result<Box<dyn Iterator<Item = *mut ()> + 'd>>
    where
        Self::Result: 'd,
        R: 'd,
        Self: 'd;
}
unsafe impl<O, T, Y, S, R, A: CpsManyBuilder<O, T, Y, S, R>> DynCpsManyBuilder<O, T, Y, S, R>
    for A
{
    type Result = A::Result;

    fn dyn_build<'b: 'd, 'c: 'd, 'd>(
        self: Box<Self>,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        go: Box<
            dyn FnMut(
                    R,
                    Self::Result,
                    &mut Func<O, T, Y, S>,
                    Id<Block<O, T, Y, S>>,
                ) -> anyhow::Result<*mut ()>
                + 'c,
        >,
    ) -> anyhow::Result<Box<dyn Iterator<Item = *mut ()> + 'd>>
    where
        Self::Result: 'd,
        R: 'd,
        Self: 'd,
    {
        return Ok(Box::new(self.build(func, root, relay, go)?));
    }
}
impl<O, T, Y, S, R, A: DynCpsManyBuilder<O, T, Y, S, R> + ?Sized> CpsManyBuilder<O, T, Y, S, R>
    for Box<A>
{
    type Result = A::Result;

    fn build<'b: 'd, 'c: 'd, 'd, U: 'd>(
        self,
        func: &'b mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        relay: R,
        mut go: impl FnMut(
                R,
                Self::Result,
                &mut Func<O, T, Y, S>,
                Id<Block<O, T, Y, S>>,
            ) -> anyhow::Result<U>
            + 'c,
    ) -> anyhow::Result<impl Iterator<Item = U> + 'd>
    where
        Self::Result: 'd,
        R: 'd,
        Self: 'd,
    {
        let x = Box::new(self).dyn_build(
            func,
            root,
            relay,
            Box::new(move |e, r, f, b| {
                let x = go(e, r, f, b)?;

                Ok(Box::into_raw(Box::new(x)) as *mut ())
            }),
        )?;

        return Ok(x.map(|y| *unsafe { Box::from_raw(y as *mut U) }));
    }
}
