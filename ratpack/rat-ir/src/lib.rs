use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    iter::once,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use either::Either;
use id_arena::{Arena, Id};
use rat_debug::Span;
use serde::{Deserialize, Serialize};
use util::PerID;
pub mod bi;
pub mod cfg;
pub mod dom;
pub mod droppify;
pub mod maxssa;
pub mod module;
pub mod transform;
pub mod util;
pub mod var;
#[derive(Clone)]
pub struct Then<A: ?Sized, B> {
    pub first: Box<A>,
    pub then: B,
}
#[derive(Clone)]
pub struct Unit<A>(pub A);

pub trait Builder<O, T, Y, S> {
    type Result;
    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)>;
    fn then<B>(self: Box<Self>, b: B) -> Then<Self, B>
    where
        Self: Sized,
    {
        return Then {
            first: self,
            then: b,
        };
    }
}
impl<O, T, Y, S, X: Builder<O, T, Y, S> + ?Sized> Builder<O, T, Y, S> for Box<X> {
    type Result = X::Result;

    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        (*self).build(func, root)
    }
}
pub fn build_fn<
    O,
    T,
    Y,
    S,
    R,
    F: FnOnce(
        &mut Func<O, T, Y, S>,
        Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(R, Id<Block<O, T, Y, S>>)>,
>(
    f: F,
) -> BuildFn<F> {
    BuildFn { fun: f }
}
#[derive(Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct BuildFn<F> {
    pub fun: F,
}
impl<
        O,
        T,
        Y,
        S,
        R,
        F: FnOnce(
            &mut Func<O, T, Y, S>,
            Id<Block<O, T, Y, S>>,
        ) -> anyhow::Result<(R, Id<Block<O, T, Y, S>>)>,
    > Builder<O, T, Y, S> for BuildFn<F>
{
    type Result = R;

    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        (self.fun)(func, root)
    }
}
impl<O, T, Y, S, A: Builder<O, T, Y, S, Result = R>, B: Builder<O, T, Y, S, Result = R>, R>
    Builder<O, T, Y, S> for Either<A, B>
{
    type Result = R;

    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        match *self {
            Either::Left(a) => Box::new(a).build(func, root),
            Either::Right(b) => Box::new(b).build(func, root),
        }
    }
}
impl<O, T, Y, S, A: Builder<O, T, Y, S>, B: FnOnce(A::Result) -> C, C: Builder<O, T, Y, S>>
    Builder<O, T, Y, S> for Then<A, B>
{
    type Result = C::Result;

    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let (a, root) = self.first.build(func, root)?;
        return Box::new((self.then)(a)).build(func, root);
    }
}
impl<O, T, Y, S, A> Builder<O, T, Y, S> for Unit<A> {
    type Result = A;

    fn build(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        Ok((self.0, root))
    }
}
#[derive(Serialize, Deserialize)]
#[serde(default)]
pub struct Func<O, T, Y, S> {
    pub opts: Arena<Value<O, T, Y, S>>,
    pub blocks: Arena<Block<O, T, Y, S>>,
    pub entry: Id<Block<O, T, Y, S>>,
    #[serde(skip)]
    pub pred_cache: BTreeMap<Id<Block<O, T, Y, S>>, BTreeSet<Id<Block<O, T, Y, S>>>>,
    pub spans: PerID<Value<O, T, Y, S>, Option<Span>>,
}
impl<O, T, Y, S> Func<O, T, Y, S> {
    pub fn def_blocks(&self) -> PerID<Value<O, T, Y, S>, Option<Id<Block<O, T, Y, S>>>> {
        let mut r: PerID<Value<O, T, Y, S>, Option<Id<Block<O, T, Y, S>>>> = Default::default();
        for w in self.opts.iter().map(|a| a.0).collect::<Vec<_>>() {
            let k = self
                .blocks
                .iter()
                .find(|b| b.1.insts.contains(&w))
                .map(|a| a.0);
            r[w] = k;
        }
        return r;
    }
}
impl<O, T: Default, Y, S> Default for Func<O, T, Y, S> {
    fn default() -> Self {
        let mut ks: Arena<Block<O, T, Y, S>> = Default::default();
        let entry = ks.alloc(Default::default());
        return Self {
            opts: Default::default(),
            blocks: ks,
            entry,
            pred_cache: Default::default(),
            spans: Default::default(),
        };
    }
}
impl<O: Clone, T: Clone, Y: Clone, S: Clone> Clone for Func<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            opts: self.opts.clone(),
            blocks: self.blocks.clone(),
            entry: self.entry.clone(),
            pred_cache: self.pred_cache.clone(),
            spans: self.spans.clone(),
        }
    }
}
#[derive(Serialize, Deserialize)]
pub struct Use<O, T, Y, S> {
    pub value: Id<Value<O, T, Y, S>>,
    pub select: S,
}
impl<O, T, Y, S: Clone> Clone for Use<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            select: self.select.clone(),
        }
    }
}
impl<O, T, Y, S: PartialEq> PartialEq for Use<O, T, Y, S> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.select == other.select
    }
}
impl<O, T, Y, S: Eq> Eq for Use<O, T, Y, S> {}
impl<O, T, Y, S: PartialOrd> PartialOrd for Use<O, T, Y, S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.value.partial_cmp(&other.value) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.select.partial_cmp(&other.select)
    }
}
impl<O, T, Y, S: Ord> Ord for Use<O, T, Y, S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.value.cmp(&other.value) {
            Ordering::Equal => {}
            ord => return ord,
        }
        self.select.cmp(&other.select)
    }
}
impl<O, T, Y, S> Func<O, T, Y, S> {
    pub fn invalidate_caches(&mut self) {
        self.pred_cache = BTreeMap::new();
    }
}
#[derive(Serialize, Deserialize)]
pub enum Value<O, T, Y, S> {
    Operator(O, Vec<Use<O, T, Y, S>>, Y, PhantomData<T>),
    BlockParam(usize, Id<Block<O, T, Y, S>>, Y),
    Alias(Use<O, T, Y, S>, Y),
}
impl<O: Clone, T, Y: Clone, S: Clone> Clone for Value<O, T, Y, S> {
    fn clone(&self) -> Self {
        match self {
            Self::Operator(arg0, arg1, arg2, arg3) => {
                Self::Operator(arg0.clone(), arg1.clone(), arg2.clone(), arg3.clone())
            }
            Self::BlockParam(arg0, arg1, arg2) => {
                Self::BlockParam(arg0.clone(), arg1.clone(), arg2.clone())
            }
            Self::Alias(arg0, arg1) => Self::Alias(arg0.clone(), arg1.clone()),
        }
    }
}
impl<O, T, Y, S> Value<O, T, Y, S> {
    pub fn ty(&self) -> &Y {
        match self {
            Value::Operator(_, _, y, _) => y,
            Value::BlockParam(_, _, y) => y,
            Value::Alias(_, y) => y,
        }
    }
}
#[derive(Serialize, Deserialize)]
pub struct Block<O, T, Y, S> {
    pub insts: Vec<Id<Value<O, T, Y, S>>>,
    pub term: T,
    pub params: Vec<Y>,
    pub term_span: Option<Span>,
}
impl<O, T: Clone, Y: Clone, S> Clone for Block<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            insts: self.insts.clone(),
            term: self.term.clone(),
            params: self.params.clone(),
            term_span: self.term_span.clone(),
        }
    }
}
impl<O, T: Default, Y, S> Default for Block<O, T, Y, S> {
    fn default() -> Self {
        Self {
            insts: Default::default(),
            term: Default::default(),
            params: Default::default(),
            term_span: None,
        }
    }
}
#[derive(Serialize, Deserialize)]
pub struct BlockTarget<O, T, Y, S> {
    pub block: Id<Block<O, T, Y, S>>,
    pub args: Vec<Use<O, T, Y, S>>,
    pub prepend: Vec<Y>,
}
impl<O, T, Y: Clone, S: Clone> Clone for BlockTarget<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            args: self.args.clone(),
            prepend: self.prepend.clone(),
        }
    }
}
impl<O, T, Y, S> BlockTarget<O, T, Y, S> {
    pub fn block_types<'a>(&'a self, f: &'a Func<O, T, Y, S>) -> impl Iterator<Item = &'a Y> + 'a {
        return self
            .prepend
            .iter()
            .chain(self.args.iter().map(|b| f.opts[b.value].ty()));
    }
}
pub trait SaneTerminator<O, T, Y, S> {
    fn targets(&self) -> BTreeSet<Id<Block<O, T, Y, S>>> {
        return self.t2s().map(|a| a.block).collect();
    }
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a;
    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a;
    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a;
    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a;
}
impl<O, T, Y, S, A: SaneTerminator<O, T, Y, S>, B: SaneTerminator<O, T, Y, S>>
    SaneTerminator<O, T, Y, S> for Either<A, B>
{
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            Either::Left(a) => Either::Left(a.uses()),
            Either::Right(b) => Either::Right(b.uses()),
        }
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            Either::Left(a) => Either::Left(a.uses_mut()),
            Either::Right(b) => Either::Right(b.uses_mut()),
        }
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            Either::Left(a) => Either::Left(a.t2s()),
            Either::Right(b) => Either::Right(b.t2s()),
        }
    }

    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            Either::Left(a) => Either::Left(a.t2s_mut()),
            Either::Right(b) => Either::Right(b.t2s_mut()),
        }
    }
}
impl<O, T, Y, S> SaneTerminator<O, T, Y, S> for BlockTarget<O, T, Y, S> {
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.args.iter()
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.args.iter_mut()
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(self)
    }

    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(self)
    }
}
impl<O, T: SaneTerminator<O, T, Y, S>, Y, S> Func<O, T, Y, S> {
    pub fn preds(&mut self, a: Id<Block<O, T, Y, S>>) -> BTreeSet<Id<Block<O, T, Y, S>>> {
        loop {
            if let Some(b) = self.pred_cache.get(&a) {
                return b.clone();
            }
            let mut x = BTreeSet::new();
            for (c, b) in self.blocks.iter() {
                if b.term.targets().contains(&a) {
                    x.insert(c);
                }
            }
            self.pred_cache.insert(a, x);
        }
    }
    pub fn preds_ref(&self, a: Id<Block<O, T, Y, S>>) -> BTreeSet<Id<Block<O, T, Y, S>>> {
        if let Some(b) = self.pred_cache.get(&a) {
            return b.clone();
        }
        let mut x = BTreeSet::new();
        for (c, b) in self.blocks.iter() {
            if b.term.targets().contains(&a) {
                x.insert(c);
            }
        }
        return x;
    }
}
impl<O, T, Y: Clone, S> Func<O, T, Y, S> {
    pub fn add_blockparam(&mut self, k: Id<Block<O, T, Y, S>>, ty: Y) -> Id<Value<O, T, Y, S>> {
        let i = self.blocks[k].params.len();
        self.blocks[k].params.push(ty.clone());
        let v = self.opts.alloc(Value::BlockParam(i, k, ty));
        self.blocks[k].insts = vec![v]
            .into_iter()
            .chain(self.blocks[k].insts.iter().map(|a| *a))
            .collect();
        return v;
    }
}

pub trait Bound {
    type O<O, T, Y, S>;
    type T<O, T, Y, S>;
    type Y<O, T, Y, S>;
    type S<O, T, Y, S>;
}
macro_rules! bound_impls {
    ($ty:tt, $x:tt) => {
        impl<B: Bound> Clone for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Clone,
        {
            fn clone(&self) -> Self {
                return Self(self.0.clone());
            }
        }
        impl<B: Bound> Default for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Default,
        {
            fn default() -> Self {
                return Self(Default::default());
            }
        }
        impl<B: Bound> PartialEq for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                return self.0 == other.0;
            }
        }
        impl<B: Bound> Eq for $ty<B> where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Eq
        {
        }
        impl<B: Bound> PartialOrd for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: PartialOrd,
        {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                return self.0.partial_cmp(&other.0);
            }
        }
        impl<B: Bound> Ord for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Ord,
        {
            fn cmp(&self, other: &Self) -> Ordering {
                return self.0.cmp(&other.0);
            }
        }
        // impl<B: Bound,A> $crate::util::Extract<A> for $ty<B>
        // where
        //     B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: $crate::util::Extract<A>,{
        //         fn extract(&self) -> A{
        //             return self.0.extract();
        //         }
        //     }
        impl<B: Bound, C: Bound> $crate::util::Extract<$ty<C>> for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: $crate::util::Extract<
                C::$x<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
            >,
        {
            fn extract(&self) -> $ty<C> {
                return $ty(self.0.extract());
            }
        }
        impl<B: Bound, C: Bound, D> $crate::util::ExtractIn<D, $ty<C>> for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: $crate::util::ExtractIn<
                D,
                C::$x<BoundOp<C>, BoundTerm<C>, BoundType<C>, BoundSelect<C>>,
            >,
        {
            fn extract_in(&self, ctx: &mut D) -> $ty<C> {
                return $ty(self.0.extract_in(ctx));
            }
        }
        impl<B: Bound, C> $crate::util::Push<C> for $ty<B>
        where
            B::$x<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: $crate::util::Push<C>,
        {
            fn push(c: C) -> Either<Self, C> {
                return B::$x::<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>::push(c)
                    .map_left($ty);
            }
        }
    };
}
#[derive(derive_more::Deref, derive_more::DerefMut, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
#[serde(bound(
    serialize = "B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Serialize",
    deserialize = "B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Deserialize<'de>"
))]
pub struct BoundOp<B: Bound>(pub B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>);
#[derive(derive_more::Deref, derive_more::DerefMut, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
#[serde(bound(
    serialize = "B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Serialize",
    deserialize = "B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Deserialize<'de>"
))]
pub struct BoundTerm<B: Bound>(pub B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>);
#[derive(derive_more::Deref, derive_more::DerefMut, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
#[serde(bound(
    serialize = "B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Serialize",
    deserialize = "B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Deserialize<'de>"
))]
pub struct BoundType<B: Bound>(pub B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>);
#[derive(derive_more::Deref, derive_more::DerefMut, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
#[serde(bound(
    serialize = "B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Serialize",
    deserialize = "B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Deserialize<'de>"
))]
pub struct BoundSelect<B: Bound>(pub B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>);
bound_impls!(BoundOp, O);
bound_impls!(BoundTerm, T);
bound_impls!(BoundType, Y);
bound_impls!(BoundSelect, S);
pub trait BoundExt: Bound {
    type Bo;
    type Bt;
    type By;
    type Bs;
}
impl<T: Bound> BoundExt for T {
    type Bo = BoundOp<T>;

    type Bt = BoundTerm<T>;

    type By = BoundType<T>;

    type Bs = BoundSelect<T>;
}
impl<B: Bound> SaneTerminator<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>
    for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        SaneTerminator<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn uses<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a Use<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>> + 'a
    where
        BoundOp<B>: 'a,
        BoundTerm<B>: 'a,
        BoundType<B>: 'a,
        BoundSelect<B>: 'a,
    {
        self.0.uses()
    }

    fn uses_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = &'a mut Use<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>> + 'a
    where
        BoundOp<B>: 'a,
        BoundTerm<B>: 'a,
        BoundType<B>: 'a,
        BoundSelect<B>: 'a,
    {
        self.0.uses_mut()
    }

    fn t2s<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>> + 'a
    where
        BoundOp<B>: 'a,
        BoundTerm<B>: 'a,
        BoundType<B>: 'a,
        BoundSelect<B>: 'a,
    {
        self.0.t2s()
    }

    fn t2s_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<
        Item = &'a mut BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    > + 'a
    where
        BoundOp<B>: 'a,
        BoundTerm<B>: 'a,
        BoundType<B>: 'a,
        BoundSelect<B>: 'a,
    {
        self.0.t2s_mut()
    }
}

pub struct Call<O, T, Y, S> {
    pub func: Id<Func<O, T, Y, S>>,
}
impl<O, T, Y, S> Clone for Call<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
}
