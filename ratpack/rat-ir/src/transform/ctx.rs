use std::{collections::BTreeMap, marker::PhantomData};

use either::Either;
use id_arena::Id;

use crate::{
    dom::DoMap,
    maxssa::{maxssa, MaxSSA},
    util::{Extract, ExtractIn, If},
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator,
    Use, Value,
};
pub trait NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> {
    type Then;
    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then;
}
impl<
        C,
        O,
        T,
        Y,
        S: Extract<S2>,
        O2,
        T2,
        Y2,
        S2,
        A: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for If<O, T, Y, S, A>
{
    type Then = If<O2, T2, Y2, S2, A::Then>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        If {
            val: Use {
                value: m.get(&self.val.value).copied().unwrap(),
                select: self.val.select.extract(),
            },
            then: self.then.norm(ctx, to_dst, m),
            r#else: self.r#else.as_ref().map(|s| s.norm(ctx, to_dst, m)),
        }
    }
}
impl<
        C,
        O,
        T,
        Y,
        S,
        O2,
        T2,
        Y2,
        S2,
        A: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2>,
        B: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for Either<A, B>
{
    type Then = Either<A::Then, B::Then>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        match self {
            Either::Left(a) => Either::Left(a.norm(ctx, to_dst, m)),
            Either::Right(b) => Either::Right(b.norm(ctx, to_dst, m)),
        }
    }
}
impl<C, O, T, Y: Extract<Y2>, S: Extract<S2>, O2, T2, Y2, S2>
    NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for BlockTarget<O, T, Y, S>
{
    type Then = BlockTarget<O2, T2, Y2, S2>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        BlockTarget {
            block: to_dst.get(&self.block).copied().unwrap(),
            args: self
                .args
                .iter()
                .map(|a| Use {
                    value: m.get(&a.value).copied().unwrap(),
                    select: a.select.extract(),
                })
                .collect(),
            prepend: self.prepend.iter().map(|a| a.extract()).collect(),
        }
    }
}
impl<B: Bound, O2, T2, Y2, S2, D>
    NormalTermIn<D, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, O2, T2, Y2, S2>
    for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        NormalTermIn<D, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, O2, T2, Y2, S2>,
{
    type Then = <B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> as NormalTermIn<
        D,
        BoundOp<B>,
        BoundTerm<B>,
        BoundType<B>,
        BoundSelect<B>,
        O2,
        T2,
        Y2,
        S2,
    >>::Then;

    fn norm(
        &self,
        ctx: &mut D,
        to_dst: &BTreeMap<
            Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
            Id<Block<O2, T2, Y2, S2>>,
        >,
        m: &BTreeMap<
            Id<Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
            Id<Value<O2, T2, Y2, S2>>,
        >,
    ) -> Self::Then {
        self.0.norm(ctx, to_dst, m)
    }
}
