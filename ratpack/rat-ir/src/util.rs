use std::{
    iter::once,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use bytes::Bytes;
use either::Either;
use id_arena::Id;
use serde::{Deserialize, Serialize};

use crate::{
    bi::NormalTermBi,
    module::TailCall,
    transform::{ctx::NormalTermIn, NormalTerm},
    BlockTarget, Call, SaneTerminator, Use, Value,
};
#[derive(Debug,Serialize,Deserialize)]
pub struct PerID<A, B> {
    pub data: Vec<B>,
    default: B,
    phantom: PhantomData<Id<A>>,
}
impl<A, B: Default> Default for PerID<A, B> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            default: Default::default(),
            phantom: Default::default(),
        }
    }
}
impl<A, B: Clone> Clone for PerID<A, B> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            default: self.default.clone(),
            phantom: self.phantom.clone(),
        }
    }
}
impl<A, B> Index<Id<A>> for PerID<A, B> {
    type Output = B;

    fn index(&self, index: Id<A>) -> &Self::Output {
        return self.data.get(index.index()).unwrap_or(&self.default);
    }
}
impl<A, B: Default + Clone> IndexMut<Id<A>> for PerID<A, B> {
    fn index_mut(&mut self, idx: Id<A>) -> &mut Self::Output {
        if idx.index() >= self.data.len() {
            self.data.resize(idx.index() + 1, B::default());
        }
        &mut self.data[idx.index()]
    }
}
#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub enum ObjectOriented {
    NewObj(String),
    GetField(String),
    SetField(String),
    CallMethod(String),
}
#[derive(Clone, Debug, Hash, PartialEq, PartialOrd, Ord, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    DivU,
    ModU,
    DivS,
    ModS,
    Shl,
    ShrS,
    ShrU,
}
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct If<O, T, Y, S, W> {
    pub val: Use<O, T, Y, S>,
    pub then: W,
    pub r#else: Option<W>,
}
impl<O, T, Y, S, W: SaneTerminator<O, T, Y, S>> SaneTerminator<O, T, Y, S> for If<O, T, Y, S, W> {
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a crate::Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(&self.val)
            .chain(self.then.uses())
            .chain(self.r#else.iter().flat_map(|x| x.uses()))
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut crate::Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(&mut self.val)
            .chain(self.then.uses_mut())
            .chain(self.r#else.iter_mut().flat_map(|x| x.uses_mut()))
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a crate::BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.then
            .t2s()
            .chain(self.r#else.iter().flat_map(|b| b.t2s()))
    }

    fn t2s_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = &'a mut crate::BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.then
            .t2s_mut()
            .chain(self.r#else.iter_mut().flat_map(|b| b.t2s_mut()))
    }
}
pub trait Bt<O, T, Y, S>: Push<BlockTarget<O,T,Y,S>> {
    fn bt(x: BlockTarget<O, T, Y, S>) -> Self;
}
impl<O,T,Y,S,A: Push<BlockTarget<O,T,Y,S>>> Bt<O,T,Y,S> for A{
    fn bt(x: BlockTarget<O, T, Y, S>) -> Self {
        Self::push(x).map_right(|_|()).unwrap_left()
    }
}
pub trait Extract<A> {
    fn extract(&self) -> A;
}
impl<T: Extract<A>, U: Extract<A>, A> Extract<A> for Either<T, U> {
    fn extract(&self) -> A {
        match self {
            Either::Left(a) => a.extract(),
            Either::Right(b) => b.extract(),
        }
    }
}
pub trait ExtractIn<C, A> {
    fn extract_in(&self, ctx: &mut C) -> A;
}
impl<A, C, T: ExtractIn<C, A>, U: ExtractIn<C, A>> ExtractIn<C, A> for Either<T, U> {
    fn extract_in(&self, ctx: &mut C) -> A {
        match self {
            Either::Left(a) => a.extract_in(ctx),
            Either::Right(b) => b.extract_in(ctx),
        }
    }
}
pub trait Push<B>: Sized {
    fn push(b: B) -> Either<Self, B>;
}
impl<A: Push<B>, C: Push<B>, B> Push<B> for Either<A, C> {
    fn push(b: B) -> Either<Self, B> {
        match A::push(b) {
            Either::Left(a) => Either::Left(Either::Left(a)),
            Either::Right(b) => C::push(b).map_left(Either::Right),
        }
    }
}
#[macro_export]
macro_rules! no_push {
    (type $NewType:ident;) => {
        impl<B: 'static> $crate::util::Push<B> for $NewType{
            fn push(b: B) -> ::either::Either<Self, B>{
                ::castaway::match_type!(b,{
                    Self as s => ::either::Either::Left(s),
                    b => ::either::Either::Right(b),
                })
            }
        }
    };
    (type $NewType:ident<$($genArgs:tt),*>;) => {
        impl<B: 'static,$($genArgs),*> $crate::util::Push<B> for $NewType<$($genArgs),*> where Self: 'static{
            fn push(b: B) -> ::either::Either<Self, B>{
                ::castaway::match_type!(b,{
                    Self as s => ::either::Either::Left(s),
                    b => ::either::Either::Right(b),
                })
            }
        }
    };
    ($pub:vis struct $NewType:ident $body:tt;) => {
        no_push!(type $NewType;)
    };
    ($pub:vis enum $NewType:ident $body:tt;) => {
        no_push!(type $NewType;)
    };
    ($pub:vis struct $NewType:ident<$($genArgs:tt),*> $body:tt;) => {
        no_push!(type $NewType<$($genArgs),*>;)
    };
    ($pub:vis enum $NewType:ident<$($genArgs:tt),*> $body:tt;) => {
        no_push!(type $NewType<$($genArgs),*>;)
    };
}
no_push!(
    type BinOp;
);
no_push!(
    type Call<O, T, Y, S>;
);
no_push!(
    type TailCall<O, T, Y, S>;
);
no_push!(
    type BlockTarget<O, T, Y, S>;
);
no_push!(
    type If<O, T, Y, S, W>;
);
pub struct Bool {}
no_push!(
    type Bool;
);
no_push!(
    type Vec<T>;
);
no_push!(
    type Option<T>;
);
#[derive(Clone,Serialize,Deserialize)]
pub struct DropGuest<Y> {
    pub ty: Y
}
no_push!(
    type DropGuest<Y>;
);
no_push!(type u64;);
no_push!(type u32;);
no_push!(type u16;);
no_push!(type u8;);
no_push!(type usize;);
no_push!(type Bytes;);
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Catch<O, T, Y, S, W> {
    pub wrapped: W,
    pub catch: Option<BlockTarget<O, T, Y, S>>,
}
impl<C: 'static, O, T, Y, S, W: Push<C>> Push<C> for Catch<O, T, Y, S, W>
where
    Self: 'static,
{
    fn push(b: C) -> Either<Self, C> {
        castaway::match_type!(b,{
            Self as s => Either::Left(s),
            s => W::push(s).map_left(|x|Catch { wrapped: x, catch: None })
        })
    }
}
impl<O, T, Y, S, W: SaneTerminator<O, T, Y, S>> SaneTerminator<O, T, Y, S>
    for Catch<O, T, Y, S, W>
{
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.wrapped
            .uses()
            .chain(self.catch.iter().flat_map(|a| a.uses()))
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.wrapped
            .uses_mut()
            .chain(self.catch.iter_mut().flat_map(|a| a.uses_mut()))
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.wrapped.t2s().chain(self.catch.iter())
    }

    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.wrapped.t2s_mut().chain(self.catch.iter_mut())
    }
}
impl<
        O,
        T,
        Y: Extract<Y2>,
        S: Extract<S2>,
        O2,
        T2,
        Y2,
        S2,
        W: NormalTerm<O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTerm<O, T, Y, S, O2, T2, Y2, S2> for Catch<O, T, Y, S, W>
{
    type Then = Catch<O2, T2, Y2, S2, W::Then>;

    fn norm(
        &self,
        to_dst: &std::collections::BTreeMap<
            Id<crate::Block<O, T, Y, S>>,
            Id<crate::Block<O2, T2, Y2, S2>>,
        >,
        m: &std::collections::BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        Catch {
            wrapped: self.wrapped.norm(to_dst, m),
            catch: self.catch.as_ref().map(|x| NormalTerm::norm(x, to_dst, m)),
        }
    }
}
impl<
        C,
        O,
        T,
        Y: Extract<Y2>,
        S: Extract<S2>,
        O2,
        T2,
        Y2,
        S2,
        W: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for Catch<O, T, Y, S, W>
{
    type Then = Catch<O2, T2, Y2, S2, W::Then>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &std::collections::BTreeMap<
            Id<crate::Block<O, T, Y, S>>,
            Id<crate::Block<O2, T2, Y2, S2>>,
        >,
        m: &std::collections::BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        Catch {
            wrapped: self.wrapped.norm(ctx, to_dst, m),
            catch: self
                .catch
                .as_ref()
                .map(|x| NormalTermIn::norm(x, ctx, to_dst, m)),
        }
    }
}
