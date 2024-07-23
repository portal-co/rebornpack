use either::Either;
use std::hash::Hash;
use std::{fmt::Display, iter::once, ops::Deref};

use id_arena::Id;
use rat_ast::export::{CffAst, ExportAst, ExportTerm, ReloopAst, ToIdAst};
use rat_ir::util::If;
use rat_ir::{Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, Value};
pub mod langs;
pub trait Script:
    Into<String> + From<String> + Deref<Target = str> + Display + Eq + Ord + Hash + Clone + Default
{
}
impl<
        A: Into<String>
            + From<String>
            + Deref<Target = str>
            + Display
            + Eq
            + Ord
            + Hash
            + Clone
            + Default,
    > Script for A
{
}
#[macro_export]
macro_rules! script {
    ($a:ident) => {
        #[derive(
            Clone,
            Eq,
            PartialEq,
            Ord,
            PartialOrd,
            Hash,
            derive_more::Into,
            derive_more::From,
            derive_more::Deref,
            derive_more::Display,
            Default,
        )]
        #[deref(forward)]
        pub struct $a(pub String);
    };
}
#[derive(
    PartialEq,
    PartialOrd,
    Ord,
    Eq,
    Hash,
    Clone,
    Default,
    // derive_more::Into,
    // derive_more::From,
    // derive_more::Deref,
    derive_more::Display,
)]
pub struct ScrCLikeAst<A>(pub A);
pub trait ScrOp<C, A> {
    fn op(&self, ctx: &mut C, args: impl Iterator<Item = A>) -> A;
}

impl<C, A, X: ScrOp<C, A>, Y: ScrOp<C, A>> ScrOp<C, A> for Either<X, Y> {
    fn op(&self, ctx: &mut C, args: impl Iterator<Item = A>) -> A {
        match self {
            Either::Left(a) => a.op(ctx, args),
            Either::Right(b) => b.op(ctx, args),
        }
    }
}
impl<B: Bound, C, A> ScrOp<C, A> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: ScrOp<C, A>,
{
    fn op(&self, ctx: &mut C, args: impl Iterator<Item = A>) -> A {
        return self.0.op(ctx, args);
    }
}

impl<
        C,
        O: ScrOp<C, A>,
        T,
        Y,
        S,
        W: ExportTerm<ScrCLikeAst<A>, C, O, T, Y, S>,
        A: Script + CLike,
    > ExportTerm<ScrCLikeAst<A>, C, O, T, Y, S> for If<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, Id<rat_ir::Block<O, T, Y, S>>) -> ScrCLikeAst<A>,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ScrCLikeAst<A>,
    ) -> anyhow::Result<ScrCLikeAst<A>> {
        let ssa = self.val.value.ssa::<C, O, T, Y, S, ScrCLikeAst<A>>();
        let t = self.then.go(ctx, &mut s, f, ScrCLikeAst(A::default()))?.0;
        let e = match self.r#else.as_ref() {
            None => None,
            Some(x) => Some(x.go(ctx, s, f, ScrCLikeAst(A::default()))?.0),
        };
        let e = match e {
            Some(a) => a.to_string(),
            None => format!(""),
        };
        Ok(ScrCLikeAst(
            format!("{};if({ssa} != 0){{{t}}}else{{{e}}}", body.0).into(),
        ))
    }
}
impl<C, O: ScrOp<C, A>, T, Y, S, A: Script + CLike> ExportAst<C, O, T, S, Y> for ScrCLikeAst<A> {
    type Var = A;

    fn get(ctx: &mut C, var: Self::Var, y: &Y) -> Self {
        Self(var)
    }

    fn assign(&self, ctx: &mut C, var: Self::Var, y: &Y) -> Self {
        Self(format!("{var} = {}", self.0).into())
    }

    fn select(&self, ctx: &mut C, s: &S) -> Self {
        self.clone()
    }

    fn append(&mut self, ctx: &mut C, i: impl Iterator<Item = Self>) {
        let s = once(std::mem::replace(self, Self("".to_owned().into())))
            .chain(i)
            .map(|a| a.0.to_string())
            .collect::<Vec<_>>();
        let s = s.join(";");
        *self = Self(s.into())
    }

    fn unit(ctx: &mut C) -> Self {
        Self(format!("0").into())
    }

    fn op(ctx: &mut C, o: &O, args: &[Self]) -> Self {
        Self(o.op(ctx, args.iter().map(|x| x.0.clone())))
    }
}
impl<C, O: ScrOp<C, A>, T, Y, S, A: Script + CLike> CffAst<C, O, T, S, Y> for ScrCLikeAst<A> {
    fn br_id(ctx: &mut C, a: usize) -> Self {
        let mut s: Self = <Self as CffAst<C, O, T, S, Y>>::set_id(ctx, a);
        let l = once(<Self as ReloopAst<C, O, T, S, Y>>::r#break(ctx, u16::MAX));
        <Self as ExportAst<C, O, T, S, Y>>::append(&mut s, ctx, l);
        return s;
    }
    fn set_id(ctx: &mut C, a: usize) -> Self {
        Self(format!("$id = {a}").into())
    }
    fn switch(ctx: &mut C, m: &std::collections::BTreeMap<usize, Self>) -> Self {
        Self(
            format!(
                "switch($id){{{}}}",
                m.iter()
                    .map(|(a, b)| format!("case {a}: {}", b.0))
                    .collect::<Vec<_>>()
                    .join(";break;")
            )
            .into(),
        )
    }

    fn forever(&self, ctx: &mut C) -> Self {
        Self(format!("while(1){{{}}}", self.0).into())
    }
}
impl<C, O: ScrOp<C, A>, T, Y, S, A: Script + CLike> ReloopAst<C, O, T, S, Y> for ScrCLikeAst<A> {
    fn r#break(ctx: &mut C, id: u16) -> Self {
        Self(A::r#break(id))
    }

    fn r#continue(ctx: &mut C, id: u16) -> Self {
        Self(A::r#continue(id))
    }

    fn r#loop(&self, ctx: &mut C, id: u16) -> Self {
        Self(self.0.r#loop(id))
    }
}
pub trait CLike {
    fn r#break(id: u16) -> Self;

    fn r#continue(id: u16) -> Self;
    fn r#loop(&self, id: u16) -> Self;
}
pub trait CEmit<T: CLike> {
    fn cemit(&self) -> T;
}
