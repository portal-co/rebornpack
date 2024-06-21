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
pub trait ScrOp<A> {
    fn op(&self, args: impl Iterator<Item = A>) -> A;
}

impl<A, X: ScrOp<A>, Y: ScrOp<A>> ScrOp<A> for Either<X, Y> {
    fn op(&self, args: impl Iterator<Item = A>) -> A {
        match self {
            Either::Left(a) => a.op(args),
            Either::Right(b) => b.op(args),
        }
    }
}
impl<B: Bound, A> ScrOp<A> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: ScrOp<A>,
{
    fn op(&self, args: impl Iterator<Item = A>) -> A {
        return self.0.op(args);
    }
}

impl<O: ScrOp<A>, T, Y, S, W: ExportTerm<ScrCLikeAst<A>, O, T, Y, S>, A: Script + CLike>
    ExportTerm<ScrCLikeAst<A>, O, T, Y, S> for If<O, T, Y, S, W>
{
    fn go(
        &self,
        mut s: impl FnMut(Id<rat_ir::Block<O, T, Y, S>>) -> ScrCLikeAst<A>,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ScrCLikeAst<A>,
    ) -> anyhow::Result<ScrCLikeAst<A>> {
        let ssa = self.val.value.ssa::<O, T, Y, S, ScrCLikeAst<A>>();
        let t = self.then.go(&mut s, f, ScrCLikeAst(A::default()))?.0;
        let e = match self.r#else.as_ref() {
            None => None,
            Some(x) => Some(x.go(s, f, ScrCLikeAst(A::default()))?.0),
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
impl<O: ScrOp<A>, T, Y, S, A: Script + CLike> ExportAst<O, T, S, Y> for ScrCLikeAst<A> {
    type Var = A;

    fn get(var: Self::Var, y: &Y) -> Self {
        Self(var)
    }

    fn assign(&self, var: Self::Var, y: &Y) -> Self {
        Self(format!("{var} = {}", self.0).into())
    }

    fn select(&self, s: &S) -> Self {
        self.clone()
    }

    fn append(&mut self, i: impl Iterator<Item = Self>) {
        let s = once(std::mem::replace(self, Self("".to_owned().into())))
            .chain(i)
            .map(|a| a.0.to_string())
            .collect::<Vec<_>>();
        let s = s.join(";");
        *self = Self(s.into())
    }

    fn unit() -> Self {
        Self(format!("0").into())
    }

    fn op(o: &O, args: &[Self]) -> Self {
        Self(o.op(args.iter().map(|x| x.0.clone())))
    }
}
impl<O: ScrOp<A>, T, Y, S, A: Script + CLike> CffAst<O, T, S, Y> for ScrCLikeAst<A> {
    fn br_id(a: usize) -> Self {
        let mut s: Self = <Self as CffAst<O, T, S, Y>>::set_id(a);
        <Self as ExportAst<O, T, S, Y>>::append(
            &mut s,
            once(<Self as ReloopAst<O, T, S, Y>>::r#break(u16::MAX)),
        );
        return s;
    }
    fn set_id(a: usize) -> Self {
        Self(format!("$id = {a}").into())
    }
    fn switch(m: &std::collections::BTreeMap<usize, Self>) -> Self {
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

    fn forever(&self) -> Self {
        Self(format!("while(1){{{}}}", self.0).into())
    }
}
impl<O: ScrOp<A>, T, Y, S, A: Script + CLike> ReloopAst<O, T, S, Y> for ScrCLikeAst<A> {
    fn r#break(id: u16) -> Self {
        Self(A::r#break(id))
    }

    fn r#continue(id: u16) -> Self {
        Self(A::r#continue(id))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(self.0.r#loop(id))
    }
}
pub trait CLike {
    fn r#break(id: u16) -> Self;

    fn r#continue(id: u16) -> Self;
    fn r#loop(&self, id: u16) -> Self;
}
pub trait CEmit<T: CLike>{
    fn cemit(&self) -> T;
}