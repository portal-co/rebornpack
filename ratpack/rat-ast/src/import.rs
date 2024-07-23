use either::Either;
use id_arena::Id;
use rat_ir::{
    Block, Bound, BoundOp, BoundTerm, BoundType, BuildFn, Builder, Func, Then, Unit, Value,
};

pub trait VarBuilder<O, T, Y, S, V> {
    type Result;
    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [V],
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
pub fn build_fn<
    O,
    T,
    Y,
    S,
    V,
    F: FnOnce(
        &mut Func<O, T, Y, S>,
        Id<Block<O, T, Y, S>>,
        &mut [V],
    ) -> anyhow::Result<(R, Id<Block<O, T, Y, S>>)>,
    R,
>(
    a: F,
) -> BuildFn<F> {
    return BuildFn { fun: a };
}
impl<
        O,
        T,
        Y,
        S,
        V,
        F: FnOnce(
            &mut Func<O, T, Y, S>,
            Id<Block<O, T, Y, S>>,
            &mut [V],
        ) -> anyhow::Result<(R, Id<Block<O, T, Y, S>>)>,
        R,
    > VarBuilder<O, T, Y, S, V> for BuildFn<F>
{
    type Result = R;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        (self.fun)(func, root, vars)
    }
}
#[derive(Clone)]
pub struct Wrap<A>(pub Box<A>);
impl<O, T, S, Y, V, A: Builder<O, T, S, Y>> VarBuilder<O, T, S, Y, V> for Wrap<A> {
    type Result = A::Result;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, S, Y>,
        root: Id<Block<O, T, S, Y>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
        return self.0.build(func, root);
    }
}

#[derive(Clone)]
pub struct Thread<A: ?Sized, V> {
    pub wrapped: Box<A>,
    pub vars: Vec<V>,
}
impl<O, T, S, Y, V, A: VarBuilder<O, T, S, Y, V> + ?Sized> Builder<O, T, S, Y> for Thread<A, V> {
    type Result = (A::Result, Vec<V>);

    fn build(
        mut self: Box<Self>,
        func: &mut Func<O, T, S, Y>,
        root: Id<Block<O, T, S, Y>>,
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
        let (w, k) = self.wrapped.build_with_vars(func, root, &mut self.vars)?;
        return Ok(((w, self.vars), k));
    }
}
#[derive(Clone, Copy)]
pub struct Get<T> {
    pub v: T,
}
impl<O, T, Y, S, V: Clone> VarBuilder<O, T, Y, S, V> for Get<usize> {
    type Result = V;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        return Ok((vars[self.v].clone(), root));
    }
}

#[derive(Clone)]
pub struct Set<A: ?Sized, T> {
    pub v: T,
    pub wrapped: Box<A>,
}

impl<O, T, S, Y, V: Clone, A: VarBuilder<O, T, S, Y, V, Result = (V, X)> + ?Sized, X>
    VarBuilder<O, T, S, Y, V> for Set<A, usize>
{
    type Result = (V, X);

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, S, Y>,
        root: Id<Block<O, T, S, Y>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
        let ((v, x), root) = self.wrapped.build_with_vars(func, root, vars)?;
        vars[self.v] = v.clone();
        return Ok(((v, x), root));
    }
}
#[derive(Clone)]
pub struct SetDirect<A: ?Sized, T> {
    pub v: T,
    pub wrapped: Box<A>,
}
impl<O, T, S, Y, V: Clone, A: VarBuilder<O, T, S, Y, V, Result = V> + ?Sized>
    VarBuilder<O, T, S, Y, V> for SetDirect<A, usize>
{
    type Result = V;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, S, Y>,
        root: Id<Block<O, T, S, Y>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
        let (v, root) = self.wrapped.build_with_vars(func, root, vars)?;
        vars[self.v] = v.clone();
        return Ok((v, root));
    }
}
impl<
        O,
        T,
        Y,
        S,
        V,
        A: VarBuilder<O, T, Y, S, V> + ?Sized,
        B: FnOnce(A::Result) -> C,
        C: VarBuilder<O, T, Y, S, V>,
    > VarBuilder<O, T, Y, S, V> for Then<A, B>
{
    type Result = C::Result;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let (a, root) = self.first.build_with_vars(func, root, vars)?;
        return Box::new((self.then)(a)).build_with_vars(func, root, vars);
    }
}
impl<O, T, S, Y, V, A> VarBuilder<O, T, S, Y, V> for Unit<A> {
    type Result = A;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, S, Y>,
        root: Id<Block<O, T, S, Y>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
        Ok(((*self).0, root))
    }
}

impl<
        O,
        T,
        Y,
        S,
        V,
        A: VarBuilder<O, T, Y, S, V, Result = R>,
        B: VarBuilder<O, T, Y, S, V, Result = R>,
        R,
    > VarBuilder<O, T, Y, S, V> for Either<A, B>
{
    type Result = R;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [V],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        match *self {
            Either::Left(a) => Box::new(a).build_with_vars(func, root, vars),
            Either::Right(b) => Box::new(b).build_with_vars(func, root, vars),
        }
    }
}
