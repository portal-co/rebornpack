use either::Either;
use id_arena::Id;
use rat_ir::{
    module::{Module, TailCall},
    util::If,
    Block, BlockTarget, Bound, BoundExt, BoundOp, BoundSelect, BoundTerm, BoundType, Call, Func,
    Value,
};

use crate::{abs, app, var, Binder, GTerm};
pub trait Thing<O, T, Y, S, D, V: Binder, M> {
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>>;
}
impl<O, T, Y, S, D, V: Binder, M, A: Thing<O, T, Y, S, D, V, M>, B: Thing<O, T, Y, S, D, V, M>>
    Thing<O, T, Y, S, D, V, M> for Either<A, B>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        match self {
            Either::Left(a) => a.go(m, f, k),
            Either::Right(b) => b.go(m, f, k),
        }
    }
}
impl<D, V: Binder, M, B: Bound>
    Thing<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D, V, M> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        Thing<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D, V, M>,
{
    fn go(
        &self,
        m: &Module<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D>,
        f: Id<Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
        k: Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        self.0.go(m, f, k)
    }
}
impl<D, V: Binder, M, B: Bound>
    Thing<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D, V, M> for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        Thing<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D, V, M>,
{
    fn go(
        &self,
        m: &Module<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, D>,
        f: Id<Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
        k: Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        self.0.go(m, f, k)
    }
}
pub fn vars<
    'a,
    O,
    T,
    Y,
    S,
    D,
    V: Binder<Var = V>
        + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
        + From<Id<Value<O, T, Y, S>>>
        + From<String>
        + 'a,
>(
    m: &'a Module<O, T, Y, S, D>,
) -> impl DoubleEndedIterator<Item = V> + 'a {
    return m
        .funcs
        .iter()
        .flat_map(|x| x.1.blocks.iter().map(move |k| (x.0, k.0)))
        .map(V::from);
}
pub fn push<
    O: Thing<O, T, Y, S, D, V, M>,
    T: Thing<O, T, Y, S, D, V, M>,
    Y,
    S,
    D,
    V: Binder<Var = V>
        + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
        + From<Id<Value<O, T, Y, S>>>
        + From<String>,
    M,
>(
    m: &Module<O, T, Y, S, D>,
    f: Id<Func<O, T, Y, S>>,
    k: Id<Block<O, T, Y, S>>,
) -> anyhow::Result<GTerm<V, M>> {
    let mut t = m.funcs[f].blocks[k].term.go(m, f, k)?;
    for v in m.funcs[f].blocks[k].insts.iter() {
        let u = match &m.funcs[f].opts[*v] {
            Value::Operator(o, r, _, _) => {
                let mut u = o.go(m, f, k)?;
                for us in r.iter() {
                    u = app(u, var(V::from(us.value)));
                }
                u
            }
            Value::BlockParam(i, _, _) => var(V::from(format!("param{i}"))),
            Value::Alias(u, _) => var(V::from(u.value)),
        };
        t = app(u, abs(V::from(*v), t));
    }
    t = abs(V::from(format!("return")), t);
    for (i, _) in m.funcs[f].blocks[k].params.iter().enumerate().rev() {
        t = abs(V::from(format!("param{i}")), t);
    }
    for v in vars(m).rev() {
        t = abs(v, t);
    }
    return Ok(t);
}
impl<
        O,
        T,
        Y,
        S,
        D,
        V: Binder<Var = V>
            + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
            + From<Id<Value<O, T, Y, S>>>
            + From<String>,
        M,
    > Thing<O, T, Y, S, D, V, M> for BlockTarget<O, T, Y, S>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        let mut t = var(V::from((f, self.block)));
        for v in vars(m) {
            t = app(t, var(v));
        }
        for (p, _) in self.prepend.iter().enumerate() {
            let p: V = format!("prepend{p}").into();
            t = app(t, var(p))
        }
        for r in self.args.iter() {
            t = app(t, var(V::from(r.value)));
        }
        t = app(t, var(V::from(format!("return"))));
        for (p, _) in self.prepend.iter().enumerate().rev() {
            let p: V = format!("prepend{p}").into();
            t = abs(p, t);
        }
        return Ok(t);
    }
}
impl<
        O,
        T,
        Y,
        S,
        D,
        V: Binder<Var = V>
            + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
            + From<Id<Value<O, T, Y, S>>>
            + From<String>,
        M,
        W: Thing<O, T, Y, S, D, V, M>,
    > Thing<O, T, Y, S, D, V, M> for If<O, T, Y, S, W>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        let then = self.then.go(m, f, k)?;
        let els = self.r#else.as_ref().unwrap().go(m, f, k)?;
        let v = var(V::from(self.val.value));
        return Ok(app(app(v, then), els));
    }
}
impl<
        O,
        T,
        Y,
        S,
        D,
        V: Binder<Var = V>
            + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
            + From<Id<Value<O, T, Y, S>>>
            + From<String>,
        M,
    > Thing<O, T, Y, S, D, V, M> for TailCall<O, T, Y, S>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        let mut t = var(V::from((self.func, m.funcs[self.func].entry)));
        for v in vars(m) {
            t = app(t, var(v));
        }

        for r in self.params.iter() {
            t = app(t, var(V::from(r.value)));
        }
        t = app(t, var(V::from(format!("return"))));
        return Ok(t);
    }
}
impl<
        O,
        T,
        Y,
        S,
        D,
        V: Binder<Var = V>
            + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
            + From<Id<Value<O, T, Y, S>>>
            + From<String>,
        M,
    > Thing<O, T, Y, S, D, V, M> for Call<O, T, Y, S>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        let mut t = var(V::from((self.func, m.funcs[self.func].entry)));
        for v in vars(m) {
            t = app(t, var(v));
        }

        return Ok(t);
    }
}
// impl<
//         O,
//         T,
//         Y,
//         S,
//         D,
//         V: Binder<Var = V>
//             + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
//             + From<Id<Value<O, T, Y, S>>>
//             + From<String>,
//         M,
//     > Thing<O, T, Y, S, D, V, M> for crate::rat::export::Call<O, T, Y, S>
// {
//     fn go(
//         &self,
//         m: &Module<O, T, Y, S, D>,
//         f: Id<Func<O, T, Y, S>>,
//         k: Id<Block<O, T, Y, S>>,
//     ) -> anyhow::Result<GTerm<V, M>> {
//         match self {
//             super::export::Call::Func(f) => {
//                 let mut t = var(V::from((*f, m.funcs[*f].entry)));
//                 for v in vars(m) {
//                     t = app(t, var(v));
//                 }

//                 return Ok(t);
//             }
//             super::export::Call::Var => Ok(abs(V::from(format!("$")), var(V::from(format!("$"))))),
//         }
//     }
// }
impl<
        O,
        T,
        Y,
        S,
        D,
        V: Binder<Var = V>
            + From<(Id<Func<O, T, Y, S>>, Id<Block<O, T, Y, S>>)>
            + From<Id<Value<O, T, Y, S>>>
            + From<String>,
        M,
    > Thing<O, T, Y, S, D, V, M> for crate::rat::export::Ret<O, T, Y, S>
{
    fn go(
        &self,
        m: &Module<O, T, Y, S, D>,
        f: Id<Func<O, T, Y, S>>,
        k: Id<Block<O, T, Y, S>>,
    ) -> anyhow::Result<GTerm<V, M>> {
        return Ok(app(var(V::from(format!("return"))), var(V::from(self.val))));
    }
}
