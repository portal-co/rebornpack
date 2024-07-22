use std::collections::BTreeMap;

use either::Either;
use id_arena::Id;
use rat_debug::Span;

use crate::{
    build_fn,
    transform::{ctx::NormalTermIn, NormalTerm},
    BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Builder, SaneTerminator, Value,
};

use super::Tracer;

pub struct DoCopy {}

// impl<O: Clone,Y: crate::util::Extract<Y2>,O2: crate::util::Push<O>,T2,Y2,S2: Default> SimpleOp<O,Y,O2,T2,Y2,S2> for DoCopy{
//     fn build(
//         &mut self,
//         o: &O,
//         y: &Y,
//         args: &[Id<Value<O2, T2, Y2, S2>>],
//         span: Option<Span>,
//     ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
//         crate::build_fn(move|new,k|{
//             let v = new.opts.alloc(Value::Operator(
//                 O2::push(o.clone()).map_right(|_|()).unwrap_left(),
//                 args.iter()
//                     .map(|a| crate::Use {
//                         value: *a,
//                         select: S2::default(),
//                     })
//                     .collect(),
//                 y.extract(),
//                 ::std::marker::PhantomData,
//             ));
//             new.blocks[k].insts.push(v);
//             new.spans[v] = span;
//             Ok((v,k))
//         })
//     }
// }

pub trait SimpleOp<O, Y, O2, T2, Y2, S2> {
    fn build(
        &mut self,
        o: &O,
        y: &Y,
        args: &[Id<Value<O2, T2, Y2, S2>>],
        span: Option<Span>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>>;
}
impl<
        B: Bound,
        O2,
        T2,
        Y2,
        S2,
        // A: SimpleOp<
        //         B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        //         B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        //         O2,
        //         T2,
        //         Y2,
        //         S2,
        //     > + Sized
        //     + 'static,
        G: Gm<
            First: SimpleOp<
                B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
                B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
                O2,
                T2,
                Y2,
                S2,
            > + Sized,
        >,
    > SimpleOp<BoundOp<B>, BoundType<B>, O2, T2, Y2, S2> for G
{
    fn build(
        &mut self,
        o: &BoundOp<B>,
        y: &BoundType<B>,
        args: &[Id<Value<O2, T2, Y2, S2>>],
        span: Option<Span>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
        self.first().build(&o.0, &y.0, args, span)
    }
}
pub trait Gm {
    type First;
    type Second;
    fn first<'a>(&'a mut self) -> &'a mut Self::First
    where
        Self::First: 'a,
        Self: 'a;
    fn second<'a>(&'a mut self) -> &'a mut Self::Second
    where
        Self::Second: 'a,
        Self: 'a;
}
macro_rules! gm_impl {
    (type $ty:ident <$($param:tt),*>;) => {
        impl<$($param),*> $crate::bi::simple::Gm for $ty<$($param),*>{
            type First = Self;

            type Second = Self;

            fn first<'a>(&'a mut self) -> &'a mut Self::First where Self::First: 'a{
                self
            }

            fn second<'a>(&'a mut self) -> &'a mut Self::Second where Self::Second: 'a{
                self
            }
        }
    };
    (type $ty:ident;) => {
        impl $crate::bi::simple::Gm for $ty{
            type First = Self;

            type Second = Self;

            fn first<'a>(&'a mut self) -> &'a mut Self::First where Self::First: 'a{
                self
            }

            fn second<'a>(&'a mut self) -> &'a mut Self::Second where Self::Second: 'a {
                self
            }
        }
    }
}
type _Unit = ();
gm_impl!(
    type _Unit;
);
gm_impl!(
    type DoCopy;
);
mod _test {
    use super::*;
    use crate::{build_fn, safe, transform::NormalTerm, Builder, SaneTerminator, Value};
    #[derive(Clone)]
    struct _Test {}
    gm_impl!(
        type _Test;
    );
    // impl<Y, O2, T2, Y2, S2> SimpleOp<_Test, Y, O2, T2, Y2, S2> for _Test {
    //     fn build(
    //         &mut self,
    //         o: &_Test,
    //         y: &Y,
    //         args: &[Id<Value<O2, T2, Y2, S2>>],
    //     ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
    //         build_fn(|f, _| unreachable!())
    //     }
    // }
    safe!(_Test from _Test);
}
impl<
        O,
        Ox,
        Y,
        O2,
        T2,
        Y2,
        S2,
        // A: SimpleOp<O, Y, O2, T2, Y2, S2>,
        // B: SimpleOp<Ox, Y, O2, T2, Y2, S2> + 'static,
        T: Gm<First: SimpleOp<O, Y, O2, T2, Y2, S2>, Second: SimpleOp<Ox, Y, O2, T2, Y2, S2>>,
    > SimpleOp<Either<O, Ox>, Y, O2, T2, Y2, S2> for T
{
    fn build(
        &mut self,
        o: &Either<O, Ox>,
        y: &Y,
        args: &[Id<Value<O2, T2, Y2, S2>>],
        span: Option<Span>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
        match o {
            Either::Left(a) => Either::Left(self.first().build(a, y, args, span)),
            Either::Right(a) => Either::Right(self.second().build(a, y, args, span)),
        }
    }
}
#[macro_export]
macro_rules! safe {
    ($a:ident<$($ap:tt),*> from $b:ident<$($bp:tt),*>) => {
        impl<$($ap),*,$($bp),*,Y: Clone,O2: $crate::util::Push<$b<$(bp),*>>,T2,Y2,S2: Default> SimpleOp<$b<$(bp),*>,Y,O2,T2,Y2: $crate::util::Push<Y>,S2> for $a<$($ap),*> where $b<$(bp),*>: Clone{
            fn build(
                &mut self,
                o: &$b<$(bp),*>,
                y: &Y,
                args: &[Id<Value<O2, T2, Y2, S2>>],
            ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|new,k|{
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(o.clone()).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                            Y2::push(y.clone()).map_right(|_|()).unwrap_left(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    Ok((v,k))
                })
            }
        }
    };
    ($a:ident from $b:ident<$($bp:tt),*>) => {
        impl<$($bp),*,Y: Clone,O2: $crate::util::Push<$b<$(bp),*>>,T2,Y2: $crate::util::Push<Y>,S2: Default> SimpleOp<$b<$(bp),*>,Y,O2,T2,Y2,S2> for $a where $b<$(bp),*>: Clone{
            fn build(
                &mut self,
                o: &$b<$(bp),*>,
                y: &Y,
                args: &[Id<Value<O2, T2, Y2, S2>>],
            ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|new,k|{
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(o.clone()).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                            Y2::push(y.clone()).map_right(|_|()).unwrap_left(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    Ok((v,k))
                })
            }
        }
    };
    ($a:ident<$($ap:tt),*> from $b:ident) => {
        impl<$($ap),*,Y: Clone,O2: $crate::util::Push<$b>,T2,Y2: $crate::util::Push<Y>,S2: Default> SimpleOp<$b,Y,O2,T2,Y2,S2> for $a<$($ap),*> where $b: Clone{
            fn build(
                &mut self,
                o: &$b,
                y: &Y,
                args: &[Id<Value<O2, T2, Y2, S2>>],
                span: Option<Span>,
            ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|new,k|{
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(o.clone()).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                            Y2::push(y.clone()).map_right(|_|()).unwrap_left(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    new.spans[v] = span;
                    Ok((v,k))
                })
            }
        }
    };
    ($a:ident from $b:ident) => {
        impl<Y: Clone,O2: $crate::util::Push<$b>,T2,Y2: $crate::util::Push<Y>,S2: Default> SimpleOp<$b,Y,O2,T2,Y2,S2> for $a where $b: Clone{
            fn build(
                &mut self,
                o: &$b,
                y: &Y,
                args: &[Id<Value<O2, T2, Y2, S2>>],
                span: Option<Span>,
            ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|new,k|{
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(o.clone()).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                        Y2::push(y.clone()).map_right(|_|()).unwrap_left(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    new.spans[v] = span;
                    Ok((v,k))
                })
            }
        }
    };
}
pub trait SimpleSelect<Y, S, O2, T2, Y2, S2> {
    fn build_select<'a>(
        &'a mut self,
        s: &'a S,
        y: &'a Y,
        arg: &'a Id<Value<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> + 'a;
}
impl<
        B: Bound,
        O2,
        T2,
        Y2,
        S2,
        G: Gm<
            First: SimpleSelect<
                B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
                B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
                O2,
                T2,
                Y2,
                S2,
            > + Sized,
        >,
    > SimpleSelect<BoundType<B>, BoundSelect<B>, O2, T2, Y2, S2> for G
{
    fn build_select<'a>(
        &'a mut self,
        s: &'a BoundSelect<B>,
        y: &'a BoundType<B>,
        arg: &'a Id<Value<O2, T2, Y2, S2>>,
        span: Option<Span>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> + 'a {
        let b = self.first().build_select(s, y, arg, span);
        let b: Box<dyn Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> + '_> =
            Box::new(b);
        return b;
    }
}
pub trait SimpleParam<Y, O2, T2, Y2, S2> {
    fn param(&mut self, y: &Y) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>>;
}

impl<
        B: Bound,
        O2,
        T2,
        Y2,
        S2,
        G: Gm<
            First: SimpleParam<
                B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
                O2,
                T2,
                Y2,
                S2,
            > + Sized,
        >,
    > SimpleParam<BoundType<B>, O2, T2, Y2, S2> for G
{
    fn param(
        &mut self,
        y: &BoundType<B>,
    ) -> impl Builder<O2, T2, Y2, S2, Result = Id<Value<O2, T2, Y2, S2>>> {
        self.first().param(y)
    }
}
pub trait SimpleTerm<O2, T2, Y2, S2, T2B> {
    fn warp(&mut self, t: T2B) -> impl Builder<O2, T2, Y2, S2, Result = T2>;
}

impl<
        O2,
        T2,
        Y2,
        S2,
        Va,
        Vb,
        X: Gm<First: SimpleTerm<O2, T2, Y2, S2, Va>, Second: SimpleTerm<O2, T2, Y2, S2, Vb>>,
    > SimpleTerm<O2, T2, Y2, S2, Either<Va, Vb>> for X
{
    fn warp(&mut self, x: Either<Va, Vb>) -> impl Builder<O2, T2, Y2, S2, Result = T2> {
        match x {
            Either::Left(a) => Either::Left(self.first().warp(a)),
            Either::Right(b) => Either::Right(self.second().warp(b))
        }
    }
}


pub trait Simple<O, T, Y, S, O2, T2, Y2, S2, T2B>:
    SimpleOp<O, Y, O2, T2, Y2, S2>
    + SimpleSelect<Y, S, O2, T2, Y2, S2>
    + SimpleParam<Y, O2, T2, Y2, S2>
    + SimpleTerm<O2, T2, Y2, S2, T2B>
{
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
        T2B,
        A: SimpleOp<O, Y, O2, T2, Y2, S2>
            + SimpleSelect<Y, S, O2, T2, Y2, S2>
            + SimpleParam<Y, O2, T2, Y2, S2>
            + SimpleTerm<O2, T2, Y2, S2, T2B>,
    > Simple<O, T, Y, S, O2, T2, Y2, S2, T2B> for A
{
}
#[repr(transparent)]
pub struct DoSimple<X> {
    pub wrapped: X,
}
impl<Y, X: AsMut<Y>> AsMut<Y> for DoSimple<X> {
    fn as_mut(&mut self) -> &mut Y {
        return self.wrapped.as_mut();
    }
}

impl<
        O,
        T: SaneTerminator<O, T, Y, S> + NormalTermIn<X, O, T, Y, S, O2, T2, Y2, S2, Then = T2B>,
        Y,
        S,
        O2,
        T2,
        Y2,
        S2,
        X: Simple<O, T, Y, S, O2, T2, Y2, S2, T2B>,
        T2B,
    > Tracer<O, T, Y, S, O2, T2, Y2, S2> for DoSimple<X>
{
    type Instance = ();

    type Meta = Id<Value<O2, T2, Y2, S2>>;

    fn meta_param(
        &mut self,
        i: &Self::Instance,
        idx: usize,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, id_arena::Id<crate::Block<O2, T2, Y2, S2>>)> {
        Box::new(self.wrapped.param(y)).build(new, k)
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
        Box::new(self.wrapped.build_select(s, y, m, span)).build(new, k)
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
        Box::new(self.wrapped.build(o, y, args, span)).build(new, k)
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
        k: id_arena::Id<crate::Block<O2, T2, Y2, S2>>,
        // old: &crate::Func<O, T, Y, S>,
        span: Option<Span>,
    ) -> anyhow::Result<()> {
        let m = t
            .t2s()
            .map(|a| Ok((a.block, go(state, new, a, ())?)))
            .collect::<anyhow::Result<BTreeMap<_, _>>>()?;
        let n = t.norm(
            &mut state.tracer.wrapped,
            &m,
            &valmap.iter().map(|(a, b)| (*a, **b)).collect(),
        );
        let (n, k) = Box::new(state.tracer.wrapped.warp(n)).build(new, k)?;
        new.blocks[k].term = n;
        new.blocks[k].term_span = span;
        Ok(())
    }
}

