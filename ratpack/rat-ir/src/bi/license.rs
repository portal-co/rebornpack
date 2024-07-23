use std::{collections::BTreeMap, marker::PhantomData, sync::Arc};

use anyhow::Context;
use either::Either;
use id_arena::Id;
use rat_debug::Span;

use crate::{
    no_push,
    transform::NormalTerm,
    util::{Bool, Extract, ExtractIn, If, Push},
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator,
    Use, Value,
};

use super::{NormalTermBi, Tracer};
#[repr(transparent)]
pub struct License<C> {
    pub ctx: C,
}
no_push!(
    type License<C>;
);
pub trait Taint {
    fn no_license(&mut self);
}
impl<A: Taint, B: Taint> Taint for Either<A, B> {
    fn no_license(&mut self) {
        match self {
            Either::Left(a) => a.no_license(),
            Either::Right(b) => b.no_license(),
        }
    }
}
impl<B: Bound> Taint for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Taint,
{
    fn no_license(&mut self) {
        self.0.no_license()
    }
}
impl<
        C: Tracer<O, T, Y, S, O2, T2, Y2, S2>,
        O: Taint + Clone,
        T,
        Y,
        S,
        O2: Push<License<()>>,
        T2: Push<If<O2, T2, Y2, S2, BlockTarget<O2, T2, Y2, S2>>>,
        Y2: Clone + Ord + Push<Bool>,
        S2: Default,
    > Tracer<O, T, Y, S, O2, T2, Y2, S2> for License<C>
{
    type Instance = (Arc<C::Instance>, bool);

    type Meta = C::Meta;

    fn meta_param(
        &mut self,
        i: &Self::Instance,
        idx: usize,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: Id<crate::Block<O2, T2, Y2, S2>>,
    ) -> anyhow::Result<(Self::Meta, Id<crate::Block<O2, T2, Y2, S2>>)> {
        self.ctx.meta_param(i.0.as_ref(), idx, y, new, k)
    }

    fn select(
        &mut self,
        i: &Self::Instance,
        m: &Self::Meta,
        s: &S,
        y: &Y,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: Id<crate::Block<O2, T2, Y2, S2>>,
        sp: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, Id<crate::Block<O2, T2, Y2, S2>>)> {
        self.ctx.select(i.0.as_ref(), m, s, y, new, k, sp)
    }

    fn op(
        &mut self,
        i: &Self::Instance,
        o: &O,
        y: &Y,
        args: &[Self::Meta],
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: Id<crate::Block<O2, T2, Y2, S2>>,
        sp: Option<Span>,
    ) -> anyhow::Result<(Self::Meta, Id<crate::Block<O2, T2, Y2, S2>>)> {
        let mut o = o.clone();
        if !i.1 {
            o.no_license();
        }
        self.ctx.op(i.0.as_ref(), &o, y, args, new, k, sp)
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
        ) -> anyhow::Result<Id<crate::Block<O2, T2, Y2, S2>>>,
        valmap: &std::collections::BTreeMap<
            Id<crate::Value<O, T, Y, S>>,
            std::sync::Arc<Self::Meta>,
        >,
        new: &mut crate::Func<O2, T2, Y2, S2>,
        k: Id<crate::Block<O2, T2, Y2, S2>>,
        // old: &Func<O, T, Y, S>,
        span: Option<Span>,
    ) -> anyhow::Result<()> {
        let st2 = unsafe {
            std::mem::transmute::<_, &mut super::State<O, T, Y, S, O2, T2, Y2, S2, C>>(state)
        };
        return C::term(
            st2,
            i.0.as_ref(),
            t,
            |s, new, k, i2| {
                let state = unsafe {
                    std::mem::transmute::<_, &mut super::State<O, T, Y, S, O2, T2, Y2, S2, Self>>(s)
                };
                let i2 = Arc::new(i2);
                let b: Vec<_> = [true, false]
                    .into_iter()
                    .map(|a| a && i.1)
                    .map(|b| go(state, new, k, (i2.clone(), b)))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                let b0 = b[0];
                let b1 = b[1];
                let v = new.opts.alloc(Value::Operator(
                    O2::push(License { ctx: () })
                        .map_right(|_| ())
                        .unwrap_left(),
                    vec![],
                    Y2::push(Bool {}).map_right(|_| ()).unwrap_left(),
                    PhantomData,
                ));
                let k = new.blocks.alloc(Block {
                    term_span: span.clone(),
                    insts: vec![v],
                    term: Some(T2::push(If {
                        val: Use {
                            value: v,
                            select: Default::default(),
                        },
                        then: BlockTarget {
                            block: b0,
                            args: vec![],
                            prepend: vec![],
                        },
                        r#else: Some(BlockTarget {
                            block: b1,
                            args: vec![],
                            prepend: vec![],
                        }),
                    })
                    .map_right(|_| ())
                    .unwrap_left()),
                    params: vec![],
                });
                Ok(k)
            },
            valmap,
            new,
            k,
            // old,
            span.clone(),
        );
        // let b: Vec<_> = [true, false]
        //     .into_iter()
        //     .map(|a| a && *i)
        //     .map(|b| {
        //         let m = t
        //             .t2s()
        //             .map(|a| Ok((a.block, go(state, new, a.block, b)?)))
        //             .collect::<anyhow::Result<BTreeMap<_, _>>>()?;
        //         let n = t.norm(&m, &valmap.iter().map(|(a, b)| (*a, **b)).collect());
        //         let nb = new.blocks.alloc(Block {
        //             insts: vec![],
        //             term: n,
        //             params: vec![],
        //         });
        //         Ok(nb)
        //     })
        //     .collect::<anyhow::Result<Vec<_>>>()?;
        // let b0 = b[0];
        // let b1 = b[1];
        // let v = new.opts.alloc(Value::Operator(
        //     O2::push(License { ctx: () })
        //         .map_right(|_| ())
        //         .unwrap_left(),
        //     vec![],
        //     Y2::push(Bool {}).map_right(|_| ()).unwrap_left(),
        //     PhantomData,
        // ));
        // new.blocks[k].insts.push(v);
        // new.blocks[k].term = T2::push(If {
        //     val: Use {
        //         value: v,
        //         select: Default::default(),
        //     },
        //     then: BlockTarget {
        //         block: b0,
        //         args: vec![],
        //         prepend: vec![],
        //     },
        //     r#else: Some(BlockTarget {
        //         block: b1,
        //         args: vec![],
        //         prepend: vec![],
        //     }),
        // })
        // .map_right(|_| ())
        // .unwrap_left();
        // Ok(())
    }
}
