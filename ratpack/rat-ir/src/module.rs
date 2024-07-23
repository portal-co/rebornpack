use std::{collections::BTreeMap, marker::PhantomData};

use id_arena::{Arena, Id};
use serde::{Deserialize, Serialize};

use crate::{
    bi::{
        self,
        license::License,
        simple::{DoSimple, SimpleOp},
        State, Tracer,
    },
    transform::{ctx::NormalTermIn, NormalTerm},
    util::{Extract, ExtractIn, PerID, Push},
    Block, BlockTarget, Bound, Call, Func, Use, Value,
};
pub mod cps;
// pub mod bi;
#[derive(Serialize, Deserialize)]
pub struct Module<O, T, Y, S, D> {
    pub funcs: Arena<Func<O, T, Y, S>>,
    pub data: Arena<D>,
}
impl<O, T, S, Y, D> Default for Module<O, T, S, Y, D> {
    fn default() -> Self {
        Self {
            funcs: Default::default(),
            data: Default::default(),
        }
    }
}
pub trait BoundModule {
    type F<F: Bound, D>: Bound;
    type D<F: Bound, D>;
}
pub struct BoundData<B: BoundModule>(pub B::D<BoundFn<B>, BoundData<B>>);
pub struct BoundFn<B: BoundModule>(pub B::F<BoundFn<B>, BoundData<B>>);
macro_rules! bound_impls {
    ($ty:tt, $x:tt) => {
        impl<B: BoundModule> Clone for $ty<B>
        where
            B::$x<BoundFn<B>, BoundData<B>>: Clone,
        {
            fn clone(&self) -> Self {
                return Self(self.0.clone());
            }
        }
        impl<B: BoundModule> Default for $ty<B>
        where
            B::$x<BoundFn<B>, BoundData<B>>: Default,
        {
            fn default() -> Self {
                return Self(Default::default());
            }
        }
    };
}
bound_impls!(BoundData, D);
bound_impls!(BoundFn, F);
impl<B: BoundModule> Bound for BoundFn<B> {
    type O<O, T, Y, S> = <<B as BoundModule>::F<BoundFn<B>, BoundData<B>> as Bound>::O<O, T, Y, S>;

    type T<O, T, Y, S> = <<B as BoundModule>::F<BoundFn<B>, BoundData<B>> as Bound>::T<O, T, Y, S>;

    type Y<O, T, Y, S> = <<B as BoundModule>::F<BoundFn<B>, BoundData<B>> as Bound>::Y<O, T, Y, S>;

    type S<O, T, Y, S> = <<B as BoundModule>::F<BoundFn<B>, BoundData<B>> as Bound>::S<O, T, Y, S>;
}
pub trait HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2> {
    fn funcs(&self) -> &PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>>;
}
pub trait HasModuleData<D, D2> {
    fn data(&self) -> &PerID<D, Option<Id<D2>>>;
}
impl<D, D2, C: HasModuleData<D, D2>> HasModuleData<D, D2> for License<C> {
    fn data(&self) -> &PerID<D, Option<Id<D2>>> {
        self.ctx.data()
    }
}
impl<D, D2, C: HasModuleData<D, D2>> HasModuleData<D, D2> for DoSimple<C> {
    fn data(&self) -> &PerID<D, Option<Id<D2>>> {
        self.wrapped.data()
    }
}
impl<O, T, Y, S, O2, T2, Y2, S2, C: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>
    HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2> for License<C>
{
    fn funcs(&self) -> &PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>> {
        self.ctx.funcs()
    }
}
impl<O, T, Y, S, O2, T2, Y2, S2, C: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>
    HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2> for DoSimple<C>
{
    fn funcs(&self) -> &PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>> {
        self.wrapped.funcs()
    }
}
#[macro_export]
macro_rules! safe_call {
    (safe $x:ident<$($a:tt),*>;) => {
        impl<O, T, Y, S, O2: $crate::util::Push<$crate::Call<O2,T2,Y2,S2>>, T2, Y2, S2, $($a),*> SimpleOp<$crate::Call<O,T,Y,S>,Y,O2,T2,Y2,S2> for $x<$($a),*> where  $x<$($a),*>: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>{
            fn build(
                &mut self,
                o: &$crate::Call<O,T,Y,S>,
                y: &Y,
                args: &[Id<crate::Value<O2, T2, Y2, S2>>],
            ) -> impl $crate::Builder<O2, T2, Y2, S2, Result = Id<crate::Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|f,k|{
                    let n = o.func;
                    let n = self.funcs()[n].clone().unwrap();
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(Call{func: n}).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                        y.extract(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    Ok((v,k))
                })
            }
        }
    };
    (safe $x:ident) => {
        impl<O, T, Y, S, O2: $crate::util::Push<$crate::Call<O2,T2,Y2,S2>>, T2, Y2, S2> SimpleOp<$crate::Call<O,T,Y,S>,Y,O2,T2,Y2,S2> for $x where  $x: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>{
            fn build(
                &mut self,
                o: &$crate::Call<O,T,Y,S>,
                y: &Y,
                args: &[Id<crate::Value<O2, T2, Y2, S2>>],
            ) -> impl $crate::Builder<O2, T2, Y2, S2, Result = Id<crate::Value<O2, T2, Y2, S2>>> {
                $crate::build_fn(move|f,k|{
                    let n = o.func;
                    let n = self.funcs()[n].clone().unwrap();
                    let v = new.opts.alloc(Value::Operator(
                        O2::push(Call{func: n}).map_right(|_|()).unwrap_left(),
                        args.iter()
                            .map(|a| $crate::Use {
                                value: *a,
                                select: S2::default(),
                            })
                            .collect(),
                        y.extract(),
                        ::std::marker::PhantomData,
                    ));
                    new.blocks[k].insts.push(v);
                    Ok((v,k))
                })
            }
        }
    };
}

pub struct ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> {
    pub funcs: PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>>,
    pub data: PerID<D, Option<Id<D2>>>,
    pub wrapped: C,
}
impl<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>
    for ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>
{
    fn funcs(&self) -> &PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>> {
        return &self.funcs;
    }
}
pub fn transform_mod<O, T, Y, S, D, O2, T2: Default, Y2, S2, D2, C>(
    ctx: C,
    mut go: impl FnMut(
        &mut Func<O, T, Y, S>,
        &mut Module<O2, T2, Y2, S2, D2>,
        &mut ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>,
    ) -> anyhow::Result<Func<O2, T2, Y2, S2>>,
    mut d: impl FnMut(&D) -> anyhow::Result<D2>,
    module: &mut Module<O, T, Y, S, D>,
    n: &mut Module<O2, T2, Y2, S2, D2>,
) -> anyhow::Result<ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>> {
    // let mut n = Module::default();
    let mut a = PerID::default();
    for (i, j) in module.funcs.iter() {
        a[i] = Some(n.funcs.alloc(Default::default()))
    }
    let mut b = PerID::default();
    for (i, j) in module.data.iter() {
        b[i] = Some(n.data.alloc(d(j)?))
    }
    let mut c = ModCtx {
        wrapped: ctx,
        funcs: a,
        data: b,
    };
    for (i, j) in module.funcs.iter_mut() {
        let f = go(j, &mut *n, &mut c)?;
        n.funcs[c.funcs[i].as_ref().copied().unwrap()] = f;
    }
    return Ok(c);
}
impl<O, T, Y, S, O2, T2, S2, Y2, C: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>
    ExtractIn<C, Id<Func<O2, T2, Y2, S2>>> for Id<Func<O, T, Y, S>>
{
    fn extract_in(&self, ctx: &mut C) -> Id<Func<O2, T2, Y2, S2>> {
        return ctx.funcs()[*self].as_ref().cloned().unwrap();
    }
}
#[derive(serde::Serialize, serde::Deserialize)]
pub struct TailCall<O, T, Y, S> {
    pub func: Id<Func<O, T, Y, S>>,
    pub params: Vec<Use<O, T, Y, S>>,
}
impl<O, T, Y, S: Clone> Clone for TailCall<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            func: self.func.clone(),
            params: self.params.clone(),
        }
    }
}
impl<O, T, Y, S: Extract<S2>, O2, T2, Y2, S2, C: HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>>
    NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for TailCall<O, T, Y, S>
{
    type Then = TailCall<O2, T2, Y2, S2>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &std::collections::BTreeMap<
            Id<crate::Block<O, T, Y, S>>,
            Id<crate::Block<O2, T2, Y2, S2>>,
        >,
        m: &std::collections::BTreeMap<
            Id<crate::Value<O, T, Y, S>>,
            Id<crate::Value<O2, T2, Y2, S2>>,
        >,
    ) -> Self::Then {
        return TailCall {
            func: self.func.extract_in(ctx),
            params: self
                .params
                .iter()
                .map(|a| Use {
                    value: m.get(&a.value).copied().unwrap(),
                    select: a.select.extract(),
                })
                .collect(),
        };
    }
}
