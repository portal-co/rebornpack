use id_arena::Id;

use crate::{
    bi::{trace_func, Tracer},
    util::PerID,
    Block, Func,
};

use super::{HasModuleFuncs, Module};

pub struct ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> {
    pub funcs: PerID<Func<O, T, Y, S>, PerID<Block<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>>>,
    pub legacy: PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>>,
    pub data: PerID<D, Option<Id<D2>>>,
    pub wrapped: C,
}
impl<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> {
    pub fn bud<X>(&self, d: X) -> ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, X> {
        ModCtx {
            funcs: self.funcs.clone(),
            legacy: self.legacy.clone(),
            data: self.data.clone(),
            wrapped: d,
        }
    }
}

impl<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> HasModuleFuncs<O, T, Y, S, O2, T2, Y2, S2>
    for ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>
{
    fn funcs(&self) -> &PerID<Func<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>> {
        return &self.legacy;
    }
}
pub fn transform_mod<O, T, Y, S, D, O2, T2: Default, Y2, S2, D2, C>(
    ctx: C,
    mut go: impl FnMut(
        &mut Func<O, T, Y, S>,
        Id<Block<O, T, Y, S>>,
        &mut Module<O2, T2, Y2, S2, D2>,
        &mut ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>,
    ) -> anyhow::Result<Func<O2, T2, Y2, S2>>,
    mut d: impl FnMut(&D) -> anyhow::Result<D2>,
    module: &mut Module<O, T, Y, S, D>,
    n: &mut Module<O2, T2, Y2, S2, D2>,
) -> anyhow::Result<ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>> {
    // let mut n = Module::default();
    let mut a = PerID::default();
    let mut l = PerID::default();
    for (i, j) in module.funcs.iter() {
        let mut c: PerID<Block<O, T, Y, S>, Option<Id<Func<O2, T2, Y2, S2>>>> = Default::default();
        for (k, _) in j.blocks.iter() {
            c[k] = Some(n.funcs.alloc(Default::default()));
        }
        a[i] = c;
        l[i] = a[i][j.entry];
    }
    let mut b = PerID::default();
    for (i, j) in module.data.iter() {
        b[i] = Some(n.data.alloc(d(j)?))
    }
    let mut c = ModCtx {
        wrapped: ctx,
        funcs: a,
        data: b,
        legacy: l,
    };
    for (i, j) in module.funcs.iter_mut() {
        for k in j.blocks.iter().map(|a| a.0).collect::<Vec<_>>() {
            let f = go(j, k, &mut *n, &mut c)?;
            n.funcs[c.funcs[i][k].as_ref().copied().unwrap()] = f;
        }
    }
    return Ok(c);
}
pub trait TransparentModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C> {
    fn transparent(ctx: &mut ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>) -> &mut Self;
}
pub fn tracer<
    O,
    T,
    Y,
    S,
    O2,
    T2: Default,
    Y2,
    S2,
    R: Tracer<O, T, Y, S, O2, T2, Y2, S2, Instance: Clone>,
    D,
    D2,
    C,
>(
    // tracer: &mut C,
    // old: &Func<O, T, Y, S>,
    // new: &mut Func<O2, T2, Y2, S2>,
    // k: Id<Block<O, T, Y, S>>,
    i: R::Instance,
    mut go: impl FnMut(ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, ()>) -> anyhow::Result<R>,
) -> impl FnMut(
    &mut Func<O, T, Y, S>,
    Id<Block<O, T, Y, S>>,
    &mut Module<O2, T2, Y2, S2, D2>,
    &mut ModCtx<O, T, Y, S, D, O2, T2, Y2, S2, D2, C>,
) -> anyhow::Result<Func<O2, T2, Y2, S2>> {
    move |f, b, m, c| {
        let mut c = go(c.bud(()))?;
        let mut f2 = Func::default();
        trace_func(&mut c, f, &mut f2, b, i.clone())?;
        return Ok(f2);
    }
}
