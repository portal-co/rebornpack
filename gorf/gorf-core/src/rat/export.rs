use std::{collections::BTreeMap, f32::consts::E, marker::PhantomData};

use id_arena::Id;
use rat_ir::{module::Module, no_push, Block, Func, Use, Value};

use crate::{Binder, GTerm};
pub enum Call<O, T, Y, S> {
    Func(Id<Func<O, T, Y, S>>),
    Var,
}
no_push!(
    type Call<O, T, Y, S>;
);
pub struct Ret<O, T, Y, S> {
    pub val: Id<Value<O, T, Y, S>>,
}
no_push!(
    type Ret<O, T, Y, S>;
);
pub fn export<
    V: Binder<Var = V> + Eq + Ord + Clone,
    M: Clone,
    O: From<Call<O, T, Y, S>>,
    T: Default + From<Ret<O, T, Y, S>>,
    Y: Default + Clone,
    S: Default,
    D,
>(
    g: &GTerm<V, M>,
    m: &mut Module<O, T, Y, S, D>,
    f: Id<Func<O, T, Y, S>>,
    k: Id<Block<O, T, Y, S>>,
    scope: &BTreeMap<V, Id<Value<O, T, Y, S>>>,
) -> anyhow::Result<Id<Value<O, T, Y, S>>> {
    match g {
        GTerm::Undef => todo!(),
        GTerm::Var(v) => Ok(scope.get(v).unwrap().clone()),
        GTerm::Abs(a) => {
            let (a, b) = a.as_ref();
            let new = m.funcs.alloc(Default::default());
            let e = m.funcs[new].entry;
            let mut new_params: BTreeMap<V, Id<Value<O, T, Y, S>>> = b
                .frees()
                .iter()
                .filter(|x| *x != a)
                .map(|a| {
                    (
                        a.clone(),
                        m.funcs[new].add_blockparam(e, Default::default()),
                    )
                })
                .collect();
            new_params.insert(
                a.clone(),
                m.funcs[new].add_blockparam(e, Default::default()),
            );
            let new_value = export(g, m, new, e, &new_params)?;
            m.funcs[new].blocks[e].term = Ret { val: new_value }.into();
            let args = b
                .frees()
                .iter()
                .filter(|x| *x != a)
                .map(|r| Use {
                    value: scope.get(r).unwrap().clone(),
                    select: S::default(),
                })
                .collect::<Vec<_>>();
            let c = m.funcs[f].opts.alloc(Value::Operator(
                Call::Func(new).into(),
                args,
                Y::default(),
                PhantomData,
            ));
            m.funcs[f].blocks[k].insts.push(c);
            return Ok(c);
        }
        GTerm::App(k2) => {
            let (a, b) = k2.as_ref();
            let a = export(a, m, f, k, scope)?;
            let b = export(b, m, f, k, scope)?;
            let c = m.funcs[f].opts.alloc(Value::Operator(
                Call::Var.into(),
                vec![
                    Use {
                        value: a,
                        select: S::default(),
                    },
                    Use {
                        value: b,
                        select: S::default(),
                    },
                ],
                Y::default(),
                PhantomData,
            ));
            m.funcs[f].blocks[k].insts.push(c);
            return Ok(c);
        }
        GTerm::Mix(_) => todo!(),
    }
}
