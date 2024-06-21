use axle::{Func, Module, ParamID, ValueID};
use gorf_core::{Binder, GTerm};
use id_arena::Id;
use indexmap::IndexMap;
use std::{collections::HashMap, fmt::format, hash::Hash};

use crate::export::{App1, Bind};
pub trait Return {
    fn r#return(x: ValueID) -> Self;
}
pub struct Translator<'a, O, T, D, S, V: Binder, M> {
    pub cache: HashMap<GTerm<V, M>, Id<Func<O, T, D, S>>>,
    pub target: &'a mut Module<O, T, D, S>,
}
impl<
        'a,
        O: From<App1> + From<Bind<O, T, D, S>>,
        T: Return + Default,
        D,
        S: Default,
        V: Binder<Var = V> + Into<ParamID> + Into<ValueID> + Eq + Ord + Hash + Clone,
        M: Eq + Hash + Clone,
    > Translator<'a, O, T, D, S, V, M>
{
    pub fn import(&mut self, t: GTerm<V, M>) -> anyhow::Result<Id<Func<O, T, D, S>>> {
        if let Some(c) = self.cache.get(&t) {
            return Ok(*c);
        }
        let mut f = Func::default();
        let v = self.apply(&t, &mut f)?;
        f.terminator = T::r#return(v);
        let f = self.target.funcs.alloc(f);
        self.cache.insert(t, f);
        return Ok(f);
    }
    pub fn apply(&mut self, t: &GTerm<V, M>, f: &mut Func<O, T, D, S>) -> anyhow::Result<ValueID> {
        return Ok(match t {
            GTerm::Undef => todo!(),
            GTerm::Var(v) => {
                let v = v.clone();
                let k: ValueID = v.clone().into();
                if !f.values.contains_key(&k) {
                    f.values.insert(k.clone(), axle::Value::Param(v.into()));
                };
                k
            }
            GTerm::Abs(a) => {
                let (ref a, ref t) = &**a;
                let t = self.import(t.clone())?;
                let a = a.clone();
                let mut m = IndexMap::new();
                for p in self.target.funcs[t].params() {
                    if p != a.clone().into() {
                        m.insert(p.clone(), ValueID(p.0.clone()));
                    }
                }
                let c = ValueID(format!("/{}", t.index()));
                if !f.values.contains_key(&c) {
                    f.values.insert(
                        c.clone(),
                        axle::Value::Operator(Bind { id: t }.into(), m, vec![]),
                    );
                };
                c
            }
            GTerm::App(a) => {
                let (ref a, ref b) = &**a;
                let a = self.apply(a, f)?;
                let b = self.apply(b, f)?;
                let c = ValueID(format!("({} {})", a.0, b.0));
                let mut m = IndexMap::new();
                m.insert(ParamID(a.0.clone()), a.clone());
                m.insert(ParamID(b.0.clone()), b.clone());
                if !f.values.contains_key(&c) {
                    f.values.insert(
                        c.clone(),
                        axle::Value::Operator(
                            App1 {
                                target: ParamID(a.0.clone()),
                                param: ParamID(b.0.clone()),
                            }
                            .into(),
                            m,
                            vec![],
                        ),
                    );
                }

                c
            }
            GTerm::Mix(_) => todo!(),
        });
    }
}
