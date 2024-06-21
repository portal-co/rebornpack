use std::{clone, collections::BTreeMap, convert::Infallible};

use anyhow::Context;
use axle::{Func, Module, ParamID, ValueID};

use gorf_core::{abs, app, var, GTerm, SimpleBinder};
use id_arena::Id;
use indexmap::IndexMap;

use crate::Nope;
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Key<O, T, D, S> {
    Func(Id<Func<O, T, D, S>>),
    Val(ValueID),
    Param(ParamID),
    Tmp(usize),
}
impl<O, T, D, S> Clone for Key<O, T, D, S> {
    fn clone(&self) -> Self {
        match self {
            Self::Func(arg0) => Self::Func(arg0.clone()),
            Self::Val(arg0) => Self::Val(arg0.clone()),
            Self::Param(arg0) => Self::Param(arg0.clone()),
            Self::Tmp(a) => Self::Tmp(*a),
        }
    }
}
pub struct Bind<O, T, D, S> {
    pub id: Id<Func<O, T, D, S>>,
}

impl<O, T, D, S> SimpleBinder for Key<O, T, D, S> {}
type Term<O, T, D, S> = gorf_core::GTerm<Key<O, T, D, S>, Infallible>;
pub trait GorfOp<O, T, D, S> {
    fn op(
        &self,
        then: Term<O, T, D, S>,
        operands: IndexMap<ParamID, Key<O, T, D, S>>,
        mo: &Module<O, T, D, S>,
    ) -> anyhow::Result<Term<O, T, D, S>>;
}
impl<O, T, D, S> GorfOp<O, T, D, S> for Bind<O, T, D, S> {
    fn op(
        &self,
        then: Term<O, T, D, S>,
        operands: IndexMap<ParamID, Key<O, T, D, S>>,
        mo: &Module<O, T, D, S>,
    ) -> anyhow::Result<Term<O, T, D, S>> {
        let order = mo.funcs[self.id].params();
        let mut v = var(Key::Func(self.id));
        for (f, _) in mo.funcs.iter() {
            v = app(v, var(Key::Func(f)));
        }
        let mut i = 0;
        for o in order {
            // let o = operands.get(&o).context("in getting an operand")?.clone();
            match operands.get(&o) {
                Some(o) => v = app(v, var(o.clone())),
                None => {
                    v = app(v, var(Key::Tmp(i)));
                    i += 1
                }
            }
        }
        for n in (0..i).rev() {
            v = abs(Key::Tmp(n), v)
        }
        v = app(then, v);
        return Ok(v);
    }
}
pub struct App1 {
    pub target: ParamID,
    pub param: ParamID,
}
impl<O, T, D, S> GorfOp<O, T, D, S> for App1 {
    fn op(
        &self,
        then: Term<O, T, D, S>,
        operands: IndexMap<ParamID, Key<O, T, D, S>>,
        mo: &Module<O, T, D, S>,
    ) -> anyhow::Result<Term<O, T, D, S>> {
        let a = operands
            .get(&self.target)
            .context("in getting the function")?
            .clone();
        let b = operands
            .get(&self.param)
            .context("in getting the parameter")?
            .clone();
        return Ok(app(then, app(var(a), var(b))));
    }
}
pub trait GorfTerm<O, T, D, S> {
    fn term(&self) -> anyhow::Result<Term<O, T, D, S>>;
}

pub fn translate<O: GorfOp<O, T, D, S>, T: GorfTerm<O, T, D, S>, D: Nope, S>(
    m: &Module<O, T, D, S>,
    f: Id<Func<O, T, D, S>>,
) -> anyhow::Result<Term<O, T, D, S>> {
    let fr = &m.funcs[f];
    let mut res = fr.terminator.term()?;
    for (i, j) in fr.values.iter().rev() {
        let i = i.clone();
        match j {
            axle::Value::Operator(a, b, _) => {
                res = a.op(
                    abs(Key::Val(i), res),
                    b.iter()
                        .map(|(k, c)| (k.clone(), Key::Val(c.clone())))
                        .collect(),
                    m,
                )?;
            }
            axle::Value::Param(p) => res = app(abs(Key::Val(i), res), var(Key::Param(p.clone()))),
            axle::Value::Alias(v) => res = app(abs(Key::Val(i), res), var(Key::Val(v.clone()))),
        }
    }
    for p in fr.params().iter().rev() {
        res = abs(Key::Param(p.clone()), res);
    }
    for (f, _) in m.funcs.iter().rev() {
        res = abs(Key::Func(f), res);
    }
    return Ok(res);
}
