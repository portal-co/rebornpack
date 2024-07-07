use either::Either;
use id_arena::Id;
use rat_ir::{
    no_push,
    util::{BinOp, If, PerID},
    BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, Value,
};

use crate::backend::{CondType, Plat};

use super::{rat_fe::Data, Instr, SSAFunc, Term, ValueId};

pub trait BackendOp<C, O, T, Y, S> {
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)>;
}
impl<C, O, T, Y, S> BackendOp<C, O, T, Y, S> for BinOp {
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return Ok((
            f.blocks[k].need(Instr::Bin(
                self.clone(),
                params[0].clone(),
                params[1].clone(),
            )),
            k,
        ));
    }
}
pub struct Fptr<T> {
    pub fptr: T,
}
no_push!(
    type Fptr<T>;
);
no_push!(type CondType;);
impl<C, O, T, Y, S> BackendOp<C, O, T, Y, S> for Fptr<String> {
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return Ok((f.blocks[k].need(Instr::Addrof(self.fptr.clone())), k));
    }
}
impl<C, O, T, Y, S> BackendOp<C, O, T, Y, S> for Data {
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return Ok((f.blocks[k].need(Instr::Data(self.0.clone())), k));
    }
}
impl<C,O,T,Y,S> BackendOp<C,O,T,Y,S> for CondType{
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        fun: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        let tru = fun.blocks.alloc(Default::default());
        let fals = fun.blocks.alloc(Default::default());
        let done = fun.blocks.alloc(Default::default());
        // let (cond, k) = cond.ssa(fun, k, vars, fparams,via)?;
        let cond = params[0].clone();
        let mut vars = fun.blocks[k].insts.iter().map(|a|((),a.0.clone())).collect::<Vec<_>>();
        let bak = vars.clone();
        let mut trup = vec![];
        for (i, (_, v)) in vars.iter_mut().enumerate() {
            trup.push(v.clone());
            *v = fun.blocks[tru].need(Instr::Param(i));
        }
        // let (t, l) = if_true.ssa(fun, tru, vars, fparams,via)?;
        let l = tru;
        let t = fun.blocks[l].need(Instr::Const(1));
        let mut p = vec![];
        for (_, a) in vars.iter() {
            p.push(a.clone())
        }
        p.push(t);
        fun.blocks[l].term = Term::Br(super::Target {
            id: done,
            params: p,
        });
        vars = bak;
        let mut flsp = vec![];
        for (i, (_, v)) in vars.iter_mut().enumerate() {
            flsp.push(v.clone());
            *v = fun.blocks[fals].need(Instr::Param(i));
        }
        // let (t, l) = if_false.ssa(fun, fals, vars, fparams,via)?;
        let l = fals;
        let t = fun.blocks[l].need(Instr::Const(0));
        let mut p = vec![];
        for (_, a) in vars.iter() {
            p.push(a.clone())
        }
        p.push(t);
        fun.blocks[l].term = Term::Br(super::Target {
            id: done,
            params: p,
        });
        fun.blocks[k].term = Term::BrIf {
            cond: cond,
            cty: self.clone(),
            if_true: super::Target {
                id: tru,
                params: trup,
            },
            if_false: super::Target {
                id: fals,
                params: flsp,
            },
        };
        for (i, (_, v)) in vars.iter_mut().enumerate() {
            *v = fun.blocks[done].need(Instr::Param(i));
        }
        let w = fun.blocks[done].need(Instr::Param(vars.len()));
        return Ok((w, done));
    }
}
pub struct Call {}
impl<C, O, T, Y, S> BackendOp<C, O, T, Y, S> for Call {
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return Ok((
            f.blocks[k].need(Instr::Call(
                params[0].clone(),
                params[1..].iter().cloned().collect(),
            )),
            k,
        ));
    }
}
impl<C, O, T, Y, S> BackendOp<C, O, T, Y, S> for Plat{
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return Ok((
            f.blocks[k].need(Instr::Plat(
                self.clone(),
                params.iter().cloned().collect(),
            )),
            k,
        ));
    }
}
impl<C, O, T, Y, S, A: BackendOp<C, O, T, Y, S>, B: BackendOp<C, O, T, Y, S>>
    BackendOp<C, O, T, Y, S> for Either<A, B>
{
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        match self {
            Either::Left(a) => a.emit(ctx, params, f, k),
            Either::Right(b) => b.emit(ctx, params, f, k),
        }
    }
}
pub trait BackendTerm<C, O, T, Y, S> {
    fn finalize(
        &self,
        ctx: &mut C,
        blocks: &PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>,
        values: &PerID<Value<O, T, Y, S>, Option<ValueId>>,
        target: Id<super::Block>,
        d: &mut SSAFunc,
    ) -> anyhow::Result<()>;
}
impl<C, O, T, Y, S> BackendTerm<C, O, T, Y, S> for BlockTarget<O, T, Y, S> {
    fn finalize(
        &self,
        ctx: &mut C,
        blocks: &PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>,
        values: &PerID<Value<O, T, Y, S>, Option<ValueId>>,
        target: Id<super::Block>,
        d: &mut SSAFunc,
    ) -> anyhow::Result<()> {
        d.blocks[target].term = Term::Br(super::Target {
            id: blocks[self.block].unwrap(),
            params: self
                .args
                .iter()
                .flat_map(|a| values[a.value].clone())
                .collect(),
        });
        Ok(())
    }
}
impl<C, O, T, Y, S, X: BackendTerm<C, O, T, Y, S>> BackendTerm<C, O, T, Y, S>
    for If<O, T, Y, S, X>
{
    fn finalize(
        &self,
        ctx: &mut C,
        blocks: &PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>,
        values: &PerID<Value<O, T, Y, S>, Option<ValueId>>,
        target: Id<super::Block>,
        d: &mut SSAFunc,
    ) -> anyhow::Result<()> {
        let t2 = d.blocks.alloc(Default::default());
        let tp2 = values
            .data
            .iter()
            .flatten()
            .enumerate()
            .map(|(i, a)| {
                // let j = d.blocks[t2].need(Instr::Param(i));
                d.blocks[t2].insts.insert(a.clone(),Instr::Param(i));
                a.clone()
            })
            .collect::<Vec<_>>();
        let t3 = d.blocks.alloc(Default::default());
        let tp3 = values
            .data
            .iter()
            .flatten()
            .enumerate()
            .map(|(i, a)| {
                // d.blocks[t3].need(Instr::Param(i));
                d.blocks[t3].insts.insert(a.clone(),Instr::Param(i));
                a.clone()
            })
            .collect::<Vec<_>>();
        self.then.finalize(ctx, blocks, values, t2, d)?;
        self.r#else
            .as_ref()
            .unwrap()
            .finalize(ctx, blocks, values, t3, d)?;
        d.blocks[target].term = Term::BrIf {
            cond: values[self.val.value].as_ref().unwrap().clone(),
            cty: crate::backend::CondType::Eq,
            if_true: super::Target {
                id: t3,
                params: tp3,
            },
            if_false: super::Target {
                id: t2,
                params: tp2,
            },
        };
        Ok(())
    }
}
impl<C, O, T, Y, S, A: BackendTerm<C, O, T, Y, S>, B: BackendTerm<C, O, T, Y, S>>
    BackendTerm<C, O, T, Y, S> for Either<A, B>
{
    fn finalize(
        &self,
        ctx: &mut C,
        blocks: &PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>,
        values: &PerID<Value<O, T, Y, S>, Option<ValueId>>,
        target: Id<super::Block>,
        d: &mut SSAFunc,
    ) -> anyhow::Result<()> {
        match self {
            Either::Left(a) => a.finalize(ctx, blocks, values, target, d),
            Either::Right(b) => b.finalize(ctx, blocks, values, target, d),
        }
    }
}
impl<B: Bound, C> BackendTerm<C, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>
    for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        BackendTerm<C, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn finalize(
        &self,
        ctx: &mut C,
        blocks: &PerID<
            rat_ir::Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
            Option<Id<super::Block>>,
        >,
        values: &PerID<
            Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
            Option<ValueId>,
        >,
        target: Id<super::Block>,
        d: &mut SSAFunc,
    ) -> anyhow::Result<()> {
        return self.0.finalize(ctx, blocks, values, target, d);
    }
}
impl<B: Bound, C> BackendOp<C, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>
    for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        BackendOp<C, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn emit(
        &self,
        ctx: &mut C,
        params: Vec<ValueId>,
        f: &mut SSAFunc,
        k: Id<super::Block>,
    ) -> anyhow::Result<(ValueId, Id<super::Block>)> {
        return self.0.emit(ctx, params, f, k);
    }
}
pub fn export_block<C, O: BackendOp<C, O, T, Y, S>, T: BackendTerm<C, O, T, Y, S>, Y, S>(
    i: &Func<O, T, Y, S>,
    ib: Id<rat_ir::Block<O, T, Y, S>>,
    d: &mut SSAFunc,
    p: &PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>,
    ctx: &mut C,
) -> anyhow::Result<()> {
    let mut dst = p[ib].as_ref().copied().unwrap();
    let mut params = i.blocks[ib]
        .params
        .iter()
        .enumerate()
        .map(|(i, _)| d.blocks[dst].need(Instr::Param(i)))
        .collect::<Vec<_>>();
    let mut m: PerID<Value<O, T, Y, S>, Option<ValueId>> = PerID::default();
    for v in i.blocks[ib].insts.iter() {
        m[*v] = Some(match &i.opts[*v] {
            Value::Operator(o, a, _, _) => {
                let (a, b) = o.emit(
                    ctx,
                    a.iter()
                        .filter_map(|b| m[b.value].as_ref().cloned())
                        .collect(),
                    d,
                    dst,
                )?;
                dst = b;
                a
            }
            Value::BlockParam(a, _, _) => params[*a].clone(),
            Value::Alias(u, _) => m[u.value].as_ref().cloned().unwrap(),
        })
    }
    i.blocks[ib].term.finalize(ctx, p, &m, dst, d)?;
    return Ok(());
}
pub fn export_func<C, O: BackendOp<C, O, T, Y, S>, T: BackendTerm<C, O, T, Y, S>, Y, S>(
    i: &Func<O, T, Y, S>,
    d: &mut SSAFunc,
    ctx: &mut C,
) -> anyhow::Result<PerID<rat_ir::Block<O, T, Y, S>, Option<Id<super::Block>>>> {
    let mut m = PerID::default();
    for k in i.blocks.iter().map(|a| a.0) {
        m[k] = Some(d.blocks.alloc(Default::default()));
    }
    for k in i.blocks.iter().map(|a| a.0) {
        export_block(i, k, d, &m, ctx)?;
    }
    return Ok(m);
}
