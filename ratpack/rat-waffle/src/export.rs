use either::Either;
use id_arena::Id;
// use more_waffle::{
//     add_op, bundle_fn,
//     copying::func::{DontObf, Obfuscate},
//     new_sig, results_ref_2,
// };
use rat_ir::{
    bi::ai::InferOp,
    util::{BinOp, PerID},
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func,
};
use waffle::{FunctionBody, Module, Operator, Value, ValueDef};

use crate::{import::WaffleTerm,  OpWrapper};
pub trait WaffleExtra<T>: Clone {
    fn waffle(&self) -> T;
}
impl<B: Bound, T> WaffleExtra<T> for BoundType<B>
where
    B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: WaffleExtra<T>,
{
    fn waffle(&self) -> T {
        return self.0.waffle();
    }
}
impl<B: Bound, T> WaffleExtra<T> for BoundSelect<B>
where
    B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: WaffleExtra<T>,
{
    fn waffle(&self) -> T {
        return self.0.waffle();
    }
}
impl WaffleExtra<Vec<waffle::Type>> for Vec<waffle::Type> {
    fn waffle(&self) -> Vec<waffle::Type> {
        self.clone()
    }
}
impl WaffleExtra<Option<usize>> for Option<usize> {
    fn waffle(&self) -> Option<usize> {
        self.clone()
    }
}
pub trait ExportOp<O, T, Y, S, C> {
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)>;
}
impl<O, T, Y, S, C> ExportOp<O, T, Y, S, C> for OpWrapper {
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)> {
        return <waffle::Operator as ExportOp<O, T, Y, S, C>>::export(
            &self.0, ctx, m, target, wb, args, types,
        );
    }
}
pub fn results_ref_2(f: &mut FunctionBody, c: Value) -> Vec<Value> {
    let c = f.resolve_and_update_alias(c);
    let b = f.value_blocks[c];
    let mut v = vec![];
    let s = match f.values[c] {
        ValueDef::Operator(_, _1, _2) => f.type_pool[_2].to_owned(),
        _ => return vec![c],
    };
    if s.len() == 1 {
        return vec![c];
    }
    for (s, i) in s.iter().map(|a| *a).enumerate() {
        let w = f.add_value(ValueDef::PickOutput(c, s as u32, i));
        f.append_to_block(b, w);
        v.push(w);
    }

    return v;
}
impl<O, T, Y, S, C> ExportOp<O, T, Y, S, C> for waffle::Operator {
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)> {
        let args = target.arg_pool.from_iter(args.iter().cloned());
        let tys = target.type_pool.from_iter(types.iter().cloned());
        let v = target.values.push(ValueDef::Operator(self.clone(), args, tys));
        target.append_to_block(wb, v);
        let r = results_ref_2(target, v);
        return Ok((r, wb));
    }
}
impl<O, T, Y, S, C> ExportOp<O, T, S, Y, C> for BinOp {
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)> {
        let o: waffle::Operator = match (&self, &types[0]) {
            (_, waffle::Type::V128) => anyhow::bail!("{self:?} does not work for {}s", types[0]),
            (_, waffle::Type::FuncRef) => anyhow::bail!("{self:?} does not work for {}s", types[0]),
            (_, waffle::Type::ExternRef) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (_, waffle::Type::TypedFuncRef{..}) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Add, waffle::Type::I32) => Operator::I32Add,
            (BinOp::Add, waffle::Type::I64) => Operator::I64Add,
            (BinOp::Add, waffle::Type::F32) => Operator::F32Add,
            (BinOp::Add, waffle::Type::F64) => Operator::F64Add,
            (BinOp::Sub, waffle::Type::I32) => Operator::I32Sub,
            (BinOp::Sub, waffle::Type::I64) => Operator::I64Sub,
            (BinOp::Sub, waffle::Type::F32) => Operator::F32Sub,
            (BinOp::Sub, waffle::Type::F64) => Operator::F64Sub,
            (BinOp::Mul, waffle::Type::I32) => Operator::I32Mul,
            (BinOp::Mul, waffle::Type::I64) => Operator::I64Mul,
            (BinOp::Mul, waffle::Type::F32) => Operator::F32Mul,
            (BinOp::Mul, waffle::Type::F64) => Operator::F64Mul,
            (BinOp::And, waffle::Type::I32) => Operator::I32And,
            (BinOp::And, waffle::Type::I64) => Operator::I64And,
            (BinOp::And, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::And, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Or, waffle::Type::I32) => Operator::I32Or,
            (BinOp::Or, waffle::Type::I64) => Operator::I64Or,
            (BinOp::Or, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Or, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Xor, waffle::Type::I32) => Operator::I32Xor,
            (BinOp::Xor, waffle::Type::I64) => Operator::I64Xor,
            (BinOp::Xor, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Xor, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::DivU, waffle::Type::I32) => Operator::I32DivU,
            (BinOp::DivU, waffle::Type::I64) => Operator::I64DivU,
            (BinOp::DivU, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::DivU, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ModU, waffle::Type::I32) => Operator::I32RemU,
            (BinOp::ModU, waffle::Type::I64) => Operator::I64RemU,
            (BinOp::ModU, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ModU, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::DivS, waffle::Type::I32) => Operator::I32DivS,
            (BinOp::DivS, waffle::Type::I64) => Operator::I64DivS,
            (BinOp::DivS, waffle::Type::F32) => Operator::F32Div,
            (BinOp::DivS, waffle::Type::F64) => Operator::F64Div,
            (BinOp::ModS, waffle::Type::I32) => Operator::I32RemS,
            (BinOp::ModS, waffle::Type::I64) => Operator::I64RemS,
            (BinOp::ModS, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ModS, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Shl, waffle::Type::I32) => Operator::I32Shl,
            (BinOp::Shl, waffle::Type::I64) => Operator::I64Shl,
            (BinOp::Shl, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::Shl, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ShrS, waffle::Type::I32) => Operator::I32ShrS,
            (BinOp::ShrS, waffle::Type::I64) => Operator::I64ShrS,
            (BinOp::ShrS, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ShrS, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ShrU, waffle::Type::I32) => Operator::I32ShrU,
            (BinOp::ShrU, waffle::Type::I64) => Operator::I64ShrU,
            (BinOp::ShrU, waffle::Type::F32) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
            (BinOp::ShrU, waffle::Type::F64) => {
                anyhow::bail!("{self:?} does not work for {}s", types[0])
            }
        };
        return <waffle::Operator as ExportOp<O, T, Y, S, C>>::export(
            &o, ctx, m, target, wb, args, types,
        );
    }
}
impl<O, T, Y, S, A: ExportOp<O, T, Y, S, C>, B: ExportOp<O, T, Y, S, C>, C> ExportOp<O, T, Y, S, C>
    for Either<A, B>
{
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)> {
        match self {
            Either::Left(a) => a.export(ctx, m, target, wb, args, types),
            Either::Right(b) => b.export(ctx, m, target, wb, args, types),
        }
    }
}
impl<B: Bound, C> ExportOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, C> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        ExportOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, C>,
{
    fn export(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        wb: waffle::Block,
        args: Vec<waffle::Value>,
        types: &[waffle::Type],
    ) -> anyhow::Result<(Vec<waffle::Value>, waffle::Block)> {
        return self.0.export(ctx, m, target, wb, args, types);
    }
}
pub trait ExportTerm<O, T, Y, S, C> {
    fn export_term(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        k: waffle::Block,
        valmap: &PerID<rat_ir::Value<O, T, Y, S>, Vec<waffle::Value>>,
        block_map: &PerID<Block<O, T, Y, S>, waffle::Block>,
    ) -> anyhow::Result<()>;
}
impl<O, T, Y, S, A: ExportTerm<O, T, Y, S, C>, B: ExportTerm<O, T, Y, S, C>, C>
    ExportTerm<O, T, Y, S, C> for Either<A, B>
{
    fn export_term(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        k: waffle::Block,
        valmap: &PerID<rat_ir::Value<O, T, Y, S>, Vec<waffle::Value>>,
        block_map: &PerID<Block<O, T, Y, S>, waffle::Block>,
    ) -> anyhow::Result<()> {
        match self {
            Either::Left(a) => a.export_term(ctx, m, target, k, valmap, block_map),
            Either::Right(b) => b.export_term(ctx, m, target, k, valmap, block_map),
        }
    }
}
impl<B: Bound, C> ExportTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, C>
    for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        ExportTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, C>,
{
    fn export_term(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        k: waffle::Block,
        valmap: &PerID<
            rat_ir::Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
            Vec<waffle::Value>,
        >,
        block_map: &PerID<
            Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
            waffle::Block,
        >,
    ) -> anyhow::Result<()> {
        self.0.export_term(ctx, m, target, k, valmap, block_map)
    }
}
impl<O, T, Y, S: WaffleExtra<Option<usize>>, C> ExportTerm<O, T, Y, S, C>
    for WaffleTerm<O, T, Y, S>
{
    fn export_term(
        &self,
        ctx: &mut C,
        m: &mut Module,
        target: &mut FunctionBody,
        k: waffle::Block,
        valmap: &PerID<rat_ir::Value<O, T, Y, S>, Vec<waffle::Value>>,
        block_map: &PerID<Block<O, T, Y, S>, waffle::Block>,
    ) -> anyhow::Result<()> {
        let target_ = |b: &BlockTarget<O, T, Y, S>| waffle::BlockTarget {
            block: block_map[b.block],
            args: b
                .args
                .iter()
                .flat_map(|x| {
                    let mut y = valmap[x.value].clone();
                    if let Some(w) = x.select.waffle() {
                        y = vec![y[w]]
                    }
                    y
                })
                .collect(),
        };
        match self {
            WaffleTerm::Br(t) => {
                let t = target_(t);
                target.set_terminator(k, waffle::Terminator::Br { target: t });
            }
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let mut y = valmap[cond.value].clone();
                if let Some(w) = cond.select.waffle() {
                    y = vec![y[w]]
                }
                let y = y[0];
                let if_true = target_(if_true);
                let if_false = target_(if_false);
                target.set_terminator(
                    k,
                    waffle::Terminator::CondBr {
                        cond: y,
                        if_true,
                        if_false,
                    },
                );
            }
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => {
                let mut y = valmap[value.value].clone();
                if let Some(w) = value.select.waffle() {
                    y = vec![y[w]]
                }
                let y = y[0];
                let default = target_(default);
                let targets = cases.iter().map(target_).collect();
                target.set_terminator(
                    k,
                    waffle::Terminator::Select {
                        value: y,
                        targets,
                        default,
                    },
                );
            }
            WaffleTerm::Ret(v) => {
                let v = v
                    .iter()
                    .flat_map(|x| {
                        let mut y = valmap[x.value].clone();
                        if let Some(w) = x.select.waffle() {
                            y = vec![y[w]]
                        }
                        y
                    })
                    .collect();
                target.set_terminator(k, waffle::Terminator::Return { values: v });
            }
            WaffleTerm::Unreachable => {
                target.set_terminator(k, waffle::Terminator::Unreachable);
            }
        }
        Ok(())
    }
}
pub fn export_func<
    O: ExportOp<O, T, Y, S, C>,
    T: ExportTerm<O, T, Y, S, C>,
    Y: WaffleExtra<Vec<waffle::Type>>,
    S: WaffleExtra<Option<usize>>,
    C,
>(
    ctx: &mut C,
    m: &mut Module,
    target: &mut FunctionBody,
    src: &Func<O, T, Y, S>,
) -> anyhow::Result<PerID<Block<O, T, Y, S>, waffle::Block>> {
    let mut w = PerID::default();
    for (s, _) in src.blocks.iter() {
        w[s] = target.add_block();
    }
    for (s, _) in src.blocks.iter() {
        export_block(&mut *ctx, m, target, src, s, &w)?;
    }
    return Ok(w);
}
pub fn export_func_seal<
    O: ExportOp<O, T, Y, S, C>,
    T: ExportTerm<O, T, Y, S, C>,
    Y: WaffleExtra<Vec<waffle::Type>>,
    S: WaffleExtra<Option<usize>>,
    C,
>(
    ctx: &mut C,
    m: &mut Module,
    target: &mut FunctionBody,
    src: &Func<O, T, Y, S>,
) -> anyhow::Result<()> {
    let x = export_func(ctx, m, target, src)?;
    target.entry = x[src.entry];
    return Ok(());
}
pub fn export_block<
    C,
    O: ExportOp<O, T, Y, S, C>,
    T: ExportTerm<O, T, Y, S, C>,
    Y: WaffleExtra<Vec<waffle::Type>>,
    S: WaffleExtra<Option<usize>>,
>(
    ctx: &mut C,
    m: &mut Module,
    target: &mut FunctionBody,
    src: &Func<O, T, Y, S>,
    k: Id<Block<O, T, Y, S>>,
    w: &PerID<Block<O, T, Y, S>, waffle::Block>,
) -> anyhow::Result<()> {
    let mut wb = w[k];
    let mut params = src.blocks[k]
        .params
        .iter()
        .map(|t| {
            let t: Vec<waffle::Type> = t.waffle();
            let v = t
                .iter()
                .map(|a| target.add_blockparam(wb, *a))
                .collect::<Vec<_>>();
            v
        })
        .collect::<Vec<_>>();
    let mut m2: PerID<rat_ir::Value<O, T, Y, S>, Vec<waffle::Value>> = PerID::default();
    for v in src.blocks[k].insts.iter().map(|a| *a) {
        m2[v] = match &src.opts[v] {
            rat_ir::Value::Operator(o, u, y, _) => {
                let u = u
                    .iter()
                    .map(|u| {
                        let mut v = m2[u.value].clone();
                        let s: Option<usize> = u.select.waffle();
                        if let Some(s) = s {
                            v = vec![v[s]];
                        }
                        v
                    })
                    .flatten()
                    .collect::<Vec<_>>();
                let (r, wb2) = o.export(&mut *ctx, m, target, wb, u, &y.waffle())?;
                wb = wb2;
                r
            }
            rat_ir::Value::BlockParam(p, _, _) => {
                let p = params[*p].clone();
                p
            }
            rat_ir::Value::Alias(u, _) => {
                let mut v = m2[u.value].clone();
                let s: Option<usize> = u.select.waffle();
                if let Some(s) = s {
                    v = vec![v[s]];
                }
                v
            }
        };
    }
    src.blocks[k]
        .term
        .export_term(&mut *ctx, m, target, wb, &m2, &*w)?;
    return Ok(());
}
