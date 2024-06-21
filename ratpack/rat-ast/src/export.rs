use std::{collections::BTreeMap, iter::once};

use either::Either;
use id_arena::Id;
use rat_ir::{
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator,
};
use relooper::SimpleBlock;
pub mod rust;
pub trait ToIdAst {
    fn ssa<O, T, S, Y, A: ExportAst<O, T, S, Y>>(&self) -> A::Var;
    fn var_ssa<O, T, S, Y, A: ExportAst<O, T, S, Y>>(&self, y: &Y) -> A {
        return A::get(self.ssa::<O, T, S, Y, A>(), y);
    }
}
impl<X> ToIdAst for Id<X> {
    fn ssa<O, T, S, Y, A: ExportAst<O, T, S, Y>>(&self) -> A::Var {
        return A::Var::from(format!("ssa{}", self.index()));
    }
}
pub trait Emit<X>{
    fn emit(&self) -> X;
}
pub trait ExportAst<O, T, S, Y>: Clone {
    type Var: From<String> + Clone;
    fn get(var: Self::Var, y: &Y) -> Self;
    fn assign(&self, var: Self::Var, y: &Y) -> Self;
    fn select(&self, s: &S) -> Self;
    fn append(&mut self, i: impl Iterator<Item = Self>);
    fn unit() -> Self;
    fn op(o: &O, args: &[Self]) -> Self;
}

pub trait CffAst<O, T, S, Y>: ExportAst<O, T, S, Y> {
    fn br_id(a: usize) -> Self;
    fn set_id(a: usize) -> Self;
    fn switch(m: &BTreeMap<usize, Self>) -> Self;
    fn forever(&self) -> Self;
}
pub trait ReloopAst<O, T, S, Y>: CffAst<O, T, S, Y> {
    fn r#break(id: u16) -> Self;
    fn r#continue(id: u16) -> Self;
    fn r#loop(&self, id: u16) -> Self;
}
pub trait ExportTerm<A: ExportAst<O, T, S, Y>, O, T, Y, S>: SaneTerminator<O, T, Y, S> {
    fn go(
        &self,
        s: impl FnMut(Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        body: A,
    ) -> anyhow::Result<A>;
}
impl<
        O,
        T,
        Y,
        S,
        A: ExportAst<O, T, S, Y>,
        V1: ExportTerm<A, O, T, Y, S>,
        V2: ExportTerm<A, O, T, Y, S>,
    > ExportTerm<A, O, T, Y, S> for Either<V1, V2>
{
    fn go(
        &self,
        s: impl FnMut(Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        body: A,
    ) -> anyhow::Result<A> {
        match self {
            Either::Left(a) => a.go(s, f, body),
            Either::Right(b) => b.go(s, f, body),
        }
    }
}
impl<O, T, Y: Clone, S: Clone, A: ExportAst<O, T, S, Y>> ExportTerm<A, O, T, Y, S>
    for BlockTarget<O, T, Y, S>
{
    fn go(
        &self,
        mut s: impl FnMut(Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        mut body: A,
    ) -> anyhow::Result<A> {
        body.append(once(s.target(f, self, vec![])));
        return Ok(body);
    }
}
impl<B: Bound, A: ExportAst<BoundOp<B>, BoundTerm<B>, BoundSelect<B>, BoundType<B>>>
    ExportTerm<A, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        ExportTerm<A, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn go(
        &self,
        s: impl FnMut(Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>) -> A,
        f: &Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        mut body: A,
    ) -> anyhow::Result<A> {
        let s2: Box<
            dyn FnMut(Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>) -> A + '_,
        > = Box::new(s);
        return self.0.go(s2, f, body);
    }
}
// impl<
//         B: Bound,
//         A: ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >,
//     > ExportAst<BoundOp<B>, BoundTerm<B>, BoundSelect<B>, BoundType<B>> for A
// {
//     type Var = <A as ExportAst<
//         B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//     >>::Var;

//     fn get(var: Self::Var, y: &BoundType<B>) -> Self {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::get(var, &y.0)
//     }

//     fn assign(&self, var: Self::Var, y: &BoundType<B>) -> Self {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::assign(self, var, &y.0)
//     }

//     fn select(&self, s: &BoundSelect<B>) -> Self {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::select(self, &s.0)
//     }

//     fn append(&mut self, i: impl Iterator<Item = Self>) {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::append(self, i)
//     }

//     fn unit() -> Self {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::unit()
//     }

//     fn op(o: &BoundOp<B>, args: &[Self]) -> Self {
//         <A as ExportAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::op(&o.0, args)
//     }
// }
// impl<
//         B: Bound,
//         A: CffAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >,
//     > CffAst<BoundOp<B>, BoundTerm<B>, BoundSelect<B>, BoundType<B>> for A
// {
//     fn br_id(a: usize) -> Self {
//         <A as CffAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::br_id(a)
//     }

//     fn switch(entry: usize, m: &BTreeMap<usize, Self>) -> Self {
//         <A as CffAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::switch(entry, m)
//     }

//     fn forever(&self) -> Self {
//         <A as CffAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::forever(self)
//     }
// }
// impl<
//         B: Bound,
//         A: ReloopAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >,
//     > ReloopAst<BoundOp<B>, BoundTerm<B>, BoundSelect<B>, BoundType<B>> for A
// {
//     fn set_id(a: usize) -> Self {
//         <A as ReloopAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::set_id(a)
//     }

//     fn r#break(id: u16) -> Self {
//         <A as ReloopAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::r#break(id)
//     }

//     fn r#continue(id: u16) -> Self {
//         <A as ReloopAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::r#continue(id)
//     }

//     fn r#loop(&self, id: u16) -> Self {
//         <A as ReloopAst<
//             B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//             B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
//         >>::r#loop(self, id)
//     }
// }
pub trait EmitTarget<O, T, Y, S, A: ExportAst<O, T, S, Y>>:
    FnMut(Id<Block<O, T, Y, S>>) -> A
{
    fn target(
        &mut self,
        f: &Func<O, T, Y, S>,
        k: &BlockTarget<O, T, Y, S>,
        mut before: Vec<A>,
    ) -> A {
        let tys = k.block_types(f);
        before.extend(k.args.iter().map(|a| {
            let v = A::Var::from(format!("ssa{}", a.value.index()));
            return A::get(v, f.opts[a.value].ty()).select(&a.select);
        }));
        let mut r = A::unit();
        r.append(
            before.iter().zip(tys).enumerate().map(|(i, (x, y))| {
                x.assign(A::Var::from(format!("bp{i}at{}", k.block.index())), y)
            }),
        );
        r.append(once(self(k.block)));
        return r;
    }
}
impl<O, T, Y, S, A: ExportAst<O, T, S, Y>, B: FnMut(Id<Block<O, T, Y, S>>) -> A>
    EmitTarget<O, T, Y, S, A> for B
{
}
pub fn render_bb<O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: ExportAst<O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
    k: Id<Block<O, T, Y, S>>,
    a2: &mut A,
    go: impl FnMut(Id<Block<O, T, Y, S>>) -> A,
) -> anyhow::Result<()> {
    let mut a = A::unit();
    let k2 = k;
    let kr = &f.blocks[k];
    for i in kr.insts.iter() {
        let iv = A::Var::from(format!("ssa{}", i.index()));
        let y = f.opts[*i].ty();
        match &f.opts[*i] {
            rat_ir::Value::Operator(o, u, _, _) => {
                let uvs = u
                    .iter()
                    .map(|a| {
                        let v = A::Var::from(format!("ssa{}", a.value.index()));
                        return A::get(v, f.opts[a.value].ty()).select(&a.select);
                    })
                    .collect::<Vec<_>>();
                a.append(once(A::op(o, &uvs).assign(iv.clone(), f.opts[*i].ty())));
            }
            rat_ir::Value::BlockParam(i, b, y2) => {
                let v = A::Var::from(format!("bp{i}at{}", k2.index()));
                a.append(once(A::get(v, y2).assign(iv.clone(), y)));
            }
            rat_ir::Value::Alias(u, _) => {
                let v = A::Var::from(format!("ssa{}", u.value.index()));
                let s = A::get(v, f.opts[u.value].ty()).select(&u.select);
                a.append(once(s.assign(iv.clone(), f.opts[*i].ty())));
            }
        }
    }
    a2.append(once(f.blocks[k].term.go(go, f, a)?));
    return Ok(());
}
pub fn render_cff_block<O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: CffAst<O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
    k: Id<Block<O, T, Y, S>>,
    a: &mut A,
) -> anyhow::Result<()> {
    render_bb(f, k, a, |l| A::br_id(l.index()))?;
    // a.append(once(f.blocks[k].term.go(, f)?));
    return Ok(());
}
pub fn render_cff<O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: CffAst<O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
) -> anyhow::Result<A> {
    let m = f
        .blocks
        .iter()
        .map(|(id, _)| {
            let mut a = A::unit();
            render_cff_block(f, id, &mut a)?;
            return Ok((id.index(), a));
        })
        .collect::<anyhow::Result<BTreeMap<usize, A>>>()?;
    let mut s = A::set_id(f.entry.index());
    s.append(once(A::switch(&m).forever()));
    return Ok(s);
}
pub fn render_relooped_func<O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: ReloopAst<O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
    a: &mut A,
) -> anyhow::Result<()> {
    let cfg = f.domap_ref();
    let x = relooper::reloop(
        f.blocks
            .iter()
            .map(|a| {
                (
                    a.0,
                    a.1.term
                        .targets()
                        .into_iter()
                        .chain(f.blocks.iter().filter_map(|b| {
                            if rat_ir::dom::dominates(&cfg, Some(b.0), Some(a.0)) {
                                Some(b.0)
                            } else {
                                None
                            }
                        }))
                        .collect::<Vec<_>>(),
                )
            })
            .collect(),
        f.entry,
    );
    return render_relooped(f, x.as_ref(), a);
}
pub fn render_relooped<O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: ReloopAst<O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
    x: &relooper::ShapedBlock<Id<Block<O, T, Y, S>>>,
    a: &mut A,
) -> anyhow::Result<()> {
    // let mut a = A::unit();
    match x {
        relooper::ShapedBlock::Simple(s) => {
            let k = &f.blocks[s.label];
            render_bb(f, s.label, a, |k| match s.branches.get(&k).unwrap() {
                relooper::BranchMode::LoopBreak(i) => A::r#break(*i),
                relooper::BranchMode::LoopBreakIntoMulti(i) => {
                    let mut b = A::set_id(k.index());
                    b.append(once(A::r#break(*i)));
                    b
                }
                relooper::BranchMode::LoopContinue(i) => A::r#continue(*i),
                relooper::BranchMode::LoopContinueIntoMulti(i) => {
                    let mut b = A::set_id(k.index());
                    b.append(once(A::r#break(*i)));
                    b
                }
                relooper::BranchMode::MergedBranch => A::unit(),
                relooper::BranchMode::MergedBranchIntoMulti => A::set_id(k.index()),
                relooper::BranchMode::SetLabelAndBreak => A::br_id(k.index()),
            })?;

            if let Some(i) = s.immediate.as_ref().map(|a| a.as_ref()) {
                render_relooped(f, i, a)?;
            }
            if let Some(i) = s.next.as_ref().map(|a| a.as_ref()) {
                render_relooped(f, i, a)?;
            }
        }
        relooper::ShapedBlock::Loop(l) => {
            let mut b = A::unit();
            render_relooped(f, l.inner.as_ref(), &mut b)?;
            a.append(once(b.r#loop(l.loop_id)));
            if let Some(i) = l.next.as_ref().map(|a| a.as_ref()) {
                render_relooped(f, i, a)?;
            }
        }
        relooper::ShapedBlock::Multiple(block) => {
            let mut m = BTreeMap::new();
            let mut running = A::unit();
            // let mut l2 = None;
            for h in block.handled.iter().rev() {
                if h.break_after {
                    running = A::unit();
                }
                let mut b = A::unit();
                render_relooped(f, &h.inner, &mut b)?;
                b.append(once(running));
                running = b.clone();
                for l in h.labels.iter().rev() {
                    m.insert(l.index(), b.clone());
                    // l2 = Some(*l);
                }
            }
            // let l2 = l2.unwrap();
            a.append(once(A::switch(&m)))
        }
    };
    return Ok(());
}
pub fn vars<'a, O, T: ExportTerm<A, O, T, Y, S>, Y, S, A: ExportAst<O, T, S, Y>>(
    f: &'a Func<O, T, Y, S>,
) -> impl Iterator<Item = (A::Var, &'a Y)> + 'a {
    return f
        .opts
        .iter()
        .map(|a| (a.0.ssa::<O, T, S, Y, A>(), a.1.ty()))
        .chain(f.blocks.iter().flat_map(|(k, v)| {
            v.params
                .iter()
                .enumerate()
                .map(move |(i, y)| (A::Var::from(format!("bp{i}at{}", k.index())), y))
        }));
}
