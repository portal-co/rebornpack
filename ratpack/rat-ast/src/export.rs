use std::{collections::BTreeMap, iter::once};

use either::Either;
use id_arena::Id;
use rat_ir::{
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, SaneTerminator,
};
use relooper::SimpleBlock;
pub mod rust;
pub trait ToIdAst {
    fn ssa<C,O, T, S, Y, A: ExportAst<C,O, T, S, Y>>(&self) -> A::Var;
    fn var_ssa<C,O, T, S, Y, A: ExportAst<C,O, T, S, Y>>(&self,ctx: &mut C, y: &Y) -> A {
        return A::get(ctx,self.ssa::<C,O, T, S, Y, A>(), y);
    }
}
impl<X> ToIdAst for Id<X> {
    fn ssa<C,O, T, S, Y, A: ExportAst<C,O, T, S, Y>>(&self) -> A::Var {
        return A::Var::from(format!("ssa{}", self.index()));
    }
}
pub trait Emit<X>{
    fn emit(&self) -> X;
}
pub trait ExportAst<C,O, T, S, Y>: Clone {
    type Var: From<String> + Clone;
    fn get(ctx: &mut C,var: Self::Var, y: &Y) -> Self;
    fn assign(&self,ctx: &mut C, var: Self::Var, y: &Y) -> Self;
    fn select(&self,ctx: &mut C, s: &S) -> Self;
    fn append(&mut self,ctx: &mut C, i: impl Iterator<Item = Self>);
    fn unit(ctx: &mut C) -> Self;
    fn op(ctx: &mut C,o: &O, args: &[Self]) -> Self;
}

pub trait CffAst<C,O, T, S, Y>: ExportAst<C,O, T, S, Y> {
    fn br_id(ctx: &mut C,a: usize) -> Self;
    fn set_id(ctx: &mut C,a: usize) -> Self;
    fn switch(ctx: &mut C,m: &BTreeMap<usize, Self>) -> Self;
    fn forever(&self,ctx: &mut C) -> Self;
}
pub trait ReloopAst<C,O, T, S, Y>: CffAst<C,O, T, S, Y> {
    fn r#break(ctx: &mut C,id: u16) -> Self;
    fn r#continue(ctx: &mut C,id: u16) -> Self;
    fn r#loop(&self,ctx: &mut C, id: u16) -> Self;
}
pub trait ExportTerm<A: ExportAst<C,O, T, S, Y>,C, O, T, Y, S>: SaneTerminator<O, T, Y, S> {
    fn go(
        &self,
        ctx: &mut C,
        s: impl FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        body: A,
    ) -> anyhow::Result<A>;
}
impl<
        O,
        T,
        Y,
        S,
        C,
        A: ExportAst<C,O, T, S, Y>,
        V1: ExportTerm<A,C, O, T, Y, S>,
        V2: ExportTerm<A,C, O, T, Y, S>,
    > ExportTerm<A,C, O, T, Y, S> for Either<V1, V2>
{
    fn go(
        &self,
        ctx: &mut C,
        s: impl FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        body: A,
    ) -> anyhow::Result<A>{
        match self {
            Either::Left(a) => a.go(ctx,s, f, body),
            Either::Right(b) => b.go(ctx,s, f, body),
        }
    }
}
impl<O, T, Y: Clone, S: Clone,C, A: ExportAst<C,O, T, S, Y>> ExportTerm<A,C, O, T, Y, S>
    for BlockTarget<O, T, Y, S>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A,
        f: &Func<O, T, Y, S>,
        mut body: A,
    ) -> anyhow::Result<A> {
        let t = once(s.target(ctx, f, self, vec![]));
        body.append(ctx,t);
        return Ok(body);
    }
}
impl<C,B: Bound, A: ExportAst<C,BoundOp<B>, BoundTerm<B>, BoundSelect<B>, BoundType<B>>>
    ExportTerm<A, C,BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        ExportTerm<A, C,BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn go(
        &self,
        ctx: &mut C,
        s: impl FnMut(&mut C,Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>) -> A,
        f: &Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        body: A,
    ) -> anyhow::Result<A> {
        return self.0.go(ctx, s, f, body);
    }
    // fn go(
    //     &self,
    //     s: impl FnMut(Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>) -> A,
    //     f: &Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    //     mut body: A,
    // ) -> anyhow::Result<A> {
    //     return self.0.go(s2, f, body);
    // }
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
pub trait EmitTarget<O, T, Y, S,C, A: ExportAst<C,O, T, S, Y>>:
    FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A
{
    fn target(
        &mut self,
        ctx: &mut C,
        f: &Func<O, T, Y, S>,
        k: &BlockTarget<O, T, Y, S>,
        mut before: Vec<A>,
    ) -> A {
        let tys = k.block_types(f);
        before.extend(k.args.iter().map(|a| {
            let v = A::Var::from(format!("ssa{}", a.value.index()));
            let l = A::get(ctx,v, f.opts[a.value].ty());
            return l.select(ctx,&a.select);
        }));
        let mut r = A::unit(ctx);
        let l = before.iter().zip(tys).enumerate().map(|(i, (x, y))| {
            x.assign(ctx,A::Var::from(format!("bp{i}at{}", k.block.index())), y)
        }).collect::<Vec<_>>();
        r.append(
            ctx,
            l.into_iter(),
        );
        let l = once(self(ctx,k.block));
        r.append(ctx,l);
        return r;
    }
}
impl<O, T, Y, S,C, A: ExportAst<C,O, T, S, Y>, B: FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A>
    EmitTarget<O, T, Y, S,C, A> for B
{
}
pub fn render_bb<O,C, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: ExportAst<C,O, T, S, Y>>(
    ctx: &mut C,
    f: &Func<O, T, Y, S>,
    k: Id<Block<O, T, Y, S>>,
    a2: &mut A,
    go: impl FnMut(&mut C,Id<Block<O, T, Y, S>>) -> A,
) -> anyhow::Result<()> {
    let mut a = A::unit(ctx);
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
                        let l = A::get(ctx,v, f.opts[a.value].ty());
                        return l.select(ctx,&a.select);
                    })
                    .collect::<Vec<_>>();
                let l = A::op(ctx,o, &uvs);
                let l = once(l.assign(ctx,iv.clone(), f.opts[*i].ty()));
                a.append(ctx,l);
            }
            rat_ir::Value::BlockParam(i, b, y2) => {
                let v = A::Var::from(format!("bp{i}at{}", k2.index()));
                let l = A::get(ctx,v, y2);
                let l = once(l.assign(ctx,iv.clone(), y));
                a.append(ctx,l);
            }
            rat_ir::Value::Alias(u, _) => {
                let v = A::Var::from(format!("ssa{}", u.value.index()));
                let s = A::get(ctx,v, f.opts[u.value].ty());
                let s = s.select(ctx,&u.select);
                let s = once(s.assign(ctx,iv.clone(), f.opts[*i].ty()));
                a.append(ctx,s);
            }
        }
    }
    let r = once(f.blocks[k].term.go(ctx,go, f, a)?);
    a2.append(ctx,r);
    return Ok(());
}
pub fn render_cff_block<C,O, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: CffAst<C,O, T, S, Y>>(
    ctx: &mut C,
    f: &Func<O, T, Y, S>,
    k: Id<Block<O, T, Y, S>>,
    a: &mut A,
) -> anyhow::Result<()> {
    render_bb(ctx,f, k, a, |ctx,l| A::br_id(ctx,l.index()))?;
    // a.append(once(f.blocks[k].term.go(, f)?));
    return Ok(());
}
pub fn render_cff<C,O, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: CffAst<C,O, T, S, Y>>(
    ctx: &mut C,
    f: &Func<O, T, Y, S>,
) -> anyhow::Result<A> {
    let m = f
        .blocks
        .iter()
        .map(|(id, _)| {
            let mut a = A::unit(ctx);
            render_cff_block(ctx,f, id, &mut a)?;
            return Ok((id.index(), a));
        })
        .collect::<anyhow::Result<BTreeMap<usize, A>>>()?;
    let mut s = A::set_id(ctx,f.entry.index());
    let l = A::switch(ctx,&m);
    let l = l.forever(ctx);
    s.append(ctx,once(l));
    return Ok(s);
}
pub fn render_relooped_func<C,O, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: ReloopAst<C,O, T, S, Y>>(
    f: &Func<O, T, Y, S>,
    a: &mut A,
    ctx: &mut C,
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
    return render_relooped(ctx,f, x.as_ref(), a);
}
pub fn render_relooped<C,O, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: ReloopAst<C,O, T, S, Y>>(
    ctx: &mut C,
    f: &Func<O, T, Y, S>,
    x: &relooper::ShapedBlock<Id<Block<O, T, Y, S>>>,
    a: &mut A,
) -> anyhow::Result<()> {
    // let mut a = A::unit();
    match x {
        relooper::ShapedBlock::Simple(s) => {
            let k = &f.blocks[s.label];
            render_bb(ctx,f, s.label, a, |ctx,k| match s.branches.get(&k).unwrap() {
                relooper::BranchMode::LoopBreak(i) => A::r#break(ctx,*i),
                relooper::BranchMode::LoopBreakIntoMulti(i) => {
                    let mut b = A::set_id(ctx,k.index());
                    let i = once(A::r#break(ctx,*i));
                    b.append(ctx,i);
                    b
                }
                relooper::BranchMode::LoopContinue(i) => A::r#continue(ctx,*i),
                relooper::BranchMode::LoopContinueIntoMulti(i) => {
                    let mut b = A::set_id(ctx,k.index());
                    let i = once(A::r#break(ctx,*i));
                    b.append(ctx,i);
                    b
                }
                relooper::BranchMode::MergedBranch => A::unit(ctx),
                relooper::BranchMode::MergedBranchIntoMulti => A::set_id(ctx,k.index()),
                relooper::BranchMode::SetLabelAndBreak => A::br_id(ctx,k.index()),
            })?;

            if let Some(i) = s.immediate.as_ref().map(|a| a.as_ref()) {
                render_relooped(ctx,f, i, a)?;
            }
            if let Some(i) = s.next.as_ref().map(|a| a.as_ref()) {
                render_relooped(ctx,f, i, a)?;
            }
        }
        relooper::ShapedBlock::Loop(l) => {
            let mut b = A::unit(ctx);
            render_relooped(ctx,f, l.inner.as_ref(), &mut b)?;
            let l2 = once(b.r#loop(ctx,l.loop_id));
            a.append(ctx,l2);
            if let Some(i) = l.next.as_ref().map(|a| a.as_ref()) {
                render_relooped(ctx,f, i, a)?;
            }
        }
        relooper::ShapedBlock::Multiple(block) => {
            let mut m = BTreeMap::new();
            let mut running = A::unit(ctx);
            // let mut l2 = None;
            for h in block.handled.iter().rev() {
                if h.break_after {
                    running = A::unit(ctx);
                }
                let mut b = A::unit(ctx);
                render_relooped(ctx,f, &h.inner, &mut b)?;
                b.append(ctx,once(running));
                running = b.clone();
                for l in h.labels.iter().rev() {
                    m.insert(l.index(), b.clone());
                    // l2 = Some(*l);
                }
            }
            // let l2 = l2.unwrap();
            let l = once(A::switch(ctx,&m));
            a.append(ctx,l)
        }
    };
    return Ok(());
}
pub fn vars<'a, C,O, T: ExportTerm<A,C, O, T, Y, S>, Y, S, A: ExportAst<C,O, T, S, Y>>(
    f: &'a Func<O, T, Y, S>,
) -> impl Iterator<Item = (A::Var, &'a Y)> + 'a {
    return f
        .opts
        .iter()
        .map(|a| (a.0.ssa::<C,O, T, S, Y, A>(), a.1.ty()))
        .chain(f.blocks.iter().flat_map(|(k, v)| {
            v.params
                .iter()
                .enumerate()
                .map(move |(i, y)| (A::Var::from(format!("bp{i}at{}", k.index())), y))
        }));
}
