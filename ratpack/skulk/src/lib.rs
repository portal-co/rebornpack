use std::{iter::once, marker::PhantomData};

use anyhow::Context;
use either::Either;
use id_arena::Id;
use lexpr::cons::ListIter;
use rat_ast::import::{Get, Set, SetDirect, VarBuilder};
use rat_ir::{
    util::{BinOp, Bt, Push},
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Unit, Use, Value,
};
pub mod util;

pub trait LispOp<O, T, Y, S>: Clone {
    fn lisp(x: &lexpr::Value) -> anyhow::Result<Self>;
}

impl<B: Bound> LispOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        LispOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn lisp(x: &lexpr::Value) -> anyhow::Result<Self> {
        Ok(BoundOp(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::lisp(x)?))
    }
}
impl<B: Bound> LispOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundType<B>
where
    B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        LispOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn lisp(x: &lexpr::Value) -> anyhow::Result<Self> {
        Ok(BoundType(B::Y::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::lisp(x)?))
    }
}
impl<O, T, Y, S, A: LispOp<O, T, Y, S>, B: LispOp<O, T, Y, S>> LispOp<O, T, Y, S> for Either<A, B> {
    fn lisp(x: &lexpr::Value) -> anyhow::Result<Self> {
        let ae = match A::lisp(x) {
            Ok(a) => return Ok(Either::Left(a)),
            Err(e) => e,
        };
        let be = match B::lisp(x) {
            Ok(a) => return Ok(Either::Right(a)),
            Err(e) => e,
        };
        return Err(anyhow::anyhow!("{ae}; {be}"));
    }
}
pub struct Loop<O, T, Y, S> {
    pub r#break: Id<Block<O, T, Y, S>>,
    pub r#continue: Id<Block<O, T, Y, S>>,
}
impl<O, T, Y, S> Clone for Loop<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            r#break: self.r#break.clone(),
            r#continue: self.r#continue.clone(),
        }
    }
}
pub trait LispSeed<'a, O, T, Y, S, U> {
    fn bind<'b>(
        &self,
        go: &'b mut impl TermTargetFn<'a, O, T, Y, S>,
    ) -> anyhow::Result<Box<dyn VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>, Result = U> + 'a>>;
}
impl<'a,O: LispOp<O,T,Y,S> + 'static,T: LispTerm<O,T,Y,S> + 'static,Y: LispOp<O,T,Y,S> + 'static,S: Default + 'static> LispSeed<'a,O,T,Y,S,BlockTarget<O,T,Y,S>> for ListIter<'a>{
    fn bind(
        &self,
        mut go: &mut impl TermTargetFn<'a, O, T, Y, S>,
    ) -> anyhow::Result<Box<dyn VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>, Result = U> + 'a>> {
        let g = go(self.clone())?;
        Ok(Box::new(g))
    }
}
impl<'a, O, T, Y, S, U, V, A: LispSeed<'a, O, T, Y, S, U>, B: LispSeed<'a, O, T, Y, S, V>>
    LispSeed<'a, O, T, Y, S, Either<U, V>> for Either<A, B>
{
    fn bind(
        &self,
        go: &mut impl TermTargetFn<'a, O, T, Y, S>,
    ) -> Result<Box<(dyn VarBuilder<O, T, Y, S, Id<rat_ir::Value<O, T, Y, S>>, Result = Either<U, V>> + 'a)>, anyhow::Error> {
        Box::new(match self {
            Either::Left(a) => a
                .bind(go)
                .map(|a| Box::new(a).then(|x| Unit(Either::Left(x))))
                .map(Either::Left),
            Either::Right(b) => b
                .bind(go)
                .map(|a| Box::new(a).then(|x| Unit(Either::Right(x))))
                .map(Either::Right),
        })
    }
}
pub struct _Bind<X>(pub X);
impl<
        'a,
        B: Bound,
        S: LispSeed<
            'a,
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
            B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        >,
    > LispSeed<'a, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, BoundTerm<B>>
    for _Bind<S>
{
    fn bind(
        &self,
        go: &mut impl TermTargetFn<'a, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    ) -> anyhow::Result<
        impl VarBuilder<
                BoundOp<B>,
                BoundTerm<B>,
                BoundType<B>,
                BoundSelect<B>,
                Id<Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
                Result = BoundTerm<B>,
            > + 'a,
    > {
        self.0
            .bind(go)
            .map(|a| Box::new(a).then(|x| Unit(BoundTerm(x))))
    }
}
pub trait LispTerm<O, T, Y, S>: Push<BlockTarget<O, T, Y, S>> + Sized {
    fn parse<'a>(x: &'a lexpr::Value) -> anyhow::Result<impl LispSeed<'a, O, T, Y, S, Self> + 'a>;
}
impl<O, T, Y, S, A: LispTerm<O, T, Y, S>, B: LispTerm<O, T, Y, S>> LispTerm<O, T, Y, S>
    for Either<A, B>
{
    fn parse<'a>(x: &'a lexpr::Value) -> anyhow::Result<impl LispSeed<'a, O, T, Y, S, Self> + 'a> {
        let ae = match A::parse(x) {
            Ok(a) => return Ok(Either::Left(a)),
            Err(e) => e,
        };
        let be = match B::parse(x) {
            Ok(a) => return Ok(Either::Right(a)),
            Err(e) => e,
        };
        return Err(anyhow::anyhow!("{ae}; {be}"));
    }
}
impl<B: Bound> LispTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        LispTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn parse<'a>(
        x: &'a lexpr::Value,
    ) -> anyhow::Result<
        impl LispSeed<'a, BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, Self> + 'a,
    > {
        Ok(_Bind(B::T::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::parse(x)?))
    }
}
pub fn lex<
    'a,
    O: LispOp<O, T, Y, S> + 'a,
    T: LispTerm<O, T, Y, S> + 'static,
    Y: LispOp<O, T, Y, S> + 'a,
    S: Default + 'a,
>(
    x: &'a lexpr::Value,
    loop_stack: Vec<Loop<O, T, Y, S>>,
) -> anyhow::Result<
    Box<dyn VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>, Result = Id<Value<O, T, Y, S>>> + 'a>,
> {
    Ok(match x {
        lexpr::Value::Nil => todo!(),
        lexpr::Value::Null => todo!(),
        lexpr::Value::Bool(_) => todo!(),
        lexpr::Value::Number(a) => Box::new(Get {
            v: a.as_u64().context("in getting a var")? as usize,
        }),
        lexpr::Value::Char(_) => todo!(),
        lexpr::Value::String(_) => todo!(),
        lexpr::Value::Symbol(_) => todo!(),
        lexpr::Value::Keyword(_) => todo!(),
        lexpr::Value::Bytes(_) => todo!(),
        lexpr::Value::Cons(k) => {
            let mut v = k.list_iter();
            let h = v.next().context("in getting the head")?;
            if let lexpr::Value::Number(a) = h {
                return Ok(Box::new(SetDirect {
                    v: a.as_u64().context("in getting a var")? as usize,
                    wrapped: lex(v.next().context("in getting the value")?, loop_stack)?,
                }));
            }
            let y = Y::lisp(h)?;
            let h = v.next().context("in getting the head")?;
            if let lexpr::Value::Symbol(s) | lexpr::Value::Keyword(s) = h {
                if &**s == "loop" {
                    return Ok(Box::new(LoopOp {
                        ty: y,
                        all: v,
                        loop_stack,
                    }));
                }
                if &**s == "continue" {
                    let Some(lexpr::Value::Number(n)) = v.next() else {
                        anyhow::bail!("invalid label")
                    };
                    return Ok(Box::new(Continue {
                        ty: y,
                        block: loop_stack
                            [loop_stack.len() - (n.as_u64().context("in getting a var")? as usize)]
                            .r#continue,
                        phantom: PhantomData,
                    }));
                }
                if &**s == "break" {
                    let Some(lexpr::Value::Number(n)) = v.next() else {
                        anyhow::bail!("invalid label")
                    };
                    return Ok(Box::new(Break {
                        ty: y,
                        block: loop_stack
                            [loop_stack.len() - (n.as_u64().context("in getting a var")? as usize)]
                            .r#continue,
                        phantom: PhantomData,
                        loop_stack,
                        wrapped: v,
                    }));
                }
            }
            if let Ok(o) = O::lisp(h) {
                return Ok(Box::new(X {
                    op: o,
                    ty: y,
                    all: v,
                    loop_stack,
                }));
            }
            if let Ok(s) = T::parse(x) {
                return Ok(Box::new(Seed {
                    loop_stack,
                    seed: s,
                    ty: y,
                    phantom: PhantomData,
                }));
            }
            anyhow::bail!("invalid header")
        }
        lexpr::Value::Vector(_) => todo!(),
    })
}
pub struct Continue<'a, O, T, Y, S> {
    pub block: Id<Block<O, T, Y, S>>,
    pub phantom: PhantomData<fn() -> &'a ()>,
    pub ty: Y,
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for Continue<'a, O, T, Y, S>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        func.blocks[root].term = Some(T::bt(BlockTarget {
            block: self.block,
            args: vars
                .iter()
                .map(|a| Use {
                    value: *a,
                    select: Default::default(),
                })
                .collect(),
            prepend: vec![],
        }));
        let null = func.blocks.alloc(Default::default());
        let null_val = func.add_blockparam(null, self.ty);
        return Ok((null_val, null));
    }
}
pub struct Break<'a, O, T, Y, S> {
    pub block: Id<Block<O, T, Y, S>>,
    pub loop_stack: Vec<Loop<O, T, Y, S>>,
    pub phantom: PhantomData<fn() -> &'a ()>,
    pub ty: Y,
    pub wrapped: ListIter<'a>,
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for Break<'a, O, T, Y, S>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        mut root: Id<Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let mut b = None;
        for a in self.wrapped {
            let (a, r2) = lex(a, self.loop_stack.clone())?.build_with_vars(func, root, vars)?;
            root = r2;
            b = Some(a);
        }
        let a = b.context("empty loops not supported")?;
        func.blocks[root].term = Some(T::bt(BlockTarget {
            block: self.block,
            args: vars
                .iter()
                .map(|a| Use {
                    value: *a,
                    select: Default::default(),
                })
                .chain(once(Use {
                    value: a,
                    select: Default::default(),
                }))
                .collect(),
            prepend: vec![],
        }));
        let null = func.blocks.alloc(Default::default());
        let null_val = func.add_blockparam(null, self.ty);
        return Ok((null_val, null));
    }
}
pub trait TermTargetFn<'a, O, T, Y, S>:
    FnMut(ListIter<'a>) -> anyhow::Result<EndOp<'a, O, T, Y, S>> + 'a
{
}
impl<'a, O, T, Y, S, X: FnMut(ListIter<'a>) -> anyhow::Result<EndOp<'a, O, T, Y, S>> + 'a>
    TermTargetFn<'a, O, T, Y, S> for X
{
}
pub struct EndOp<'a, O, T, Y, S> {
    pub loop_stack: Vec<Loop<O, T, Y, S>>,
    pub all: ListIter<'a>,
    pub next: Id<Block<O, T, Y, S>>,
}
pub struct Seed<'a, O, T, Y, S, X> {
    pub loop_stack: Vec<Loop<O, T, Y, S>>,
    pub seed: X,
    pub ty: Y,
    pub phantom: PhantomData<fn() -> &'a ()>,
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
        X: LispSeed<'a, O, T, Y, S, T> + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for Seed<'a, O, T, Y, S, X>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let next = func.blocks.alloc(Default::default());
        let stack = self.loop_stack.clone();
        let (a, b) = Box::new(self.seed.bind(&mut move |t| {
            Ok(EndOp {
                all: t,
                next: next.clone(),
                loop_stack: stack.clone(),
            })
        })?)
        .build_with_vars(func, root, vars)?;
        func.blocks[b].term = Some(a);
        for v in vars.iter_mut() {
            let w = func.opts[*v].ty().clone();
            *v = func.add_blockparam(next, w);
        }
        let b = func.add_blockparam(next, self.ty);
        return Ok((b, next));
    }
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for EndOp<'a, O, T, Y, S>
{
    type Result = BlockTarget<O, T, Y, S>;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let mut s = root;
        let mut b = None;
        for a in self.all {
            let (a, r2) = lex(a, self.loop_stack.clone())?.build_with_vars(func, root, vars)?;
            s = r2;
            b = Some(a);
        }
        let b = b.context("empty loops not supported")?;
        let mut t = BlockTarget {
            block: self.next,
            args: vec![],
            prepend: vec![],
        };
        for v in vars.iter_mut() {
            t.args.push(Use {
                value: *v,
                select: Default::default(),
            });
            // let w = func.opts[*v].ty().clone();
            // *v = func.add_blockparam(self.next, w);
        }
        t.args.push(Use {
            value: b,
            select: Default::default(),
        });
        return Ok((t, s));
    }
}
pub struct LoopOp<'a, O, T, Y, S> {
    pub ty: Y,
    pub all: ListIter<'a>,
    pub loop_stack: Vec<Loop<O, T, Y, S>>,
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for LoopOp<'a, O, T, Y, S>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build_with_vars(
        mut self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        root: Id<Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
        let br = func.blocks.alloc(Default::default());
        let mut t = BlockTarget {
            block: br,
            args: vec![],
            prepend: vec![],
        };
        for v in vars.iter_mut() {
            t.args.push(Use {
                value: *v,
                select: Default::default(),
            });
            let w = func.opts[*v].ty().clone();
            *v = func.add_blockparam(br, w);
        }
        func.blocks[root].term = Some(T::bt(t));
        let br2 = func.blocks.alloc(Default::default());
        let l = Loop {
            r#break: br2,
            r#continue: br,
        };
        self.loop_stack.push(l);
        let mut s = br;
        let mut b = None;
        for a in self.all {
            let (a, r2) = lex(a, self.loop_stack.clone())?.build_with_vars(func, root, vars)?;
            s = r2;
            b = Some(a);
        }
        let b = b.context("empty loops not supported")?;
        let mut t = BlockTarget {
            block: br2,
            args: vec![],
            prepend: vec![],
        };
        for v in vars.iter_mut() {
            t.args.push(Use {
                value: *v,
                select: Default::default(),
            });
            let w = func.opts[*v].ty().clone();
            *v = func.add_blockparam(br2, w);
        }
        t.args.push(Use {
            value: b,
            select: Default::default(),
        });
        let w = func.add_blockparam(br2, self.ty);
        func.blocks[s].term = Some(T::bt(t));
        return Ok((w, br2));
    }
}
pub struct X<'a, O, T, Y, S> {
    pub op: O,
    pub ty: Y,
    pub all: ListIter<'a>,
    pub loop_stack: Vec<Loop<O, T, Y, S>>,
}
impl<
        'a,
        O: LispOp<O, T, Y, S> + 'a,
        T: LispTerm<O, T, Y, S> + 'static,
        Y: LispOp<O, T, Y, S> + 'a,
        S: Default + 'a,
    > VarBuilder<O, T, Y, S, Id<Value<O, T, Y, S>>> for X<'a, O, T, Y, S>
{
    type Result = Id<Value<O, T, Y, S>>;

    fn build_with_vars(
        self: Box<Self>,
        func: &mut rat_ir::Func<O, T, Y, S>,
        mut root: Id<rat_ir::Block<O, T, Y, S>>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result, Id<rat_ir::Block<O, T, Y, S>>)> {
        let mut xs = vec![];
        for y in self.all {
            let (a, r2) = lex(y, self.loop_stack.clone())?.build_with_vars(func, root, vars)?;
            root = r2;
            xs.push(Use {
                value: a,
                select: Default::default(),
            });
        }
        let v = func
            .opts
            .alloc(Value::Operator(self.op, xs, self.ty, PhantomData));
        func.blocks[root].insts.push(v);
        return Ok((v, root));
    }
}
