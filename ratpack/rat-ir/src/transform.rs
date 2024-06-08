use std::{collections::BTreeMap, marker::PhantomData};

use either::Either;
use id_arena::Id;
pub mod ctx;

use crate::{
    dom::DoMap,
    maxssa::{maxssa, MaxSSA},
    util::{Extract, ExtractIn, If},
    Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Builder, Func,
    SaneTerminator, Use, Value,
};

use self::ctx::NormalTermIn;
pub trait NormalTerm<O, T, Y, S, O2, T2, Y2, S2> {
    type Then;
    fn norm(
        &self,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then;
}
impl<O, T, Y, S: Extract<S2>, O2, T2, Y2, S2, A: NormalTerm<O, T, Y, S, O2, T2, Y2, S2>>
    NormalTerm<O, T, Y, S, O2, T2, Y2, S2> for If<O, T, Y, S, A>
{
    type Then = If<O2, T2, Y2, S2, A::Then>;

    fn norm(
        &self,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        If {
            val: Use {
                value: m.get(&self.val.value).copied().unwrap(),
                select: self.val.select.extract(),
            },
            then: self.then.norm(to_dst, m),
            r#else: self.r#else.as_ref().map(|s| s.norm(to_dst, m)),
        }
    }
}
impl<
        O,
        T,
        Y,
        S,
        O2,
        T2,
        Y2,
        S2,
        A: NormalTerm<O, T, Y, S, O2, T2, Y2, S2>,
        B: NormalTerm<O, T, Y, S, O2, T2, Y2, S2>,
    > NormalTerm<O, T, Y, S, O2, T2, Y2, S2> for Either<A, B>
{
    type Then = Either<A::Then, B::Then>;

    fn norm(
        &self,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        match self {
            Either::Left(a) => Either::Left(a.norm(to_dst, m)),
            Either::Right(b) => Either::Right(b.norm(to_dst, m)),
        }
    }
}
impl<O, T, Y: Extract<Y2>, S: Extract<S2>, O2, T2, Y2, S2> NormalTerm<O, T, Y, S, O2, T2, Y2, S2>
    for BlockTarget<O, T, Y, S>
{
    type Then = BlockTarget<O2, T2, Y2, S2>;

    fn norm(
        &self,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        BlockTarget {
            block: to_dst.get(&self.block).copied().unwrap(),
            args: self
                .args
                .iter()
                .map(|a| Use {
                    value: m.get(&a.value).copied().unwrap(),
                    select: a.select.extract(),
                })
                .collect(),
            prepend: self.prepend.iter().map(|a| a.extract()).collect(),
        }
    }
}
impl<B: Bound, O2, T2, Y2, S2>
    NormalTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, O2, T2, Y2, S2>
    for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        NormalTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>, O2, T2, Y2, S2>,
{
    type Then = <B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> as NormalTerm<
        BoundOp<B>,
        BoundTerm<B>,
        BoundType<B>,
        BoundSelect<B>,
        O2,
        T2,
        Y2,
        S2,
    >>::Then;

    fn norm(
        &self,
        to_dst: &BTreeMap<
            Id<Block<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
            Id<Block<O2, T2, Y2, S2>>,
        >,
        m: &BTreeMap<
            Id<Value<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
            Id<Value<O2, T2, Y2, S2>>,
        >,
    ) -> Self::Then {
        self.0.norm(to_dst, m)
    }
}
pub struct Emit<O, T, Y, S, O2, T2, Y2, S2> {
    pub phantom: PhantomData<fn(O, T, Y, S, O2, T2, Y2, S2)>,
}
pub fn emit<
    O: Extract<O2>,
    T: NormalTerm<O, T, Y, S, O2, T2, Y2, S2, Then = W> + SaneTerminator<O, T, Y, S>,
    Y: Extract<Y2>,
    S: Extract<S2>,
    O2,
    T2: Default,
    Y2: Clone,
    S2,
    W: Extract<T2>,
>(
    a: &mut Func<O, T, Y, S>,
) -> anyhow::Result<Func<O2, T2, Y2, S2>>
where
    Func<O, T, Y, S>: MaxSSA,
{
    let mut x = Default::default();
    let m = Emit {
        phantom: PhantomData,
    }
    .transform_func(a, &mut x)?;
    x.entry = m.get(&a.entry).copied().unwrap();
    return Ok(x);
}
impl<
        O: Extract<O2>,
        T: NormalTerm<O, T, Y, S, O2, T2, Y2, S2, Then = W>,
        Y: Extract<Y2>,
        S: Extract<S2>,
        O2,
        T2,
        Y2: Clone,
        S2,
        W: Extract<T2>,
    > Transformer<O, T, Y, S, O2, T2, Y2, S2> for Emit<O, T, Y, S, O2, T2, Y2, S2>
{
    type Meta = Id<Value<O2, T2, Y2, S2>>;

    type Boot = ();

    fn transform_type(&mut self, y: &Y) -> anyhow::Result<Y2> {
        Ok(y.extract())
    }

    fn transform_blockparam(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        t: Y2,
    ) -> anyhow::Result<Self::Meta> {
        Ok(f.add_blockparam(k, t))
    }

    fn transform_use(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        meta: Self::Meta,
        select: S2,
        y: Y2,
    ) -> anyhow::Result<Self::Meta> {
        let v = f.opts.alloc(Value::Alias(
            Use {
                value: meta,
                select: select,
            },
            y,
        ));
        f.blocks[k].insts.push(v);
        return Ok(v);
    }

    fn transform_select(&mut self, s: &S) -> anyhow::Result<S2> {
        Ok(s.extract())
    }

    fn transform_op(
        &mut self,
        boot: &mut Self::Boot,
        o: &O,
        k: Id<Block<O2, T2, Y2, S2>>,
        params: Vec<(Self::Meta, S2)>,
        ty: Y2,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<(Id<Block<O2, T2, Y2, S2>>, Self::Meta)> {
        let o = o.extract();
        let v = dst.opts.alloc(Value::Operator(
            o,
            params
                .into_iter()
                .map(|(a, b)| Use {
                    value: a,
                    select: b,
                })
                .collect(),
            ty,
            PhantomData,
        ));
        dst.blocks[k].insts.push(v);
        return Ok((k, v));
    }

    fn transform_term(
        &mut self,
        boot: Self::Boot,
        src: &Func<O, T, Y, S>,
        t: &T,
        k: Id<Block<O2, T2, Y2, S2>>,
        dst: &mut Func<O2, T2, Y2, S2>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Self::Meta>,
    ) -> anyhow::Result<()> {
        let x = t.norm(to_dst, m);
        dst.blocks[k].term = x.extract();
        return Ok(());
    }

    fn bootstrap(
        &mut self,
        src: &Func<O, T, Y, S>,
        src_block: Id<Block<O, T, Y, S>>,
        domap: &DoMap<O, T, Y, S>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<Self::Boot> {
        Ok(())
    }
}
#[repr(transparent)]
pub struct EmitIn<O, T, Y, S, O2, T2, Y2, S2, C> {
    pub phantom: PhantomData<fn(O, T, Y, S, O2, T2, Y2, S2)>,
    pub ctx: C,
}
pub fn emit_in<
    O: ExtractIn<C, O2>,
    T: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2, Then = W> + SaneTerminator<O, T, Y, S>,
    Y: ExtractIn<C, Y2>,
    S: ExtractIn<C, S2>,
    O2,
    T2: Default,
    Y2: Clone,
    S2,
    W: ExtractIn<C, T2>,
    C,
    // O2B: Builder<O2,T2,Y2,S2,Result = O2>,
>(
    a: &mut Func<O, T, Y, S>,
    c: &mut C,
) -> anyhow::Result<Func<O2, T2, Y2, S2>>
where
    Func<O, T, Y, S>: MaxSSA,
{
    let mut x = Default::default();
    let mut e: &mut EmitIn<O, T, Y, S, O2, T2, Y2, S2, C> = unsafe { std::mem::transmute(c) };
    let m = e.transform_func(a, &mut x)?;
    x.entry = m.get(&a.entry).copied().unwrap();
    return Ok(x);
}
impl<
        O: ExtractIn<C, O2>,
        T: NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2, Then = W>,
        Y: ExtractIn<C, Y2>,
        S: ExtractIn<C, S2>,
        O2,
        T2,
        Y2: Clone,
        S2,
        W: ExtractIn<C, T2>,
        C,
        // O2B: Builder<O2,T2,Y2,S2,Result = O2>,
    > Transformer<O, T, Y, S, O2, T2, Y2, S2> for EmitIn<O, T, Y, S, O2, T2, Y2, S2, C>
{
    type Meta = Id<Value<O2, T2, Y2, S2>>;

    type Boot = ();

    fn transform_type(&mut self, y: &Y) -> anyhow::Result<Y2> {
        Ok(y.extract_in(&mut self.ctx))
    }

    fn transform_blockparam(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        t: Y2,
    ) -> anyhow::Result<Self::Meta> {
        Ok(f.add_blockparam(k, t))
    }

    fn transform_use(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        meta: Self::Meta,
        select: S2,
        y: Y2,
    ) -> anyhow::Result<Self::Meta> {
        let v = f.opts.alloc(Value::Alias(
            Use {
                value: meta,
                select: select,
            },
            y,
        ));
        f.blocks[k].insts.push(v);
        return Ok(v);
    }

    fn transform_select(&mut self, s: &S) -> anyhow::Result<S2> {
        Ok(s.extract_in(&mut self.ctx))
    }

    fn transform_op(
        &mut self,
        boot: &mut Self::Boot,
        o: &O,
        k: Id<Block<O2, T2, Y2, S2>>,
        params: Vec<(Self::Meta, S2)>,
        ty: Y2,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<(Id<Block<O2, T2, Y2, S2>>, Self::Meta)> {
        let o = o.extract_in(&mut self.ctx);
        // let (o,k) = Box::new(o).build(dst,k)?;
        let v = dst.opts.alloc(Value::Operator(
            o,
            params
                .into_iter()
                .map(|(a, b)| Use {
                    value: a,
                    select: b,
                })
                .collect(),
            ty,
            PhantomData,
        ));
        dst.blocks[k].insts.push(v);
        return Ok((k, v));
    }

    fn transform_term(
        &mut self,
        boot: Self::Boot,
        src: &Func<O, T, Y, S>,
        t: &T,
        k: Id<Block<O2, T2, Y2, S2>>,
        dst: &mut Func<O2, T2, Y2, S2>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Self::Meta>,
    ) -> anyhow::Result<()> {
        let x = t.norm(&mut self.ctx, to_dst, m);
        dst.blocks[k].term = x.extract_in(&mut self.ctx);
        return Ok(());
    }

    fn bootstrap(
        &mut self,
        src: &Func<O, T, Y, S>,
        src_block: Id<Block<O, T, Y, S>>,
        domap: &DoMap<O, T, Y, S>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<Self::Boot> {
        Ok(())
    }
}
pub trait Transformer<O, T, Y, S, O2, T2, Y2, S2> {
    type Meta: Clone;
    type Boot;
    fn transform_type(&mut self, y: &Y) -> anyhow::Result<Y2>;
    fn transform_blockparam(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        t: Y2,
    ) -> anyhow::Result<Self::Meta>;
    fn transform_use(
        &mut self,
        f: &mut Func<O2, T2, Y2, S2>,
        k: Id<Block<O2, T2, Y2, S2>>,
        meta: Self::Meta,
        select: S2,
        y: Y2,
    ) -> anyhow::Result<Self::Meta>;
    fn transform_select(&mut self, s: &S) -> anyhow::Result<S2>;
    fn transform_op(
        &mut self,
        boot: &mut Self::Boot,
        o: &O,
        k: Id<Block<O2, T2, Y2, S2>>,
        params: Vec<(Self::Meta, S2)>,
        ty: Y2,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<(Id<Block<O2, T2, Y2, S2>>, Self::Meta)>;
    fn transform_term(
        &mut self,
        boot: Self::Boot,
        src: &Func<O, T, Y, S>,
        t: &T,
        k: Id<Block<O2, T2, Y2, S2>>,
        dst: &mut Func<O2, T2, Y2, S2>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        m: &BTreeMap<Id<Value<O, T, Y, S>>, Self::Meta>,
    ) -> anyhow::Result<()>;
    fn bootstrap(
        &mut self,
        src: &Func<O, T, Y, S>,
        src_block: Id<Block<O, T, Y, S>>,
        domap: &DoMap<O, T, Y, S>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<Self::Boot>;
    fn transform_maxssa(
        &mut self,
        src: &mut Func<O, T, Y, S>,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>>
    where
        Y2: Clone,
        T2: Default,
        T: SaneTerminator<O, T, Y, S>,
    {
        let d = src.domap();
        let mut to_dst = BTreeMap::new();
        for (k, _) in src.blocks.iter() {
            to_dst.insert(k, dst.blocks.alloc(Default::default()));
        }
        for (k, v) in to_dst.iter() {
            self.transform_block(src, *k, dst, &to_dst, &d)?;
        }
        // dst.entry = to_dst.get(&src.entry).copied().unwrap();
        return Ok(to_dst);
    }
    fn transform_func(
        &mut self,
        src: &mut Func<O, T, Y, S>,
        dst: &mut Func<O2, T2, Y2, S2>,
    ) -> anyhow::Result<BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>>
    where
        Y2: Clone,
        T2: Default,
        T: SaneTerminator<O, T, Y, S>,
        Func<O, T, Y, S>: MaxSSA,
    {
        src.maxssa();
        return self.transform_maxssa(src, dst);
    }
    fn transform_block(
        &mut self,
        src: &Func<O, T, Y, S>,
        src_block: Id<Block<O, T, Y, S>>,
        dst: &mut Func<O2, T2, Y2, S2>,
        to_dst: &BTreeMap<Id<Block<O, T, Y, S>>, Id<Block<O2, T2, Y2, S2>>>,
        domap: &DoMap<O, T, Y, S>,
    ) -> anyhow::Result<()>
    where
        Y2: Clone,
    {
        let mut dst_block = to_dst.get(&src_block).copied().unwrap();
        let params = src.blocks[src_block]
            .params
            .iter()
            .map(|a| {
                let b = self.transform_type(a)?;
                Ok(self.transform_blockparam(dst, dst_block, b)?)
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        let mut b = self.bootstrap(src, src_block, domap, to_dst, dst)?;
        let mut m = BTreeMap::new();
        for v in src.blocks[src_block].insts.clone() {
            let v2 = &src.opts[v];
            let w = match v2 {
                crate::Value::Operator(o, u, y, _) => {
                    let u = u
                        .iter()
                        .map(|u| {
                            Ok((
                                m.get(&u.value).cloned().unwrap(),
                                self.transform_select(&u.select)?,
                            ))
                        })
                        .collect::<anyhow::Result<Vec<(Self::Meta, S2)>>>()?;
                    let y = self.transform_type(y)?;
                    let (n, v) = self.transform_op(&mut b, o, dst_block, u, y, dst)?;
                    dst_block = n;
                    v
                }
                crate::Value::BlockParam(i, _, _) => params[*i].clone(),
                crate::Value::Alias(u, t) => {
                    // let w = dst.opts.alloc(Value::Alias(
                    //     crate::Use {
                    //         value: m.get(&u.value).copied().unwrap(),
                    //         select: self.transform_select(&u.select)?,
                    //     },
                    //     self.transform_type(t)?,
                    // ));
                    // dst.blocks[dst_block].insts.push(w);
                    let select = self.transform_select(&u.select)?;
                    let t = self.transform_type(t)?;
                    self.transform_use(
                        dst,
                        dst_block,
                        m.get(&u.value).cloned().unwrap(),
                        select,
                        t,
                    )?
                }
            };
            m.insert(v, w);
        }
        self.transform_term(
            b,
            src,
            &src.blocks[src_block].term,
            dst_block,
            dst,
            &*to_dst,
            &m,
        )?;
        return Ok(());
    }
}
