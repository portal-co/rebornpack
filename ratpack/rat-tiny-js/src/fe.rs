use std::{
    collections::{BTreeMap, HashSet},
    f32::consts::E,
    marker::PhantomData,
    sync::Arc,
};

use anyhow::Context;
use id_arena::Id;
use rat_ir::{
    no_push, Block, BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Func, Use,
    Value,
};
use swc_core::{
    atoms::Atom,
    ecma::ast::{BinaryOp, Decl, Expr, FnDecl, Ident, ModuleDecl, ModuleItem, Pat, Stmt, UnaryOp},
};
pub fn decls<'a>(m: &'a swc_core::ecma::ast::Module) -> impl Iterator<Item = &'a Decl> {
    return m.body.iter().flat_map(|x| match x {
        ModuleItem::ModuleDecl(m) => m.as_export_decl().map(|a| &a.decl),
        ModuleItem::Stmt(s) => s.as_decl(),
    });
}
pub mod basic;
pub struct ModCtx<O, T, Y, S, D> {
    pub phantom: PhantomData<(O, T, S, Y, D)>,
    pub funcs: BTreeMap<Atom, Id<Func<O, T, Y, S>>>,
}
pub enum Mapper<'a, O, T, Y, S, D> {
    Func(Id<Func<O, T, Y, S>>, PhantomData<D>, &'a FnDecl),
    Intrinsic(O),
}
pub trait TinyOp<O, T, Y, S>: Clone {
    fn null() -> anyhow::Result<Self>;
    fn call(a: Id<Func<O, T, Y, S>>) -> anyhow::Result<Self>;
    fn intrinsic(module: Atom, name: Atom) -> anyhow::Result<Self>;
    fn bin(o: &BinaryOp) -> anyhow::Result<Self>;
    fn unary(a: &UnaryOp) -> anyhow::Result<Self>;
    fn str(a: Atom) -> anyhow::Result<Self>;
    fn num(a: f64) -> anyhow::Result<Self>;
}
impl<B: Bound> TinyOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        TinyOp<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn null() -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::null()?))
    }

    fn call(
        a: Id<Func<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>>,
    ) -> anyhow::Result<Self> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::call(a)?))
    }

    fn intrinsic(module: Atom, name: Atom) -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::intrinsic(module, name)?))
    }

    fn bin(o: &BinaryOp) -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::bin(o)?))
    }

    fn unary(a: &UnaryOp) -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::unary(a)?))
    }

    fn str(a: Atom) -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::str(a)?))
    }

    fn num(a: f64) -> anyhow::Result<BoundOp<B>> {
        Ok(Self(B::O::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::num(a)?))
    }
}
pub enum BasicTinyOp<O, T, Y, S> {
    Null,
    Call(Id<Func<O, T, Y, S>>),
    Intrinsic { module: Atom, name: Atom },
    Bin(BinaryOp),
    Unary(UnaryOp),
    Str(Atom),
    Num(f64),
}
no_push!(
    type BasicTinyOp<O, T, Y, S>;
);
no_push!(
    type BasicTinyTerm<O, T, Y, S>;
);

impl<O, T, Y, S> Clone for BasicTinyOp<O, T, Y, S> {
    fn clone(&self) -> Self {
        match self {
            Self::Null => Self::Null,
            Self::Call(arg0) => Self::Call(arg0.clone()),
            Self::Intrinsic { module, name } => Self::Intrinsic {
                module: module.clone(),
                name: name.clone(),
            },
            Self::Bin(arg0) => Self::Bin(arg0.clone()),
            Self::Unary(arg0) => Self::Unary(arg0.clone()),
            Self::Str(arg0) => Self::Str(arg0.clone()),
            Self::Num(arg0) => Self::Num(arg0.clone()),
        }
    }
}
impl<O, T, Y, S> TinyOp<O, T, Y, S> for BasicTinyOp<O, T, Y, S> {
    fn null() -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Null)
    }

    fn call(a: Id<Func<O, T, Y, S>>) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Call(a))
    }

    fn intrinsic(module: Atom, name: Atom) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Intrinsic { module, name })
    }

    fn bin(o: &BinaryOp) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Bin(o.clone()))
    }

    fn unary(a: &UnaryOp) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Unary(a.clone()))
    }

    fn str(a: Atom) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Str(a))
    }

    fn num(a: f64) -> anyhow::Result<BasicTinyOp<O, T, Y, S>> {
        Ok(Self::Num(a))
    }
}
pub trait TinyTerm<O, T, Y, S>: Default {
    fn r#if(
        a: Use<O, T, Y, S>,
        if_true: BlockTarget<O, T, Y, S>,
        if_false: BlockTarget<O, T, Y, S>,
    ) -> anyhow::Result<Self>;
    fn just(a: BlockTarget<O, T, Y, S>) -> anyhow::Result<Self>;
    fn ret(a: Use<O, T, Y, S>) -> anyhow::Result<Self>;
}
impl<B: Bound> TinyTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>> for BoundTerm<B>
where
    B::T<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>:
        TinyTerm<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
{
    fn r#if(
        a: Use<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        if_true: BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
        if_false: BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    ) -> anyhow::Result<BoundTerm<B>> {
        Ok(Self(B::T::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::r#if(a, if_true, if_false)?))
    }

    fn just(
        a: BlockTarget<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    ) -> anyhow::Result<BoundTerm<B>> {
        Ok(Self(B::T::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::just(a)?))
    }

    fn ret(
        a: Use<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>,
    ) -> anyhow::Result<BoundTerm<B>> {
        Ok(Self(B::T::<
            BoundOp<B>,
            BoundTerm<B>,
            BoundType<B>,
            BoundSelect<B>,
        >::ret(a)?))
    }
}
pub enum BasicTinyTerm<O, T, Y, S> {
    Panic,
    Just(BlockTarget<O, T, Y, S>),
    Ret(Use<O, T, Y, S>),
    If {
        cond: Use<O, T, Y, S>,
        if_true: BlockTarget<O, T, Y, S>,
        if_false: BlockTarget<O, T, Y, S>,
    },
}
impl<O, T, Y, S> Default for BasicTinyTerm<O, T, Y, S> {
    fn default() -> Self {
        Self::Panic
    }
}
impl<O, T, Y: Clone, S: Clone> Clone for BasicTinyTerm<O, T, Y, S> {
    fn clone(&self) -> Self {
        match self {
            Self::Panic => Self::Panic,
            Self::Just(arg0) => Self::Just(arg0.clone()),
            Self::Ret(arg0) => Self::Ret(arg0.clone()),
            Self::If {
                cond,
                if_true,
                if_false,
            } => Self::If {
                cond: cond.clone(),
                if_true: if_true.clone(),
                if_false: if_false.clone(),
            },
        }
    }
}
impl<O, T, Y, S> TinyTerm<O, T, Y, S> for BasicTinyTerm<O, T, Y, S> {
    fn r#if(
        a: Use<O, T, Y, S>,
        if_true: BlockTarget<O, T, Y, S>,
        if_false: BlockTarget<O, T, Y, S>,
    ) -> anyhow::Result<BasicTinyTerm<O, T, Y, S>> {
        Ok(Self::If {
            cond: a,
            if_true,
            if_false,
        })
    }

    fn just(a: BlockTarget<O, T, Y, S>) -> anyhow::Result<BasicTinyTerm<O, T, Y, S>> {
        Ok(Self::Just(a))
    }

    fn ret(a: Use<O, T, Y, S>) -> anyhow::Result<BasicTinyTerm<O, T, Y, S>> {
        Ok(Self::Ret(a))
    }
}
pub fn funcs<
    O: TinyOp<O, T, Y, S> + 'static,
    T: TinyTerm<O, T, Y, S> + 'static,
    Y: Clone + Default + 'static,
    S: Default + 'static,
    D: 'static,
>(
    m: &swc_core::ecma::ast::Module,
    n: &mut rat_ir::module::Module<O, T, Y, S, D>,
) -> anyhow::Result<ModCtx<O, T, Y, S, D>> {
    let mut ctx: ModCtx<O, T, Y, S, D> = ModCtx {
        phantom: PhantomData,
        funcs: BTreeMap::new(),
    };
    let mut mapper = BTreeMap::new();
    for d in decls(m) {
        match d {
            Decl::Class(_) => todo!(),
            Decl::Fn(f) => {
                let m = Mapper::Func(n.funcs.alloc(Default::default()), PhantomData::<D>, f);
                mapper.insert(f.ident.sym.clone(), m);
            }
            Decl::Var(_) => todo!(),
            Decl::Using(_) => todo!(),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
    }
    for i in m
        .body
        .iter()
        .flat_map(|a| a.as_module_decl())
        .flat_map(|a| a.as_import())
    {
        for x in i.specifiers.iter() {
            match x {
                swc_core::ecma::ast::ImportSpecifier::Named(a) => {
                    let n = a.local.sym.clone();
                    mapper.insert(
                        n,
                        Mapper::Intrinsic(
                            O::intrinsic(
                                i.src.value.clone(),
                                a.imported
                                    .as_ref()
                                    .and_then(|x| match x {
                                        swc_core::ecma::ast::ModuleExportName::Ident(a) => {
                                            Some(a.sym.clone())
                                        }
                                        swc_core::ecma::ast::ModuleExportName::Str(_) => None,
                                    })
                                    .context("in getting the import name")?,
                            )
                            .context("in getting the intrinsic")?,
                        ),
                    );
                }
                swc_core::ecma::ast::ImportSpecifier::Default(_) => todo!(),
                swc_core::ecma::ast::ImportSpecifier::Namespace(_) => todo!(),
            }
        }
    }
    for (_, m) in mapper.iter() {
        match m {
            Mapper::Func(a, _, c) => {
                let e = n.funcs[*a].entry;
                func(&**c, *a, &mapper, n, Arc::new(T::ret), e)?;
            }
            Mapper::Intrinsic(_) => {}
        }
    }
    for x in m
        .body
        .iter()
        .flat_map(|x| x.as_module_decl())
        .flat_map(|x| x.as_export_decl())
    {
        match &x.decl {
            Decl::Class(_) => todo!(),
            Decl::Fn(f) => {
                let s = f.ident.sym.clone();
                let f = mapper.get(&s).unwrap();
                if let Mapper::Func(a, b, c) = f {
                    ctx.funcs.insert(s, *a);
                }
            }
            Decl::Var(_) => todo!(),
            Decl::Using(_) => todo!(),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
    }
    return Ok(ctx);
}
fn func<
    O: TinyOp<O, T, Y, S> + 'static,
    T: TinyTerm<O, T, Y, S> + 'static,
    Y: Clone + Default + 'static,
    S: Default + 'static,
    D: 'static,
>(
    c: &FnDecl,
    a: Id<Func<O, T, Y, S>>,
    m: &BTreeMap<Atom, Mapper<'_, O, T, Y, S, D>>,
    n: &mut rat_ir::module::Module<O, T, Y, S, D>,
    ret: Arc<dyn Fn(Use<O, T, Y, S>) -> anyhow::Result<T>>,
    mut k: Id<Block<O, T, Y, S>>,
) -> anyhow::Result<()> {
    let params: BTreeMap<Atom, _> = c
        .function
        .params
        .iter()
        .flat_map(|x| {
            let e = n.funcs[a].entry;
            let p = n.funcs[a].add_blockparam(e, Y::default());
            Some((x.pat.as_ident()?.id.sym.clone(), p))
        })
        .collect();
    let vars: HashSet<Atom, _> = swc_ecma_utils::collect_decls(&*c);
    let snapshot: Vec<_> = vars.iter().cloned().collect();
    let init: BTreeMap<Atom, _> = snapshot
        .iter()
        .map(|x| {
            Ok((
                x.clone(),
                match params.get(x) {
                    Some(a) => *a,
                    None => {
                        let v = n.funcs[a].opts.alloc(rat_ir::Value::Operator(
                            O::null().context("in getting null")?,
                            vec![],
                            Default::default(),
                            PhantomData,
                        ));
                        n.funcs[a].blocks[k].insts.push(v);
                        v
                    }
                },
            ))
        })
        .collect::<anyhow::Result<BTreeMap<Atom, _>>>()?;
    let mut init: Vec<_> = snapshot
        .iter()
        .flat_map(|x| init.get(x))
        .map(|a| a.clone())
        .collect();
    let snapshot: BTreeMap<_, usize> = snapshot
        .into_iter()
        .enumerate()
        .map(|(a, b)| (b, a))
        .collect();
    let ctx2 = RCtx {
        phantom: PhantomData,
        ret,
        loops: BTreeMap::new(),
    };
    for x in c
        .function
        .body
        .as_ref()
        .context("in getting the fn body")?
        .stmts
        .iter()
    {
        let (_, k2) = x.ast_compile(&ctx2, c, a, m, n, k, &snapshot, &mut init)?;
        k = k2;
    }
    let v = n.funcs[a].opts.alloc(rat_ir::Value::Operator(
        O::null().context("in getting null")?,
        vec![],
        Default::default(),
        PhantomData,
    ));
    n.funcs[a].blocks[k].insts.push(v);
    n.funcs[a].blocks[k].term = (ctx2.ret)(Use {
        value: v,
        select: S::default(),
    })
    .context("in getting ret")?;
    return Ok(());
}
pub struct RCtx<O: TinyOp<O, T, Y, S>, T: TinyTerm<O, T, Y, S>, Y: Clone + Default, S: Default, D> {
    pub phantom: PhantomData<fn(O, T, Y, S, D)>,
    pub ret: Arc<dyn Fn(Use<O, T, Y, S>) -> anyhow::Result<T>>,
    pub loops: BTreeMap<Option<Atom>, BC<O, T, Y, S>>,
}
impl<O: TinyOp<O, T, Y, S>, T: TinyTerm<O, T, Y, S>, Y: Clone + Default, S: Default, D> Clone
    for RCtx<O, T, Y, S, D>
{
    fn clone(&self) -> Self {
        Self {
            phantom: self.phantom.clone(),
            ret: self.ret.clone(),
            loops: self.loops.clone(),
        }
    }
}
pub struct BC<O, T, Y, S> {
    pub r#break: Id<Block<O, T, Y, S>>,
    pub r#continue: Id<Block<O, T, Y, S>>,
}
impl<O, T, Y, S> Clone for BC<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            r#break: self.r#break.clone(),
            r#continue: self.r#continue.clone(),
        }
    }
}
pub trait XAst {
    type Result<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    >;
    fn ast_compile<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    >(
        &self,
        ctx: &RCtx<O, T, Y, S, D>,
        c: &FnDecl,
        a: Id<Func<O, T, Y, S>>,
        m: &BTreeMap<Atom, Mapper<'_, O, T, Y, S, D>>,
        n: &mut rat_ir::module::Module<O, T, Y, S, D>,
        k: Id<Block<O, T, Y, S>>,
        ids: &BTreeMap<Atom, usize>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result<O, T, Y, S, D>, Id<Block<O, T, Y, S>>)>;
}
impl XAst for Stmt {
    type Result<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    > = ();

    fn ast_compile<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    >(
        &self,
        ctx: &RCtx<O, T, Y, S, D>,
        c: &FnDecl,
        a: Id<Func<O, T, Y, S>>,
        m: &BTreeMap<Atom, Mapper<'_, O, T, Y, S, D>>,
        n: &mut rat_ir::module::Module<O, T, Y, S, D>,
        mut k: Id<Block<O, T, Y, S>>,
        ids: &BTreeMap<Atom, usize>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result<O, T, Y, S, D>, Id<Block<O, T, Y, S>>)> {
        match self {
            Stmt::Block(b) => {
                for x in b.stmts.iter() {
                    let (_, k2) = x.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                    k = k2;
                }
                Ok(((), k))
            }
            Stmt::Empty(e) => Ok(((), k)),
            Stmt::Debugger(_) => todo!(),
            Stmt::With(_) => todo!(),
            Stmt::Return(r) => {
                let cv = match r.arg.as_ref() {
                    Some(arg) => {
                        let (cv, k2) = arg.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                        k = k2;
                        cv
                    }
                    None => {
                        let v = n.funcs[a].opts.alloc(rat_ir::Value::Operator(
                            O::null().context("in getting null")?,
                            vec![],
                            Default::default(),
                            PhantomData,
                        ));
                        n.funcs[a].blocks[k].insts.push(v);
                        v
                    }
                };
                let cv = (ctx.ret)(Use {
                    value: cv,
                    select: Default::default(),
                })
                .context("in gtting ret")?;
                n.funcs[a].blocks[k].term = cv;
                let dummy = n.funcs[a].blocks.alloc(Default::default());
                Ok(((), dummy))
            }
            Stmt::Labeled(l) => {
                let then_block = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[k].term = T::just(BlockTarget {
                    block: then_block,
                    args: vars
                        .iter()
                        .map(|a| Use {
                            value: *a,
                            select: S::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(then_block, Y::default());
                    *v = av;
                }
                let done_block = n.funcs[a].blocks.alloc(Default::default());
                let mut loops = ctx.loops.clone();
                loops.insert(
                    Some(l.label.sym.clone()),
                    BC {
                        r#break: done_block,
                        r#continue: then_block,
                    },
                );
                let ctx2 = RCtx {
                    loops,
                    ..ctx.clone()
                };
                let (_, k) = l
                    .body
                    .ast_compile(&ctx2, c, a, m, n, then_block, ids, vars)?;
                n.funcs[a].blocks[k].term = T::just(BlockTarget {
                    block: done_block,
                    args: vars
                        .iter()
                        .map(|a| Use {
                            value: *a,
                            select: S::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(done_block, Y::default());
                    *v = av;
                }
                return Ok(((), done_block));
            }
            Stmt::Break(b) => {
                let bc = ctx
                    .loops
                    .get(&b.label.as_ref().map(|a| &a.sym).cloned())
                    .context("in getting the label")?;
                n.funcs[a].blocks[k].term = T::just(BlockTarget {
                    block: bc.r#break,
                    args: vars
                        .iter()
                        .map(|a| Use {
                            value: *a,
                            select: S::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                let dummy = n.funcs[a].blocks.alloc(Default::default());
                Ok(((), dummy))
            }
            Stmt::Continue(b) => {
                let bc = ctx
                    .loops
                    .get(&b.label.as_ref().map(|a| &a.sym).cloned())
                    .context("in getting the label")?;
                n.funcs[a].blocks[k].term = T::just(BlockTarget {
                    block: bc.r#continue,
                    args: vars
                        .iter()
                        .map(|a| Use {
                            value: *a,
                            select: S::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                let dummy = n.funcs[a].blocks.alloc(Default::default());
                Ok(((), dummy))
            }
            Stmt::If(cond) => {
                let (cv, k) = cond.test.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let then_block = n.funcs[a].blocks.alloc(Default::default());
                let else_block = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[k].term = T::r#if(
                    Use {
                        value: cv,
                        select: Default::default(),
                    },
                    BlockTarget {
                        block: then_block,
                        args: vec![],
                        prepend: vec![],
                    },
                    BlockTarget {
                        block: else_block,
                        args: vec![],
                        prepend: vec![],
                    },
                )
                .context("in gtting if")?;
                let mut true_vars = vars.to_owned();
                let mut false_vars = vars.to_owned();
                let (x, true_block) =
                    cond.cons
                        .ast_compile(ctx, c, a, m, n, then_block, ids, &mut true_vars)?;
                let false_block = match cond.alt.as_ref() {
                    None => else_block,
                    Some(alt) => {
                        alt.ast_compile(ctx, c, a, m, n, else_block, ids, &mut false_vars)?
                            .1
                    }
                };
                let new = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[true_block].term = T::just(BlockTarget {
                    block: new,
                    args: true_vars
                        .into_iter()
                        .map(|a| Use {
                            value: a,
                            select: Default::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                n.funcs[a].blocks[false_block].term = T::just(BlockTarget {
                    block: new,
                    args: false_vars
                        .into_iter()
                        .map(|a| Use {
                            value: a,
                            select: Default::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                // let av = n.funcs[a].add_blockparam(new, Y::default());
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(new, Y::default());
                    *v = av;
                }
                return Ok(((), new));
            }
            Stmt::Switch(_) => todo!(),
            Stmt::Throw(_) => todo!(),
            Stmt::Try(_) => todo!(),
            Stmt::DoWhile(cond) => {
                let (cv, k) = cond.test.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let then_block = n.funcs[a].blocks.alloc(Default::default());
                let else_block = n.funcs[a].blocks.alloc(Default::default());
                let t = |vars: &[Id<Value<O, T, Y, S>>]| {
                    T::r#if(
                        Use {
                            value: cv,
                            select: Default::default(),
                        },
                        BlockTarget {
                            block: then_block,
                            args: vars
                                .iter()
                                .map(|a| Use {
                                    value: *a,
                                    select: S::default(),
                                })
                                .collect(),
                            prepend: vec![],
                        },
                        BlockTarget {
                            block: else_block,
                            args: vars
                                .iter()
                                .map(|a| Use {
                                    value: *a,
                                    select: S::default(),
                                })
                                .collect(),
                            prepend: vec![],
                        },
                    )
                };
                n.funcs[a].blocks[k].term = T::just(BlockTarget {
                    block: then_block,
                    args: vars
                        .iter()
                        .map(|a| Use {
                            value: *a,
                            select: S::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(then_block, Y::default());
                    *v = av;
                }
                let mut true_vars = vars.to_owned();
                let mut loops = ctx.loops.clone();
                loops.insert(
                    None,
                    BC {
                        r#break: else_block,
                        r#continue: then_block,
                    },
                );
                let ctx2 = RCtx {
                    loops,
                    ..ctx.clone()
                };
                let (x, true_block) =
                    cond.body
                        .ast_compile(&ctx2, c, a, m, n, then_block, ids, &mut true_vars)?;
                // let new = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[true_block].term = t(vars).context("in gtting ret")?;
                // n.funcs[a].blocks[false_block].term = T::just(BlockTarget {
                //     block: new,
                //     args: false_vars
                //         .into_iter()
                //         .map(|a| Use {
                //             value: a,
                //             select: Default::default(),
                //         })
                //         .collect(),
                //     prepend: vec![],
                // });
                // let av = n.funcs[a].add_blockparam(new, Y::default());
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(else_block, Y::default());
                    *v = av;
                }
                return Ok(((), else_block));
            }
            Stmt::While(cond) => {
                let (cv, k) = cond.test.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let then_block = n.funcs[a].blocks.alloc(Default::default());
                let else_block = n.funcs[a].blocks.alloc(Default::default());
                let t = |vars: &[Id<Value<O, T, Y, S>>]| {
                    T::r#if(
                        Use {
                            value: cv,
                            select: Default::default(),
                        },
                        BlockTarget {
                            block: then_block,
                            args: vars
                                .iter()
                                .map(|a| Use {
                                    value: *a,
                                    select: S::default(),
                                })
                                .collect(),
                            prepend: vec![],
                        },
                        BlockTarget {
                            block: else_block,
                            args: vars
                                .iter()
                                .map(|a| Use {
                                    value: *a,
                                    select: S::default(),
                                })
                                .collect(),
                            prepend: vec![],
                        },
                    )
                };
                n.funcs[a].blocks[k].term = t(vars).context("in gtting ret")?;
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(then_block, Y::default());
                    *v = av;
                }
                let mut true_vars = vars.to_owned();
                let mut loops = ctx.loops.clone();
                loops.insert(
                    None,
                    BC {
                        r#break: else_block,
                        r#continue: then_block,
                    },
                );
                let ctx2 = RCtx {
                    loops,
                    ..ctx.clone()
                };
                let (x, true_block) =
                    cond.body
                        .ast_compile(&ctx2, c, a, m, n, then_block, ids, &mut true_vars)?;
                // let new = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[true_block].term = t(vars).context("in gtting ret")?;
                // n.funcs[a].blocks[false_block].term = T::just(BlockTarget {
                //     block: new,
                //     args: false_vars
                //         .into_iter()
                //         .map(|a| Use {
                //             value: a,
                //             select: Default::default(),
                //         })
                //         .collect(),
                //     prepend: vec![],
                // });
                // let av = n.funcs[a].add_blockparam(new, Y::default());
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(else_block, Y::default());
                    *v = av;
                }
                return Ok(((), else_block));
            }
            Stmt::For(_) => todo!(),
            Stmt::ForIn(_) => todo!(),
            Stmt::ForOf(_) => todo!(),
            Stmt::Decl(d) => {
                if let Decl::Var(d) = d {
                    for d in d.decls.iter() {
                        if let Pat::Ident(name) = &d.name {
                            if let Some(x) = d.init.as_ref() {
                                let i = *ids.get(&name.sym).context("in getting a variable")?;
                                let (v, k2) = x.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                                vars[i] = v;
                                k = k2;
                            }
                        }
                    }
                }
                Ok(((), k))
            }
            Stmt::Expr(e) => {
                let (v, k) = e.expr.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                Ok(((), k))
            }
        }
    }
}
impl XAst for Expr {
    type Result<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    > = Id<Value<O, T, Y, S>>;

    fn ast_compile<
        O: TinyOp<O, T, Y, S> + 'static,
        T: TinyTerm<O, T, Y, S> + 'static,
        Y: Clone + Default + 'static,
        S: Default + 'static,
        D: 'static,
    >(
        &self,
        ctx: &RCtx<O, T, Y, S, D>,
        c: &FnDecl,
        a: Id<Func<O, T, Y, S>>,
        m: &BTreeMap<Atom, Mapper<'_, O, T, Y, S, D>>,
        n: &mut rat_ir::module::Module<O, T, Y, S, D>,
        mut k: Id<Block<O, T, Y, S>>,
        ids: &BTreeMap<Atom, usize>,
        vars: &mut [Id<Value<O, T, Y, S>>],
    ) -> anyhow::Result<(Self::Result<O, T, Y, S, D>, Id<Block<O, T, Y, S>>)> {
        match self {
            Expr::This(_) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Object(_) => todo!(),
            Expr::Fn(_) => todo!(),
            Expr::Unary(u) => {
                let (arg, k) = u.arg.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                // let (right, k) = bin.right.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let v = n.funcs[a].opts.alloc(Value::Operator(
                    O::unary(&u.op).context("in getting unary")?,
                    vec![Use {
                        value: arg,
                        select: S::default(),
                    }],
                    Y::default(),
                    PhantomData,
                ));
                n.funcs[a].blocks[k].insts.push(v);
                Ok((v, k))
            }
            Expr::Update(_) => todo!(),
            Expr::Bin(bin) => {
                let (left, k) = bin.left.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let (right, k) = bin.right.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let v = n.funcs[a].opts.alloc(Value::Operator(
                    O::bin(&bin.op).context("in getting binary")?,
                    vec![
                        Use {
                            value: left,
                            select: S::default(),
                        },
                        Use {
                            value: right,
                            select: S::default(),
                        },
                    ],
                    Y::default(),
                    PhantomData,
                ));
                n.funcs[a].blocks[k].insts.push(v);
                Ok((v, k))
            }
            Expr::Assign(assign) => {
                let i = assign
                    .left
                    .as_ident()
                    .context("in ensuring that a simple target is used")?;
                let i = *ids.get(&i.sym).context("in getting a variable")?;
                let (v, k) = assign.right.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                vars[i] = v;
                return Ok((v, k));
            }
            Expr::Member(_) => todo!(),
            Expr::SuperProp(_) => todo!(),
            Expr::Cond(cond) => {
                let (cv, k) = cond.test.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                let then_block = n.funcs[a].blocks.alloc(Default::default());
                let else_block = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[k].term = T::r#if(
                    Use {
                        value: cv,
                        select: Default::default(),
                    },
                    BlockTarget {
                        block: then_block,
                        args: vec![],
                        prepend: vec![],
                    },
                    BlockTarget {
                        block: else_block,
                        args: vec![],
                        prepend: vec![],
                    },
                )
                .context("in gtting if")?;
                let mut true_vars = vars.to_owned();
                let mut false_vars = vars.to_owned();
                let (x, true_block) =
                    cond.cons
                        .ast_compile(ctx, c, a, m, n, then_block, ids, &mut true_vars)?;
                let (y, false_block) =
                    cond.alt
                        .ast_compile(ctx, c, a, m, n, else_block, ids, &mut false_vars)?;
                let new = n.funcs[a].blocks.alloc(Default::default());
                n.funcs[a].blocks[true_block].term = T::just(BlockTarget {
                    block: new,
                    args: vec![x]
                        .into_iter()
                        .chain(true_vars.into_iter())
                        .map(|a| Use {
                            value: a,
                            select: Default::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                n.funcs[a].blocks[false_block].term = T::just(BlockTarget {
                    block: new,
                    args: vec![y]
                        .into_iter()
                        .chain(false_vars.into_iter())
                        .map(|a| Use {
                            value: a,
                            select: Default::default(),
                        })
                        .collect(),
                    prepend: vec![],
                })
                .context("in gtting just")?;
                let av = n.funcs[a].add_blockparam(new, Y::default());
                for v in vars.iter_mut() {
                    let av = n.funcs[a].add_blockparam(new, Y::default());
                    *v = av;
                }
                return Ok((av, new));
            }
            Expr::Call(call) => {
                let args = call
                    .args
                    .iter()
                    .map(|a| &a.expr)
                    .map(|e| {
                        let (v, k2) = e.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                        k = k2;
                        Ok(v)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                match call
                    .callee
                    .as_expr()
                    .context("in getting the callee")?
                    .as_ref()
                {
                    Expr::Ident(ai) => {
                        let f = m.get(&ai.sym).context("in getting the function")?;
                        match f {
                            Mapper::Func(f, _, decl) => {
                                let args: Vec<_> = args
                                    .into_iter()
                                    .map(|a| Use {
                                        value: a,
                                        select: Default::default(),
                                    })
                                    .collect();
                                let c = O::call(*f)?;
                                let v = n.funcs[a].opts.alloc(Value::Operator(
                                    c,
                                    args,
                                    Y::default(),
                                    PhantomData,
                                ));
                                n.funcs[a].blocks[k].insts.push(v);
                                Ok((v, k))
                            }
                            Mapper::Intrinsic(i) => {
                                let args: Vec<_> = args
                                    .into_iter()
                                    .map(|a| Use {
                                        value: a,
                                        select: Default::default(),
                                    })
                                    .collect();
                                let v = n.funcs[a].opts.alloc(Value::Operator(
                                    i.clone(),
                                    args,
                                    Y::default(),
                                    PhantomData,
                                ));
                                n.funcs[a].blocks[k].insts.push(v);
                                Ok((v, k))
                            }
                        }
                    }
                    Expr::Fn(f) if args.len() == 0 => {
                        let b = &f
                            .function
                            .body
                            .as_ref()
                            .context("in getting the body")?
                            .stmts;
                        let new = n.funcs[a].blocks.alloc(Default::default());
                        let ctx2 = RCtx {
                            phantom: PhantomData,
                            ret: Arc::new(move |a| {
                                T::just(BlockTarget {
                                    block: new,
                                    args: vec![a],
                                    prepend: vec![],
                                })
                            }),
                            ..ctx.clone()
                        };
                        for x in b.iter() {
                            let (_, k2) = x.ast_compile(&ctx2, c, a, m, n, k, ids, vars)?;
                            k = k2;
                        }
                        let v = n.funcs[a].opts.alloc(rat_ir::Value::Operator(
                            O::null().context("in getting null")?,
                            vec![],
                            Default::default(),
                            PhantomData,
                        ));
                        n.funcs[a].blocks[k].insts.push(v);
                        n.funcs[a].blocks[k].term = (ctx2.ret)(Use {
                            value: v,
                            select: S::default(),
                        })
                        .context("in gtting ret")?;
                        let v = n.funcs[a].add_blockparam(k, Y::default());
                        Ok((v, new))
                    }
                    _ => anyhow::bail!("invalid callee"),
                }
            }
            Expr::New(_) => todo!(),
            Expr::Seq(s) => {
                let mut v = n.funcs[a].opts.alloc(rat_ir::Value::Operator(
                    O::null().context("in getting null")?,
                    vec![],
                    Default::default(),
                    PhantomData,
                ));
                n.funcs[a].blocks[k].insts.push(v);
                for x in s.exprs.iter() {
                    let (w, k2) = x.ast_compile(ctx, c, a, m, n, k, ids, vars)?;
                    k = k2;
                    v = w;
                }
                Ok((v, k))
            }
            Expr::Ident(i) => Ok((vars[*ids.get(&i.sym).context("in getting a variable")?], k)),
            Expr::Lit(l) => {
                let o = match l {
                    swc_core::ecma::ast::Lit::Str(s) => {
                        O::str(s.value.clone()).context("in getting str")?
                    }
                    swc_core::ecma::ast::Lit::Bool(_) => todo!(),
                    swc_core::ecma::ast::Lit::Null(_) => todo!(),
                    swc_core::ecma::ast::Lit::Num(n) => {
                        O::num(n.value).context("in getting num")?
                    }
                    swc_core::ecma::ast::Lit::BigInt(_) => todo!(),
                    swc_core::ecma::ast::Lit::Regex(_) => todo!(),
                    swc_core::ecma::ast::Lit::JSXText(_) => todo!(),
                };
                let v =
                    n.funcs[a]
                        .opts
                        .alloc(Value::Operator(o, vec![], Y::default(), PhantomData));
                n.funcs[a].blocks[k].insts.push(v);
                Ok((v, k))
            }
            Expr::Tpl(_) => todo!(),
            Expr::TaggedTpl(_) => todo!(),
            Expr::Arrow(_) => todo!(),
            Expr::Class(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::MetaProp(_) => todo!(),
            Expr::Await(_) => todo!(),
            Expr::Paren(p) => p.expr.ast_compile(ctx, c, a, m, n, k, ids, vars),
            Expr::JSXMember(_) => todo!(),
            Expr::JSXNamespacedName(_) => todo!(),
            Expr::JSXEmpty(_) => todo!(),
            Expr::JSXElement(_) => todo!(),
            Expr::JSXFragment(_) => todo!(),
            Expr::TsTypeAssertion(_) => todo!(),
            Expr::TsConstAssertion(_) => todo!(),
            Expr::TsNonNull(_) => todo!(),
            Expr::TsAs(_) => todo!(),
            Expr::TsInstantiation(_) => todo!(),
            Expr::TsSatisfies(_) => todo!(),
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(_) => todo!(),
            Expr::Invalid(_) => todo!(),
        }
    }
}
