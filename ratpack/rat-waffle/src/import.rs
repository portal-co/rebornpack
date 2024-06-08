use std::{collections::BTreeMap, iter::empty, marker::PhantomData};

use either::Either;
use id_arena::Id;

use rat_ir::{
    no_push,
    transform::{ctx::NormalTermIn, NormalTerm},
    util::{Extract, Push},
    BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Call, Func, SaneTerminator,
    Use, Value,
};
use waffle::{entity::PerEntity, Block, FunctionBody, Operator, Terminator};

use crate::OpWrapper;
pub trait ImportOp<O, T, Y, S> {
    fn op(
        &mut self,
        op: &Operator,
        func: &mut Func<O, T, Y, S>,
        args: Vec<Use<O, T, Y, S>>,
        ty: Y,
        k: Id<rat_ir::Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Id<Value<O, T, Y, S>>, Id<rat_ir::Block<O, T, Y, S>>)>;
}
pub trait ImportTerm<O, T, Y, S> {
    fn term(
        &mut self,
        dst: &mut Func<O, T, Y, S>,
        t: &Terminator,
        block_mapper: &PerEntity<Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>,
        mapper: &PerEntity<waffle::Value, Option<Id<Value<O, T, Y, S>>>>,
        k: Id<rat_ir::Block<O, T, Y, S>>,
    ) -> anyhow::Result<()>;
}
pub trait WaffleCall<O, T, Y, S> {
    fn call(&mut self, a: waffle::Func) -> anyhow::Result<O>;
}
pub struct Normal<O, T, Y, S> {
    pub fn_map: PerEntity<waffle::Func, Option<Id<Func<O, T, Y, S>>>>,
}
pub trait WaffleOp {
    fn from_waffle(x: &waffle::Operator) -> Self;
}
impl WaffleOp for OpWrapper {
    fn from_waffle(x: &waffle::Operator) -> Self {
        Self(x.clone())
    }
}
impl<O: Push<OpWrapper> + Push<Call<O, T, Y, S>>, T, Y, S> ImportOp<O, T, Y, S>
    for Normal<O, T, Y, S>
{
    fn op(
        &mut self,
        op: &Operator,
        func: &mut Func<O, T, Y, S>,
        args: Vec<Use<O, T, Y, S>>,
        ty: Y,
        k: Id<rat_ir::Block<O, T, Y, S>>,
    ) -> anyhow::Result<(Id<Value<O, T, Y, S>>, Id<rat_ir::Block<O, T, Y, S>>)> {
        let v = func.opts.alloc(Value::Operator(
            (|| {
                let Operator::Call { function_index } = op else {
                    return O::push(OpWrapper(op.clone()))
                        .map_right(|_| ())
                        .unwrap_left();
                };
                let Some(x) = self.fn_map[*function_index].as_ref() else {
                    return O::push(OpWrapper(op.clone()))
                        .map_right(|_| ())
                        .unwrap_left();
                };
                return O::push(Call { func: *x }).map_right(|_| ()).unwrap_left();
            })(),
            args,
            ty,
            PhantomData,
        ));
        func.blocks[k].insts.push(v);
        return Ok((v, k));
    }
}

impl<O, T: Push<WaffleTerm<O, T, Y, S>>, Y, S: Push<Option<usize>>> ImportTerm<O, T, Y, S>
    for Normal<O, T, Y, S>
{
    fn term(
        &mut self,
        dst: &mut Func<O, T, Y, S>,
        t: &Terminator,
        block_mapper: &PerEntity<Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>,
        mapper: &PerEntity<waffle::Value, Option<Id<Value<O, T, Y, S>>>>,
        k: Id<rat_ir::Block<O, T, Y, S>>,
    ) -> anyhow::Result<()> {
        match t {
            Terminator::Br { target } => {
                let target = BlockTarget {
                    block: block_mapper[target.block].unwrap(),
                    args: target
                        .args
                        .iter()
                        .map(|v| mapper[*v].unwrap())
                        .map(|a| Use {
                            value: a,
                            select: S::push(None).map_right(|_| ()).unwrap_left(),
                        })
                        .collect(),
                    prepend: vec![],
                };
                dst.blocks[k].term = T::push(WaffleTerm::Br(target))
                    .map_right(|_| ())
                    .unwrap_left();
                Ok(())
            }
            Terminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let target = |target: &waffle::BlockTarget| BlockTarget {
                    block: block_mapper[target.block].unwrap(),
                    args: target
                        .args
                        .iter()
                        .map(|v| mapper[*v].unwrap())
                        .map(|a| Use {
                            value: a,
                            select: S::push(None).map_right(|_| ()).unwrap_left(),
                        })
                        .collect(),
                    prepend: vec![],
                };
                let if_true = target(if_true);
                let if_false = target(if_false);
                let cond = mapper[*cond].unwrap();
                dst.blocks[k].term = T::push(WaffleTerm::CondBr {
                    cond: Use {
                        value: cond,
                        select: S::push(None).map_right(|_| ()).unwrap_left(),
                    },
                    if_true,
                    if_false,
                })
                .map_right(|_| ())
                .unwrap_left();
                Ok(())
            }
            Terminator::Select {
                value,
                targets,
                default,
            } => {
                let value = Use {
                    value: mapper[*value].unwrap(),
                    select: S::push(None).map_right(|_| ()).unwrap_left(),
                };
                let target = |target: &waffle::BlockTarget| BlockTarget {
                    block: block_mapper[target.block].unwrap(),
                    args: target
                        .args
                        .iter()
                        .map(|v| mapper[*v].unwrap())
                        .map(|a| Use {
                            value: a,
                            select: S::push(None).map_right(|_| ()).unwrap_left(),
                        })
                        .collect(),
                    prepend: vec![],
                };
                let default = target(default);
                let targets = targets.iter().map(target).collect();
                dst.blocks[k].term = T::push(WaffleTerm::Select {
                    value: value,
                    cases: targets,
                    default: default,
                })
                .map_right(|_| ())
                .unwrap_left();
                Ok(())
            }
            Terminator::Return { values } => {
                let values = values
                    .iter()
                    .map(|v| mapper[*v].unwrap())
                    .map(|a| Use {
                        value: a,
                        select: S::push(None).map_right(|_| ()).unwrap_left(),
                    })
                    .collect();
                dst.blocks[k].term = T::push(WaffleTerm::Ret(values))
                    .map_right(|_| ())
                    .unwrap_left();
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
pub enum WaffleTerm<O, T, Y, S> {
    Br(BlockTarget<O, T, Y, S>),
    CondBr {
        cond: Use<O, T, Y, S>,
        if_true: BlockTarget<O, T, Y, S>,
        if_false: BlockTarget<O, T, Y, S>,
    },
    Select {
        value: Use<O, T, Y, S>,
        cases: Vec<BlockTarget<O, T, Y, S>>,
        default: BlockTarget<O, T, Y, S>,
    },
    Ret(Vec<Use<O, T, Y, S>>),
    Unreachable,
}
no_push!(
    type WaffleTerm<O, T, Y, S>;
);
impl<O, T, Y, S> Default for WaffleTerm<O, T, Y, S> {
    fn default() -> Self {
        Self::Unreachable
    }
}
impl<O, T, Y, S> SaneTerminator<O, T, Y, S> for WaffleTerm<O, T, Y, S> {
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            WaffleTerm::Br(t) => Either::Left(t.args.iter()),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Left(
                vec![cond]
                    .into_iter()
                    .chain(if_true.args.iter())
                    .chain(if_false.args.iter()),
            )),
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => Either::Right(Either::Right(Either::Right(
                vec![value]
                    .into_iter()
                    .chain(default.args.iter())
                    .chain(cases.iter().flat_map(|b| b.args.iter())),
            ))),
            WaffleTerm::Ret(r) => Either::Left(r.iter()),
            WaffleTerm::Unreachable => Either::Right(Either::Right(Either::Left(empty()))),
        }
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            WaffleTerm::Br(t) => Either::Left(t.args.iter_mut()),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Left(
                vec![cond]
                    .into_iter()
                    .chain(if_true.args.iter_mut())
                    .chain(if_false.args.iter_mut()),
            )),
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => Either::Right(Either::Right(Either::Right(
                vec![value]
                    .into_iter()
                    .chain(default.args.iter_mut())
                    .chain(cases.iter_mut().flat_map(|b| b.args.iter_mut())),
            ))),
            WaffleTerm::Ret(r) => Either::Left(r.iter_mut()),
            WaffleTerm::Unreachable => Either::Right(Either::Right(Either::Left(empty()))),
        }
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            WaffleTerm::Br(a) => Either::Right(Either::Left(vec![a].into_iter())),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Left(vec![if_true, if_false].into_iter())),
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => Either::Right(Either::Right(vec![default].into_iter().chain(cases.iter()))),
            WaffleTerm::Ret(_) | WaffleTerm::Unreachable => Either::Left(empty()),
        }
    }

    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            WaffleTerm::Br(a) => Either::Right(Either::Left(vec![a].into_iter())),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Left(vec![if_true, if_false].into_iter())),
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => Either::Right(Either::Right(
                vec![default].into_iter().chain(cases.iter_mut()),
            )),
            WaffleTerm::Ret(_) | WaffleTerm::Unreachable => Either::Left(empty()),
        }
    }
}
impl<O, T, Y: Extract<Y2>, S: Extract<S2>, O2, T2, Y2, S2> NormalTerm<O, T, Y, S, O2, T2, Y2, S2>
    for WaffleTerm<O, T, Y, S>
{
    type Then = WaffleTerm<O2, T2, Y2, S2>;

    fn norm(
        &self,
        to_dst: &std::collections::BTreeMap<
            Id<rat_ir::Block<O, T, Y, S>>,
            Id<rat_ir::Block<O2, T2, Y2, S2>>,
        >,
        m: &std::collections::BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        match self {
            WaffleTerm::Br(a) => WaffleTerm::Br(NormalTerm::norm(a, to_dst, m)),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let cond = Use {
                    value: m.get(&cond.value).copied().unwrap(),
                    select: cond.select.extract(),
                };
                WaffleTerm::CondBr {
                    cond,
                    if_true: NormalTerm::norm(if_true, to_dst, m),
                    if_false: NormalTerm::norm(if_false, to_dst, m),
                }
            }
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => {
                let value = Use {
                    value: m.get(&value.value).copied().unwrap(),
                    select: value.select.extract(),
                };
                WaffleTerm::Select {
                    value,
                    cases: cases
                        .iter()
                        .map(|a| NormalTerm::norm(a, to_dst, m))
                        .collect(),
                    default: NormalTerm::norm(default, to_dst, m),
                }
            }
            WaffleTerm::Ret(v) => WaffleTerm::Ret(
                v.iter()
                    .map(|a| Use {
                        value: m.get(&a.value).copied().unwrap(),
                        select: a.select.extract(),
                    })
                    .collect(),
            ),
            WaffleTerm::Unreachable => WaffleTerm::Unreachable,
        }
    }
}
impl<O, T, Y: Extract<Y2>, S: Extract<S2>, O2, T2, Y2, S2, C>
    NormalTermIn<C, O, T, Y, S, O2, T2, Y2, S2> for WaffleTerm<O, T, Y, S>
{
    type Then = WaffleTerm<O2, T2, Y2, S2>;

    fn norm(
        &self,
        ctx: &mut C,
        to_dst: &std::collections::BTreeMap<
            Id<rat_ir::Block<O, T, Y, S>>,
            Id<rat_ir::Block<O2, T2, Y2, S2>>,
        >,
        m: &std::collections::BTreeMap<Id<Value<O, T, Y, S>>, Id<Value<O2, T2, Y2, S2>>>,
    ) -> Self::Then {
        match self {
            WaffleTerm::Br(a) => WaffleTerm::Br(NormalTermIn::norm(a, ctx, to_dst, m)),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let cond = Use {
                    value: m.get(&cond.value).copied().unwrap(),
                    select: cond.select.extract(),
                };
                WaffleTerm::CondBr {
                    cond,
                    if_true: NormalTermIn::norm(if_true, ctx, to_dst, m),
                    if_false: NormalTermIn::norm(if_false, ctx, to_dst, m),
                }
            }
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => {
                let value = Use {
                    value: m.get(&value.value).copied().unwrap(),
                    select: value.select.extract(),
                };
                WaffleTerm::Select {
                    value,
                    cases: cases
                        .iter()
                        .map(|a| NormalTermIn::norm(a, ctx, to_dst, m))
                        .collect(),
                    default: NormalTermIn::norm(default, ctx, to_dst, m),
                }
            }
            WaffleTerm::Ret(v) => WaffleTerm::Ret(
                v.iter()
                    .map(|a| Use {
                        value: m.get(&a.value).copied().unwrap(),
                        select: a.select.extract(),
                    })
                    .collect(),
            ),
            WaffleTerm::Unreachable => WaffleTerm::Unreachable,
        }
    }
}
pub fn import_block<O, T, Y: Push<Vec<waffle::Type>> + Clone, S: Push<Option<usize>>>(
    dst: &mut Func<O, T, Y, S>,
    src: &FunctionBody,
    sk: Block,
    mapper: &PerEntity<Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>,
    builder: &mut (impl ImportOp<O, T, Y, S> + ImportTerm<O, T, Y, S>),
) -> anyhow::Result<()> {
    let mut dk = mapper[sk].unwrap();
    let params = src.blocks[sk]
        .params
        .iter()
        .map(|(p, _)| dst.add_blockparam(dk, Y::push(vec![*p]).map_right(|_| ()).unwrap_left()))
        .collect::<Vec<_>>();
    let mut m = PerEntity::default();
    for (p, q) in params.iter().zip(src.blocks[sk].params.iter().map(|a| a.1)) {
        m[q] = Some(*p);
    }
    for i in src.blocks[sk].insts.iter().map(|a| *a) {
        m[i] = Some(match &src.values[i] {
            waffle::ValueDef::BlockParam(_, _, _) => todo!(),
            waffle::ValueDef::Operator(o, p, t) => {
                let t: Y = Y::push(src.type_pool[*t].to_owned())
                    .map_right(|_| ())
                    .unwrap_left();
                let p = src.arg_pool[*p]
                    .iter()
                    .map(|a| m[*a].unwrap())
                    .map(|a| Use {
                        value: a,
                        select: S::push(None).map_right(|_| ()).unwrap_left(),
                    })
                    .collect::<Vec<_>>();
                let (v, ek) = builder.op(o, dst, p, t.clone(), dk)?;
                dk = ek;
                v
            }
            waffle::ValueDef::PickOutput(a, b, c) => {
                let d = dst.opts.alloc(rat_ir::Value::Alias(
                    rat_ir::Use {
                        value: m[*a].unwrap(),
                        select: S::push(Some(*b as usize)).map_right(|_| ()).unwrap_left(),
                    },
                    Y::push(vec![*c]).map_right(|_| ()).unwrap_left(),
                ));
                dst.blocks[dk].insts.push(d);
                d
            }
            waffle::ValueDef::Alias(v) => m[*v].unwrap(),
            waffle::ValueDef::Placeholder(_) => todo!(),
            waffle::ValueDef::Trace(_, _) => todo!(),
            waffle::ValueDef::None => {
                let t: Y = Y::push(vec![]).map_right(|_| ()).unwrap_left();
                let p = vec![];
                let (v, ek) = builder.op(&Operator::Nop, dst, p, t.clone(), dk)?;
                dk = ek;
                v
            }
        })
    }
    builder.term(dst, &src.blocks[sk].terminator, mapper, &m, dk)?;
    return Ok(());
}
pub fn import_func<O, T: Default, Y: Push<Vec<waffle::Type>> + Clone, S: Push<Option<usize>>>(
    dst: &mut Func<O, T, Y, S>,
    src: &FunctionBody,
    builder: &mut (impl ImportOp<O, T, Y, S> + ImportTerm<O, T, Y, S>),
) -> anyhow::Result<PerEntity<Block, Option<Id<rat_ir::Block<O, T, Y, S>>>>> {
    let mut mapper = PerEntity::default();
    for k in src.blocks.iter() {
        mapper[k] = Some(dst.blocks.alloc(Default::default()));
    }
    for k in src.blocks.iter() {
        import_block(dst, src, k, &mapper, &mut *builder)?;
    }
    return Ok(mapper);
}
pub struct Canon {}
impl Bound for Canon {
    type O<O, T, Y, S> = Either<Call<O, T, Y, S>, OpWrapper>;

    type T<O, T, Y, S> = WaffleTerm<O, T, Y, S>;

    type Y<O, T, Y, S> = Vec<waffle::Type>;

    type S<O, T, Y, S> = Option<usize>;
}
