use std::{collections::BTreeMap, iter::once};

use anyhow::Context;
use id_arena::Id;

use crate::backend::Ast;

use super::{Block, Instr, SSAFunc, Term, ValueId};
impl Ast {
    pub fn ssa(
        &self,
        fun: &mut SSAFunc,
        mut k: Id<Block>,
        vars: &mut BTreeMap<String, ValueId>,
        fparams: &[ValueId],
        via: &mut impl FnMut(ValueId, &mut SSAFunc, Id<Block>) -> anyhow::Result<()>,
    ) -> anyhow::Result<(ValueId, Id<Block>)> {
        match self {
            Ast::Call(a, b) => {
                let (a, mut k) = a.ssa(fun, k, vars, fparams, via)?;
                let mut c = vec![];
                for b in b {
                    let (d, l) = b.ssa(fun, k, vars, fparams, via)?;
                    k = l;
                    c.push(d);
                }
                return Ok((fun.blocks[k].need(Instr::Call(a, c)), k));
            }
            Ast::Jmp(a, b) => {
                let (a, mut k) = a.ssa(fun, k, vars, fparams, via)?;
                let mut c = vec![];
                for b in b {
                    let (d, l) = b.ssa(fun, k, vars, fparams, via)?;
                    k = l;
                    c.push(d);
                }
                fun.blocks[k].term = Term::ReturnCall(a.clone(), c);
                let d = fun.blocks.alloc(Default::default());
                return Ok((a, d));
            }
            Ast::Sym(s) => {
                return Ok((fun.blocks[k].need(Instr::Addrof(s.clone())), k));
            }
            Ast::Var(v) => {
                return Ok((vars.get(v).context("in getting var")?.clone(), k));
            }
            Ast::Assign(a, b) => {
                let (b, k) = b.ssa(fun, k, vars, fparams, via)?;
                vars.insert(a.clone(), b.clone())
                    .context("in assigning var")?;
                return Ok((b, k));
            }
            Ast::Alloca(b) => {
                let (b, k) = b.ssa(fun, k, vars, fparams, via)?;
                return Ok((fun.blocks[k].need(super::Instr::Alloca(b)), k));
            }
            Ast::Load(b, c) => {
                let (b, k) = b.ssa(fun, k, vars, fparams, via)?;
                return Ok((fun.blocks[k].need(super::Instr::Load(b, c.clone())), k));
            }
            Ast::Store(a, b, c) => {
                let (a, k) = a.ssa(fun, k, vars, fparams, via)?;
                let (b, k) = b.ssa(fun, k, vars, fparams, via)?;
                return Ok((fun.blocks[k].need(super::Instr::Store(a, b, c.clone())), k));
            }
            Ast::Const(ko) => {
                return Ok((fun.blocks[k].need(super::Instr::Const(*ko)), k));
            }
            Ast::Bin(o, a, b) => {
                let (a, k) = a.ssa(fun, k, vars, fparams, via)?;
                let (b, k) = b.ssa(fun, k, vars, fparams, via)?;
                return Ok((fun.blocks[k].need(super::Instr::Bin(o.clone(), a, b)), k));
            }
            Ast::If {
                cond,
                cty,
                if_true,
                if_false,
            } => {
                let tru = fun.blocks.alloc(Default::default());
                let fals = fun.blocks.alloc(Default::default());
                let done = fun.blocks.alloc(Default::default());
                let (cond, k) = cond.ssa(fun, k, vars, fparams, via)?;
                let bak = vars.clone();
                let mut trup = vec![];
                for (i, (_, v)) in vars.iter_mut().enumerate() {
                    trup.push(v.clone());
                    *v = fun.blocks[tru].need(Instr::Param(i));
                }
                let (t, l) = if_true.ssa(fun, tru, vars, fparams, via)?;
                let mut p = vec![];
                for (_, a) in vars.iter() {
                    p.push(a.clone())
                }
                p.push(t);
                fun.blocks[l].term = Term::Br(super::Target {
                    id: done,
                    params: p,
                });
                *vars = bak;
                let mut flsp = vec![];
                for (i, (_, v)) in vars.iter_mut().enumerate() {
                    flsp.push(v.clone());
                    *v = fun.blocks[fals].need(Instr::Param(i));
                }
                let (t, l) = if_false.ssa(fun, fals, vars, fparams, via)?;
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
                    cty: cty.clone(),
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
            Ast::Param(p) => Ok((fparams[*p].clone(), k)),
            Ast::Plat(a, b) => {
                let mut c = vec![];
                for b in b {
                    let (d, l) = b.ssa(fun, k, vars, fparams, via)?;
                    k = l;
                    c.push(d);
                }
                return Ok((fun.blocks[k].need(Instr::Plat(a.clone(), c)), k));
            }
            Ast::OS => Ok((fun.blocks[k].need(Instr::OS), k)),
            Ast::Ret(a) => {
                let (a, k) = a.ssa(fun, k, vars, fparams, via)?;
                via(a.clone(), fun, k)?;
                let d = fun.blocks.alloc(Default::default());
                return Ok((a, d));
            }
            Ast::DoWhile { body, cond, cty } => {
                let mut bb = fun.blocks.alloc(Default::default());
                let mut p = vec![];
                for (i, (_, v)) in vars.iter_mut().enumerate() {
                    p.push(v.clone());
                    *v = fun.blocks[bb].need(Instr::Param(i));
                }
                fun.blocks[k].term = Term::Br(super::Target { id: bb, params: p });
                let (v, bb) = body.ssa(fun, bb, vars, fparams, via)?;
                let (c, bb) = cond.ssa(fun, bb, vars, fparams, via)?;
                let new = fun.blocks.alloc(Default::default());
                let mut p = vec![];
                for (i, (_, v)) in vars.iter_mut().enumerate() {
                    p.push(v.clone());
                    *v = fun.blocks[new].need(Instr::Param(i));
                }
                fun.blocks[bb].term = Term::BrIf {
                    cond: c,
                    cty: cty.clone(),
                    if_true: super::Target {
                        id: bb,
                        params: p.clone(),
                    },
                    if_false: super::Target {
                        id: new,
                        params: p.iter().map(|a| a.clone()).chain(once(v)).collect(),
                    },
                };
                let v = fun.blocks[new].need(Instr::Param(vars.len()));
                return Ok((v, new));
            }
            Ast::Many(m) => {
                let mut v = ValueId(format!("!"));
                for n in m.iter() {
                    (v, k) = n.ssa(fun, k, vars, fparams, via)?;
                }
                return Ok((v, k));
            }
            Ast::Data(d) => {
                return Ok((fun.blocks[k].need(super::Instr::Data(d.clone())), k));
            }
        }
    }
}
