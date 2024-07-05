use anyhow::Context;
use either::Either;
use waffle::{
    Block, ExportKind, FunctionBody, ImportKind, Memory, Module, Operator, Type, Value, ValueDef,
};

pub fn memory(m: &Module) -> Option<Memory> {
    let mut mem = m
        .exports
        .iter()
        .map(Either::Left)
        .chain(m.imports.iter().map(Either::Right));
    let mem = mem.find_map(|i| match i {
        Either::Left(e) => {
            if e.name != "memory" {
                return None;
            }
            let ExportKind::Memory(m) = &e.kind else {
                return None;
            };
            return Some(*m);
        }
        Either::Right(i) => {
            if i.name != "memory" {
                return None;
            }
            if i.module != "env" {
                return None;
            }
            let ImportKind::Memory(m) = &i.kind else {
                return None;
            };
            return Some(*m);
        }
    });
    // let sp = m.exports.iter().find_map(|e|{
    //     if e.name != "__stack_pointer"{
    //         return None;
    //     };
    //     let ExportKind::Global(g) = &e.kind else{
    //         return None;
    //     };
    //     return Some(*g);
    // });
    mem
}
pub trait Mallocator {
    fn malloc(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        b: Block,
    ) -> anyhow::Result<Value>;
    fn free(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        s: Value,
        b: Block,
    ) -> anyhow::Result<()>;
    fn destroy(
        self: Box<Self>,
        m: &mut Module,
        f: &mut FunctionBody,
        b: Block,
    ) -> anyhow::Result<()>;
}
pub struct Brotli {
    pub state: Value,
}
pub fn add_op(f: &mut FunctionBody, args: &[Value], rets: &[Type], op: Operator) -> Value {
    let args = f.arg_pool.from_iter(args.iter().map(|a| *a));
    let rets = f.type_pool.from_iter(rets.iter().map(|a| *a));
    return f.add_value(ValueDef::Operator(op, args, rets));
}
impl Brotli {
    pub fn new(m: &mut Module, f: &mut FunctionBody, b: Block) -> anyhow::Result<Self> {
        let i = m
            .exports
            .iter()
            .find_map(|x| {
                if x.name != "BrotliEncoderCreateInstance" {
                    return None;
                }
                let ExportKind::Func(f) = &x.kind else {
                    return None;
                };
                return Some(*f);
            })
            .context("in getting init import")?;
        let z = add_op(f, &[], &[Type::I32], Operator::I32Const { value: 0 });
        f.append_to_block(b, z);
        let v = add_op(
            f,
            &[z, z, z],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(Self { state: v });
    }
}
impl Mallocator for Brotli {
    fn malloc(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        b: Block,
    ) -> anyhow::Result<Value> {
        let i = m
            .exports
            .iter()
            .find_map(|x| {
                if x.name != "BrotliEncoderMallocU8" {
                    return None;
                }
                let ExportKind::Func(f) = &x.kind else {
                    return None;
                };
                return Some(*f);
            })
            .context("in getting malloc import")?;
        let v = add_op(
            f,
            &[self.state, v],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(v);
    }

    fn free(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        s: Value,
        b: Block,
    ) -> anyhow::Result<()> {
        let i = m
            .exports
            .iter()
            .find_map(|x| {
                if x.name != "BrotliEncoderFreeU8" {
                    return None;
                }
                let ExportKind::Func(f) = &x.kind else {
                    return None;
                };
                return Some(*f);
            })
            .context("in getting free import")?;
        let v = add_op(
            f,
            &[self.state, v, s],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(());
    }

    fn destroy(
        self: Box<Self>,
        m: &mut Module,
        f: &mut FunctionBody,
        b: Block,
    ) -> anyhow::Result<()> {
        let i = m
            .exports
            .iter()
            .find_map(|x| {
                if x.name != "BrotliEncoderDestroyInstance" {
                    return None;
                }
                let ExportKind::Func(f) = &x.kind else {
                    return None;
                };
                return Some(*f);
            })
            .context("in getting free import")?;
        let v = add_op(
            f,
            &[self.state],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(());
    }
}
pub struct Normal {}
impl Normal {
    pub fn new(m: &mut Module, f: &mut FunctionBody, b: Block) -> anyhow::Result<Self> {
        let (i, _) = m
            .exports
            .iter()
            .filter_map(|a| match &a.kind {
                ExportKind::Func(f) => Some((*f, a.name.as_str())),
                _ => None,
            })
            .chain(m.funcs.entries().map(|(a, b)| (a, b.name())))
            .find(|a| a.1 == "malloc")
            .context("in getting malloc")?;
        let (i, _) = m
            .exports
            .iter()
            .filter_map(|a| match &a.kind {
                ExportKind::Func(f) => Some((*f, a.name.as_str())),
                _ => None,
            })
            .chain(m.funcs.entries().map(|(a, b)| (a, b.name())))
            .find(|a| a.1 == "free")
            .context("in getting free")?;
        return Ok(Self {});
    }
}
impl Mallocator for Normal {
    fn malloc(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        b: Block,
    ) -> anyhow::Result<Value> {
        let (i, _) = m
            .exports
            .iter()
            .filter_map(|a| match &a.kind {
                ExportKind::Func(f) => Some((*f, a.name.as_str())),
                _ => None,
            })
            .chain(m.funcs.entries().map(|(a, b)| (a, b.name())))
            .find(|a| a.1 == "malloc")
            .context("in getting malloc")?;
        let v = add_op(
            f,
            &[v],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(v);
    }

    fn free(
        &self,
        m: &mut Module,
        f: &mut FunctionBody,
        v: Value,
        s: Value,
        b: Block,
    ) -> anyhow::Result<()> {
        let (i, _) = m
            .exports
            .iter()
            .filter_map(|a| match &a.kind {
                ExportKind::Func(f) => Some((*f, a.name.as_str())),
                _ => None,
            })
            .chain(m.funcs.entries().map(|(a, b)| (a, b.name())))
            .find(|a| a.1 == "free")
            .context("in getting free")?;
        let v = add_op(
            f,
            &[v],
            &m.signatures[m.funcs[i].sig()].returns,
            Operator::Call { function_index: i },
        );
        f.append_to_block(b, v);
        return Ok(());
    }

    fn destroy(
        self: Box<Self>,
        m: &mut Module,
        f: &mut FunctionBody,
        b: Block,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}
pub fn gather_mallocator(m: &mut Module, f: &mut FunctionBody,b: Block) -> anyhow::Result<Box<dyn Mallocator>>{
    if let Ok(x) = Brotli::new(m, f, b){
        return Ok(Box::new(x));
    }
    if let Ok(x) = Normal::new(m, f, b){
        return Ok(Box::new(x));
    }
    anyhow::bail!("mallocator not found")
}