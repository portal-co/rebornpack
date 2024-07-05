use import::Canon;
use once_cell::sync::Lazy;
use proc_macro2::{Span, TokenStream};
// use prost::Message;
use quasiquote::quasiquote;
use quote::{format_ident, quote};
use rat_ast::export::rust::{Rust, RustOp};
use rat_ir::{
    bi::license::Taint, no_push, util::Push, BoundOp, BoundSelect, BoundTerm, BoundType, Func,
};
use syn::Ident;
use waffle::{util::new_sig, FunctionBody, MemoryArg, Module, Operator, SignatureData};
// pub mod rat {
//     pub mod waffle {
//         pub mod rt {
//             include!(concat!(env!("OUT_DIR"), "/rat.waffle.rt.rs"));
//         }
//     }
// }

// pub static RT: Lazy<Option<rat::waffle::rt::Rt>> =
//     Lazy::new(|| rat::waffle::rt::Rt::decode(&include_bytes!(env!("RW_RT"))[..]).ok());

pub mod export;
pub mod import;
pub mod wars;
pub mod detect;

#[cfg(test)]
mod tests {
    use super::*;
}
#[derive(
    PartialEq,
    Eq,
    Hash,
    derive_more::Deref,
    derive_more::DerefMut,
    derive_more::From,
    derive_more::Into,
    Clone,
)]
#[repr(transparent)]
pub struct OpWrapper(pub waffle::Operator);
no_push!(
    type OpWrapper;
);
#[derive(Debug, Clone)]
pub struct Import<Y> {
    pub module: String,
    pub name: String,
    pub types: Vec<Y>,
    pub rets: Vec<Y>,
}
impl Taint for OpWrapper {
    fn no_license(&mut self) {
        self.0 = match self.0.clone() {
            Operator::I32Const { value } => Operator::I32Const {
                value: value ^ 0xffffffff,
            },
            Operator::I64Const { value } => Operator::I64Const {
                value: value ^ 0xffffffffffffffff,
            },
            a => a,
        };
    }
}
pub fn do_test(m: &mut waffle::Module, f: &mut waffle::FunctionBody) -> anyhow::Result<()> {
    let mut g: Func<BoundOp<Canon>, BoundTerm<Canon>, BoundType<Canon>, BoundSelect<Canon>> =
        Default::default();
    f.convert_to_max_ssa(None);
    // eprintln!("{}",f.display("", None));
    let fm = import::import_func(&mut g, &f, &mut import::Normal { fn_map: () })?;
    g.entry = fm[f.entry].unwrap();
    // eprintln!(
    //     "{:?}",
    //     g.blocks
    //         .iter()
    //         .map(|(_, k)| k.params.iter().flat_map(|a| a.0.clone()).collect::<Vec<_>>())
    //         .collect::<Vec<_>>()
    // );
    let sig = new_sig(
        m,
        SignatureData {
            params: f.blocks[f.entry].params.iter().map(|a| a.0).collect(),
            returns: f.rets.clone(),
        },
    );
    let mut new = FunctionBody::new(&m, sig);
    rat_ir::maxssa::maxssa(&mut g);
    export::export_func_seal(&mut (), m, &mut new, &g)?;
    *f = new;
        // eprintln!("{}",f.display("", None));
    Ok(())
}
pub fn test_mod(m: &mut Module) -> anyhow::Result<()> {
    return m.try_take_per_func_body(do_test);
}
pub struct Guest<T> {
    pub wrapped: T,
    pub rt: String,
}
impl<C: 'static, T: Push<C> + 'static> Push<C> for Guest<T> {
    fn push(b: C) -> either::Either<Self, C> {
        match castaway::cast!(b, Guest<T>) {
            Ok(a) => either::Either::Left(a),
            Err(b) => T::push(b).map_left(|a| Guest {
                wrapped: a,
                rt: "FILLME!$".to_owned(),
            }),
        }
    }
}
pub struct Host<T> {
    pub wrapped: T,
    pub rt: String,
}
impl<C: 'static, T: Push<C> + 'static> Push<C> for Host<T> {
    fn push(b: C) -> either::Either<Self, C> {
        match castaway::cast!(b, Host<T>) {
            Ok(a) => either::Either::Left(a),
            Err(b) => T::push(b).map_left(|a| Host {
                wrapped: a,
                rt: "FILLME!$".to_owned(),
            }),
        }
    }
}
pub struct WasmInfer{
    
}