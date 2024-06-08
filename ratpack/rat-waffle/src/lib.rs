use proc_macro2::{Span, TokenStream};
use rat_ast::export::rust::RustOp;
use rat_ir::{bi::license::Taint, no_push};
use syn::Ident;
use waffle::{MemoryArg, Operator, SignatureData};

pub mod export;
pub mod import;
// pub mod wars;

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
pub struct OpWrapper(pub waffle::Operator);
no_push!(
    type OpWrapper;
);
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