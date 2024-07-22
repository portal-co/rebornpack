use std::iter::repeat;

use databake::Bake;
use id_arena::Id;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use rat_ast::export::rust::{RustOp, RustSel};
use rat_ir::{no_push, Func};

pub struct Await {}
impl<C> RustOp<C> for Await {
    fn rust(
        &self,
        ctx: &mut C,
        mut args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        let arg = args.next().unwrap();
        quote! {
            #arg .await
        }
    }
}
no_push!(
    type Await;
);
pub struct SelfCode {}
impl<C> RustOp<C> for SelfCode {
    fn rust(
        &self,
        ctx: &mut C,
        args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            include_str!(file!())
        }
    }
}
no_push!(
    type SelfCode;
);
pub struct Baked<T> {
    pub baked: T,
}
impl<C,T: Bake> RustOp<C> for Baked<T> {
    fn rust(
        &self,
        ctx: &mut C,
        args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        self.baked.bake(&Default::default())
    }
}
no_push!(
    type Baked<T>;
);
pub struct Bind<O, T, Y, S, C, R> {
    pub func: Id<Func<O, T, Y, S>>,
    pub cache_num_args: C,
    pub root: R,
}
no_push!(
    type Bind<O, T, Y, S, C, R>;
);
impl<C, O, T, Y, S, R: ToTokens> RustOp<C> for Bind<O, T, Y, S, usize, R> {
    fn rust(&self,ctx: &mut C, args: impl Iterator<Item = TokenStream>) -> TokenStream {
        let root = &self.root;
        let t = format_ident!("_{}", self.func.index());
        let mut x = quote! {};
        let args = args.enumerate().map(|(i, v)| {
            let i = format_ident!("arg{i}");
            quote! {
                let #i = #v;
            }
            .to_tokens(&mut x);
            quote! {
                #i
            }
        });
        let args = args.collect::<Vec<_>>();
        let args = args
            .iter()
            .cloned()
            .chain(repeat(quote! {_}).take(self.cache_num_args - args.len()))
            .collect::<Vec<_>>();
        (quote! {
            #root::partial!(move crate::palette::#t => #(#args),*)
        })
        .to_tokens(&mut x);
        return quote! {
            {
                #x
            }
        };
    }
}

pub struct Stringify {}
no_push!(
    type Stringify;
);
impl<C> RustSel<C> for Stringify {
    fn rust(&self, a: &TokenStream, ctx: &mut C) -> TokenStream {
        quote! {
            format!("{}",#a)
        }
    }
}
