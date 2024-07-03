use crate::export::{EmitTarget, ToIdAst};
use either::Either;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use rat_ir::{
    util::{Catch, If},
    BlockTarget, Bound, BoundOp, BoundSelect, BoundTerm, BoundType, Call,
};
use syn::{Ident, Index, Lifetime};

use super::{CffAst, ExportAst, ExportTerm, ReloopAst};
impl<O, T, Y, S> RustOp for Call<O, T, Y, S> {
    fn rust(&self, args: impl Iterator<Item = TokenStream>) -> TokenStream {
        let t = format_ident!("_{}", self.func.index());
        quote! {
            crate::palette::#t(#(#args),*)
        }
    }
}
pub trait Rust {
    fn rust(&self) -> TokenStream;
}
pub trait RustSel {
    fn rust(&self, a: &TokenStream) -> TokenStream;
}
pub struct Move {}
impl RustSel for Move {
    fn rust(&self, a: &TokenStream) -> TokenStream {
        quote! {
            std::mem::replace(&mut #a,Default::default())
        }
    }
}
pub struct Clone_ {}
impl RustSel for Clone_ {
    fn rust(&self, a: &TokenStream) -> TokenStream {
        quote! {
            #a.clone()
        }
    }
}
pub trait RustOp {
    fn rust(&self, args: impl Iterator<Item = TokenStream>) -> TokenStream;
}
impl<B: Bound> RustOp for BoundOp<B>
where
    B::O<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: RustOp,
{
    fn rust(&self, args: impl Iterator<Item = TokenStream>) -> TokenStream {
        self.0.rust(args)
    }
}
impl<B: Bound> Rust for BoundType<B>
where
    B::Y<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: Rust,
{
    fn rust(&self) -> TokenStream {
        self.0.rust()
    }
}
impl<B: Bound> RustSel for BoundSelect<B>
where
    B::S<BoundOp<B>, BoundTerm<B>, BoundType<B>, BoundSelect<B>>: RustSel,
{
    fn rust(&self, a: &TokenStream) -> TokenStream {
        self.0.rust(a)
    }
}
impl<A: Rust, B: Rust> Rust for Either<A, B> {
    fn rust(&self) -> TokenStream {
        match self {
            Either::Left(a) => a.rust(),
            Either::Right(b) => b.rust(),
        }
    }
}
impl<A: RustOp, B: RustOp> RustOp for Either<A, B> {
    fn rust(&self, args: impl Iterator<Item = TokenStream>) -> TokenStream {
        match self {
            Either::Left(a) => a.rust(args),
            Either::Right(b) => b.rust(args),
        }
    }
}
impl<C,O: RustOp, T, Y: Rust, S: RustSel> ExportAst<C,O, T, S, Y> for TokenStream {
    type Var = String;

    fn get(ctx: &mut C,var: Self::Var, y: &Y) -> Self {
        let var = Ident::new(&var, Span::call_site());
        quote! {
            #var.clone()
        }
    }

    fn assign(&self,ctx: &mut C, var: Self::Var, y: &Y) -> Self {
        let var = Ident::new(&var, Span::call_site());
        quote! {
            {
                #var = #self;
                #var.clone()
            }
        }
    }

    fn select(&self,ctx: &mut C, s: &S) -> Self {
        s.rust(self)
    }

    fn append(&mut self,ctx: &mut C, i: impl Iterator<Item = Self>) {
        for j in i {
            j.to_tokens(self)
        }
    }

    fn unit(ctx: &mut C) -> Self {
        quote!(())
    }

    fn op(ctx: &mut C,o: &O, args: &[Self]) -> Self {
        let o = o.rust(args.iter().cloned());
        return o;
    }
}
impl<C,O: RustOp, T, Y: Rust, S: RustSel> CffAst<C,O, T, S, Y> for TokenStream {
    fn br_id(ctx: &mut C,a: usize) -> Self {
        quote! {__id = #a; break 'main;}
    }

    fn switch(ctx: &mut C,m: &std::collections::BTreeMap<usize, Self>) -> Self {
        let k = m.iter().map(|a| a.0);
        let v = m.iter().map(|a| a.1);
        quote! {
            match __id{
                #(#k => #v),*
            }
        }
    }

    fn forever(&self,ctx: &mut C) -> Self {
        quote! {
            'main: loop{
                #self
            }
        }
    }

    fn set_id(ctx: &mut C,a: usize) -> Self {
        quote! {__id = #a}
    }
}
impl<C,O: RustOp, T, Y: Rust, S: RustSel> ReloopAst<C,O, T, S, Y> for TokenStream {
    fn r#break(ctx: &mut C,id: u16) -> Self {
        let id = Lifetime::new(&format!("l{id}"), Span::call_site());
        quote! {
            break #id;
        }
    }

    fn r#continue(ctx: &mut C,id: u16) -> Self {
        let id = Lifetime::new(&format!("l{id}"), Span::call_site());
        quote! {
            continue #id;
        }
    }

    fn r#loop(&self,ctx: &mut C, id: u16) -> Self {
        let id = Lifetime::new(&format!("l{id}"), Span::call_site());
        quote! {
            #id: loop{
                #self
            }
        }
    }
}
impl<C,O: RustOp, T, Y: Rust, S: RustSel, W: ExportTerm<TokenStream,C, O, T, Y, S>>
    ExportTerm<TokenStream,C, O, T, Y, S> for If<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C,id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
        body: TokenStream,
    ) -> anyhow::Result<TokenStream> {
        let i = Ident::new(&format!("ssa{}", self.val.value.index()), Span::call_site());
        let se = self.val.select.rust(&quote! {#i});
        // let i = quote!{
        //     #i #se
        // };
        let e = match self.r#else.as_ref() {
            None => quote!(),
            Some(x) => x.go(ctx,&mut s, f, quote! {})?,
        };
        let t = self.then.go(ctx,s, f, quote! {})?;
        return Ok(quote! {
            #body
            if #i != 0{
                #t
            }else{
                #e
            }
        });
    }
}
pub trait RustCatch<C,O: RustOp, T, Y: Rust, S: RustSel>:
    ExportTerm<TokenStream,C, O, T, Y, S>
{
    fn bundle(&self,ctx: &mut C, body: TokenStream) -> TokenStream;
    fn unbundle(
        &self,
        ctx: &mut C,
        a: TokenStream,
        s: impl FnMut(&mut C,id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
    ) -> anyhow::Result<TokenStream>;
}
impl<C,O: RustOp, T, Y: Rust, S: RustSel, W: RustCatch<C,O, T, Y, S>>
    ExportTerm<TokenStream,C, O, T, Y, S> for Catch<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C,id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
        body: TokenStream,
    ) -> anyhow::Result<TokenStream> {
        let bundle = self.wrapped.bundle(ctx,body);
        let n = quote! {
            (||{
                return Ok::<_,anyhow::Error>(#bundle);
            })()
        };
        let u = self.wrapped.unbundle(ctx,quote! {a}, &mut s, f)?;
        let y = match self.catch.as_ref() {
            Some(a) => {
                let y = s.target(ctx,f, &a, vec![quote! {y}]);
                quote! {
                    match e.downcast(){
                        Err(x) => return Err(x),
                        Ok(y) => {
                            #y
                        }
                    }
                }
            }
            None => quote! {
                return Err(e)
            },
        };
        Ok(quote! {
            match #n{
                Ok(a) => {
                    #u
                },
                Err(e) => #y
            }
        })
    }
}
impl<C,O: RustOp, T, Y: Rust + Clone, S: RustSel + Clone> RustCatch<C,O, T, Y, S>
    for BlockTarget<O, T, Y, S>
{
    fn bundle(&self,ctx: &mut C, body: TokenStream) -> TokenStream {
        let xs = self
            .args
            .iter()
            .map(|x| ToIdAst::ssa::<C,O, T, S, Y, TokenStream>(&x.value));
        quote! {
            #body;
            (#(#xs),*)
        }
    }

    fn unbundle(
        &self,
        ctx: &mut C,
        a: TokenStream,
        mut s: impl FnMut(&mut C,id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
    ) -> anyhow::Result<TokenStream> {
        let x = s(ctx,self.block);
        let tys = self.block_types(f);
        let mut before = vec![];
        before.extend(self.args.iter().enumerate().map(|(i, a2)| {
            // let v = A::Var::from(format!("ssa{}", a.value.index()));
            let i = Index::from(i);
            return a2.select.rust(&quote! {
                #a.#i
            });
        }));
        let mut r = quote! {};
        for x in before.iter().zip(tys).enumerate().map(|(i, (x, y))| {
            <TokenStream as ExportAst<C,O, T, S, Y>>::assign(
                &x,
                ctx,
                <TokenStream as ExportAst<C,O, T, S, Y>>::Var::from(format!(
                    "bp{i}at{}",
                    self.block.index()
                )),
                y,
            )
        }) {
            x.to_tokens(&mut r);
        }
        x.to_tokens(&mut r);
        Ok(quote! {
            #r
        })
    }
}
impl<C,O: RustOp, T, Y: Rust + Clone, S: RustSel + Clone, W: RustCatch<C,O,T,Y,S>> RustCatch<C,O,T,Y,S> for If<O,T,Y,S,W>{
    fn bundle(&self,ctx: &mut C, body: TokenStream) -> TokenStream {
        let i = Ident::new(&format!("ssa{}", self.val.value.index()), Span::call_site());
        let se = self.val.select.rust(&quote! {#i});
        // let i = quote!{
        //     #i #se
        // };
        let e = match self.r#else.as_ref() {
            None => quote!(()),
            Some(x) => x.bundle(ctx,quote! {}),
        };
        let t = self.then.bundle(ctx,quote! {});
        return quote! {
            #body;
            if #i != 0{
                Either::Left(#t)
            }else{
                Either::Right(#e)
            }
        };
    }

    fn unbundle(
        &self,
        ctx: &mut C,
        a: TokenStream,
        mut s: impl FnMut(&mut C,id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
    ) -> anyhow::Result<TokenStream> {
        let t = self.then.unbundle(ctx,quote! {a}, &mut s, f)?;
        let e = match self.r#else.as_ref(){
            None => quote! {},
            Some(x) => x.unbundle(ctx,quote! {b}, &mut s, f)?
        };
        Ok(quote! {
            match #a{
                Either::Left(a) => {
                    #t
                },
                Either::Right(b) => {
                    #e
                }
            }
        })
    }
}