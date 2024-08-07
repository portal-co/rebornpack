use std::convert::Infallible;
use std::iter::once;

use anyhow::Context;
use proc_macro2::TokenStream;
use quasiquote::quasiquote;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use rat_ast::export::rust::RustSel;
use rat_ast::export::ExportTerm;
use rat_ast::export::ToIdAst;
use rat_ast::export::{
    rust::{Rust, RustOp},
    Emit,
};
use rat_ir::util::Extract;
use rat_script::export::{langs::AssemblyScript, ScrOp};
use waffle::entity::EntityRef;
use waffle::Memory;

use crate::import::WaffleTerm;
use crate::Host;
use crate::{Guest, OpWrapper};
// pub fn bindname(a: &str) -> String {
//     let mut v = vec![];
//     for k in a.chars() {
//         if k.is_alphanumeric() {
//             v.push(k)
//         } else {
//             v.extend(format!("_{}_", k as u32).chars());
//         }
//     }
//     return v.into_iter().collect();
// }
// impl<Y: Rust> RustOp for Guest<crate::Import<Y>> {
//     fn rust(&self, args: impl Iterator<Item = TokenStream>) -> TokenStream {
//         let root: Result<TokenStream, syn::Error> = syn::parse_str(&self.rt);
//         let root = match root{
//             Ok(a) => a,
//             Err(e) => return e.into_compile_error(),
//         };
//         let s = quote! {
//             #root::externref
//         }.to_string();
//         quasiquote! {
//             unsafe{
//                 #[#root::externref::externref(crate = #s)]
//                 #[wasm_import_module = #{&self.wrapped.module}]
//                 extern "C"{
//                     #[wasm_import_name = #{&self.wrapped.name}]
//                     fn go(#{
//                         let a = self.wrapped.types.iter().map(|a|a.rust());
//                         let b = (0..).map(|a|format_ident!("p{a}"));
//                         quote!{
//                             #(#b : #a),*
//                         }
//                     }) -> (#{
//                         let a = self.wrapped.rets.iter().map(|a|a.rust());
//                         // let b = (0..).map(|a|format_ident!("p{a}"));
//                         quote!{
//                             #(#a),*
//                         }
//                     })
//                 }
//                 go(#(#args .clone()),*)
//             }
//         }
//     }
// }

// impl RustOp for Guest<OpWrapper> {
//     fn rust(
//         &self,
//         mut args: impl Iterator<Item = proc_macro2::TokenStream>,
//     ) -> proc_macro2::TokenStream {
//         let n = &self.wrapped.0;
//         let root: Result<TokenStream, syn::Error> = syn::parse_str(&self.rt);
//         let root = match root{
//             Ok(a) => a,
//             Err(e) => return e.into_compile_error(),
//         };
//         match n {
//             _ if waffle::op_traits::mem_count(n) == 1 => {
//                 // let mut mem = Memory::invalid();
//                 // waffle::op_traits::rewrite_mem(&mut o.clone(), &mut [();4], |m,_|{
//                 //     mem = *m;
//                 //     Ok::<(),Infallible>(())
//                 // }).unwrap();
//                 // let clean = o.to_string();
//                 let clean = format_ident!("{}", n.to_string().split_once("<").unwrap().0);
//                 // let m2 = mem;
//                 // let mem = self.mem(m2);
//                 // let mut vals = vals.iter().map(|a|format_ident!("{a}"));
//                 // let rt = if self.module.memories[m2].memory64{
//                 //     quote! {u64}
//                 // }else{
//                 //     quote! {u32}
//                 // };
//                 let offset = waffle::op_traits::memory_arg(n).unwrap().offset;
//                 let offset = {
//                     quote! {#offset}
//                 };
//                 let val = args.next().unwrap();
//                 let vals = once(quote! {(#val.clone() + #offset)}).chain(args.map(|w| quote! {#w}));
//                 quasiquote! {
//                     match #root::wars_rt::#clean(&mut unsafe{#root ::wars_rt::host_memory()},#(#root::wars_rt::func::cast::<_,_,C>(#vals .clone())),*){
//                         Ok(a) => a,
//                         Err(e) => panic!(e)
//                     }
//                 }
//             }
//             _ => {
//                 let clean = format_ident!("{n}");
//                 // let vals = vals.iter().map(|a|format_ident!("{a}"));
//                 quasiquote! {
//                     match #root::wars_rt::#clean(#(#root::wars_rt::func::cast::<_,_,C>(#args .clone())),*){
//                         Ok(a) => a,
//                         Err(e) => panic!(e)
//                     }
//                 }
//             }
//         }
//     }
// }
pub trait HasWarsRoot {
    fn wars_root(&self) -> TokenStream;
}
impl<C: HasWarsRoot> RustOp<C> for Host<OpWrapper> {
    fn rust(
        &self,
        ctx: &mut C,
        mut args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        let n = &self.wrapped.0;
        // let root: Result<TokenStream, syn::Error> = syn::parse_str(&self.rt);
        // let root = match root{
        //     Ok(a) => a,
        //     Err(e) => return e.into_compile_error(),
        // };
        let root = ctx.wars_root();
        match n {
            _ if waffle::op_traits::mem_count(n) == 1 => {
                // let mut mem = Memory::invalid();
                // waffle::op_traits::rewrite_mem(&mut n.clone(), &mut [(); 4], |m, _| {
                //     mem = *m;
                //     Ok::<(), Infallible>(())
                // })
                // .unwrap();
                // let clean = o.to_string();
                let clean = format_ident!("{}", n.to_string().split_once("<").unwrap().0);
                // let m2 = mem;
                // let mem = self.mem(m2);
                // let mut vals = vals.iter().map(|a|format_ident!("{a}"));
                // let rt = if self.module.memories[m2].memory64{
                //     quote! {u64}
                // }else{
                //     quote! {u32}
                // };
                let offset = waffle::op_traits::memory_arg(n).unwrap().offset;
                let offset = {
                    quote! {#offset}
                };
                let arg0 = args.next().unwrap();
                // let mname = format_ident!("{mem}");
                let val = args.next().unwrap();
                let vals = once(quote! {(#val.clone() + #offset)}).chain(args.map(|w| quote! {#w}));
                quasiquote! {
                    match #root::wars_rt::#clean(&mut #arg0,#(#root::wars_rt::func::cast::<_,_,C>(#vals .clone())),*){
                        Ok(a) => a,
                        Err(e) => panic!(e)
                    }
                }
            }
            _ => {
                let clean = format_ident!("{n}");
                // let vals = vals.iter().map(|a|format_ident!("{a}"));
                quasiquote! {
                    match #root::wars_rt::#clean(#(#root::wars_rt::func::cast::<_,_,C>(#args .clone())),*){
                        Ok(a) => a,
                        Err(e) => panic!(e)
                    }
                }
            }
        }
    }
}

impl<C: HasWarsRoot, O: RustOp<C>, T, Y: Rust<C>, S: RustSel<C>>
    ExportTerm<TokenStream, C, O, T, Y, S> for WaffleTerm<O, T, Y, S, Infallible>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> TokenStream,
        f: &rat_ir::Func<O, T, Y, S>,
        mut body: TokenStream,
    ) -> anyhow::Result<TokenStream> {
        use rat_ast::export::EmitTarget;
        let root = ctx.wars_root();
        match self {
            WaffleTerm::Br(t) => s.target(ctx, f, t, vec![]),
            WaffleTerm::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let cond = cond.value.ssa::<C, O, T, S, Y, TokenStream>();
                let cond = format_ident!("{cond}");
                let if_true = s.target(ctx, f, if_true, vec![]);
                let if_false = s.target(ctx, f, if_false, vec![]);
                quote! {
                    if #cond != 0{
                        #if_true
                    }else{
                        #if_false
                    }
                }
            }
            WaffleTerm::Select {
                value,
                cases,
                default,
            } => {
                let cond = value.value.ssa::<C, O, T, S, Y, TokenStream>();
                let cond = format_ident!("{cond}");
                let default = s.target(ctx, f, default, vec![]);
                let cases = cases.iter().enumerate().map(|(i, a)| {
                    let a = s.target(ctx, f, a, vec![]);
                    quote! {
                        #i => #a
                    }
                });
                quote! {
                    match #cond{
                        #(#cases),*
                        _ => #default
                    }
                }
            }
            WaffleTerm::Ret(vs) => {
                let vs = vs.iter().map(|v| {
                    let v = v.value.ssa::<C, O, T, S, Y, TokenStream>();
                    let v = format_ident!("{v}");
                    quote! {
                        #root::wars_rt::func::cast::<_,_,C>(#v)
                    }
                });
                quote! {
                    return (#(#vs),*)
                }
            }
            WaffleTerm::ReturnCall { func, args } => todo!(),
            WaffleTerm::ReturnCallIndirect { table, args } => todo!(),
            WaffleTerm::ReturnCallRef { args } => todo!(),
            WaffleTerm::Unreachable => todo!(),
        }
        .to_tokens(&mut body);
        Ok(body)
    }
}
// impl<Y: Emit<rat_script::export::langs::AssemblyScript>>
//     ScrOp<rat_script::export::langs::AssemblyScript> for Guest<crate::Import<Y>>
// {
//     fn op(
//         &self,
//         args: impl Iterator<Item = rat_script::export::langs::AssemblyScript>,
//     ) -> rat_script::export::langs::AssemblyScript {
//         let n = format!("{}.{}",self.wrapped.module,self.wrapped.name);
//         let n = bindname(&n);
//         AssemblyScript(format!(
//             "@external(\"{}\",\"{}\")declare function {n}({}): {{{}}};{n}({})",
//             self.wrapped.module,
//             self.wrapped.name,
//             self.wrapped.types
//                 .iter()
//                 .map(|a| a.emit())
//                 .enumerate()
//                 .map(|(a, b)| format!("p{a}: {b}"))
//                 .collect::<Vec<_>>()
//                 .join(","),
//             self.wrapped.rets
//                 .iter()
//                 .map(|a| a.emit())
//                 .enumerate()
//                 .map(|(a, b)| format!("p{a}: {b}"))
//                 .collect::<Vec<_>>()
//                 .join(","),
//             args.map(|a| a.0).collect::<Vec<_>>().join(",")
//         ))
//     }
// }
// impl ScrOp<AssemblyScript> for Guest<OpWrapper>{
//     fn op(&self, args: impl Iterator<Item = AssemblyScript>) -> AssemblyScript {
//         let n = &self.wrapped.0;
//         let s = n.to_string();
//         let mut r = args.map(|a|a.0).collect::<Vec<_>>();
//         if let Some(a) = waffle::op_traits::memory_arg(n){
//             r[0] = format!("{} + {}",r[0],a.offset)
//         }
//         AssemblyScript(format!("{}.{}({})",self.rt,s.split_once("<").map(|a|a.0).unwrap_or(&s),r.join(",")))
//     }
// }
