use std::convert::Infallible;

use chumsky::Parser;
use gorf_core::GTerm;
use proc_macro2::TokenStream;
// use proc_macro::TokenStream;
use quasiquote::quasiquote;
use quote::{format_ident, quote};
use syn::{parse_macro_input, LitStr};
pub struct Opts {
    pub path: TokenStream,
}
pub fn emit(a: &GTerm<String, Infallible>, opts: &Opts) -> proc_macro2::TokenStream {
    let rt = &opts.path;
    let n = {
        if let Some(s) = a.scott() {
            let mut v = quote! {
                let mut _tv = ::alloc::vec::Vec::new();
            };
            for a in s.with {
                let a = emit(&a, opts);
                v = quote! {
                    #v;
                    _tv.push(#a)
                }
            }
            let i = s.current_case;
            let n = s.cases.len();
            v = quote! {
                {
                #v;
                #rt::scott(#i,#n,_tv.into())
                }
            };
            return v;
        }
        match a {
            GTerm::Undef => quote! {
                unreachable!()
            },
            GTerm::Var(v) => {
                let v = format_ident!("{v}");
                quote! {
                    (#v).clone()
                }
            }
            GTerm::Abs(b) => {
                let (b, v) = b.as_ref();
                let w = v.frees();
                let f = w.iter().map(|a| format_ident!("{a}"));
                let mut t = quote! {
                    #(let #f = #f.clone());*
                };
                let v = emit(v, opts);
                quote! {
                    {
                        #t;
                       #rt::B(::alloc::sync::Arc::new(move|#b|{
                            #t;
                            return #v;
                        }))
                    }
                }
            }
            GTerm::App(a) => {
                let (a, b) = a.as_ref();
                let a = emit(a, opts);
                match b {
                    GTerm::Var(v) => {
                        let v = format_ident!("{v}");
                        quote! {
                            (#a.0)(&#v)
                        }
                    }
                    _ => quasiquote! {
                        {
                            let _0 = #{emit(b,opts)};
                            (#a.0)(&_0)
                        }
                    },
                }
            }
            GTerm::Mix(_) => todo!(),
        }
    };
    let w = a.frees();
    let f = w.iter().map(|a| format_ident!("{a}"));
    let mut t = quote! {
        #(let #f = #f.clone());*
    };
    return quote! {
        {#t;
        #rt::l(move||#n)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
}
