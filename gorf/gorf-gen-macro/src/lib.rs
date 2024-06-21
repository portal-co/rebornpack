extern crate proc_macro;
use std::convert::Infallible;

use chumsky::Parser;
use gorf_gen_core::Opts;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Ident, LitStr, Token};

struct X {
    pub s: LitStr,
    pub opts: Opts,
}
impl Parse for X {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut o = X {
            s: input.parse()?,
            opts: Opts {
                path: quote! {::lambda_rt},
            },
        };
        while input.lookahead1().peek(Ident) {
            let i: Ident = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            match i.to_string().as_str() {
                "crate_path" => {
                    let s: syn::LitStr = input.parse()?;
                    o.opts.path = s.parse()?;
                }
                _ => return Err(syn::Error::new(i.span(), "unexpected type")),
            };
            let _comma: Token![,] = input.parse()?;
        }
        return Ok(o);
    }
}
#[proc_macro]
pub fn lamc(tokens: TokenStream) -> TokenStream {
    let s = parse_macro_input!(tokens as X);
    let g = gorf_core::str_parser().parse(s.s.value()).unwrap();
    return TokenStream::from(gorf_gen_core::emit(&g, &s.opts));
}
#[cfg(test)]
mod tests {
    use super::*;
}
