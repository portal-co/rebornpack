use quote::{format_ident, quote};
use rat_ast::export::{rust::RustOp, Emit, ExportTerm};
use rat_ir::{
    bi::simple::{Gm, SimpleOp},
    no_push,
    util::{BinOp, Catch, ObjectOriented, Push},
    BlockTarget, SaneTerminator,
};

use crate::script;

use super::{CLike, ScrCLikeAst, ScrOp, Script};
pub mod bash;

script!(ECMAScript);
impl CLike for ECMAScript {
    fn r#break(id: u16) -> Self {
        Self(format!("break l{id};"))
    }

    fn r#continue(id: u16) -> Self {
        Self(format!("continue l{id};"))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(format!("l{id}: while(1){{{}}};", self.0))
    }
}
script!(AssemblyScript);
impl CLike for AssemblyScript {
    fn r#break(id: u16) -> Self {
        Self(format!("break l{id};"))
    }

    fn r#continue(id: u16) -> Self {
        Self(format!("continue l{id};"))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(format!("l{id}: while(1){{{}}};", self.0))
    }
}
impl<T: Emit<AssemblyScript>> Emit<AssemblyScript> for Vec<T> {
    fn emit(&self) -> AssemblyScript {
        if let [a] = self.as_slice() {
            return a.emit();
        }
        AssemblyScript(format!(
            "{{{}}}",
            self.iter()
                .enumerate()
                .map(|(a, b)| format!("p{a}: {}", b.emit()))
                .collect::<Vec<_>>()
                .join(",")
        ))
    }
}
script!(C);
impl CLike for C {
    fn r#break(id: u16) -> Self {
        Self(format!("goto b{id};"))
    }

    fn r#continue(id: u16) -> Self {
        Self(format!("goto c{id};"))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(format!("c{id}: while(1){{{}}};b{id}:", self.0))
    }
}
script!(Java);
impl CLike for Java {
    fn r#break(id: u16) -> Self {
        Self(format!("break l{id};"))
    }

    fn r#continue(id: u16) -> Self {
        Self(format!("continue l{id};"))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(format!("l{id}: while(1){{{}}};", self.0))
    }
}
impl<C> ScrOp<C, ECMAScript> for BinOp {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = ECMAScript>) -> ECMAScript {
        let arg0 = args.next().unwrap();
        let arg1 = args.next().unwrap();
        ECMAScript(match self {
            BinOp::Add => format!("{arg0} + {arg1}"),
            BinOp::Sub => format!("{arg0} - {arg1}"),
            BinOp::Mul => format!("{arg0} * {arg1}"),
            BinOp::And => format!("{arg0} & {arg1}"),
            BinOp::Or => format!("{arg0} | {arg1}"),
            BinOp::Xor => format!("{arg0} ^ {arg1}"),
            BinOp::DivU => todo!(),
            BinOp::ModU => todo!(),
            BinOp::DivS => format!("{arg0} / {arg1}"),
            BinOp::ModS => format!("(({arg0} % {arg1}) + {arg1}) % {arg1}"),
            BinOp::Shl => format!("{arg0} << {arg1}"),
            BinOp::ShrS => format!("{arg0} >> {arg1}"),
            BinOp::ShrU => format!("{arg0} >>> {arg1}"),
        })
    }
}
impl<C> ScrOp<C, ECMAScript> for ObjectOriented {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = ECMAScript>) -> ECMAScript {
        ECMAScript(match self {
            ObjectOriented::NewObj(m) => format!(
                "new {m}({})",
                args.map(|a| a.0).collect::<Vec<_>>().join(",")
            ),
            ObjectOriented::GetField(f) => format!("{}.{f}", args.next().unwrap()),
            ObjectOriented::SetField(f) => {
                format!("{}.{f} = {}", args.next().unwrap(), args.next().unwrap())
            }
            ObjectOriented::CallMethod(m) => format!(
                "{}.{m}({})",
                args.next().unwrap(),
                args.map(|a| a.0).collect::<Vec<_>>().join(",")
            ),
        })
    }
}
impl<X> ScrOp<X, C> for BinOp {
    fn op(&self, ctx: &mut X, mut args: impl Iterator<Item = C>) -> C {
        let arg0 = args.next().unwrap();
        let arg1 = args.next().unwrap();
        C(match self {
            BinOp::Add => format!("{arg0} + {arg1}"),
            BinOp::Sub => format!("{arg0} - {arg1}"),
            BinOp::Mul => format!("{arg0} * {arg1}"),
            BinOp::And => format!("{arg0} & {arg1}"),
            BinOp::Or => format!("{arg0} | {arg1}"),
            BinOp::Xor => format!("{arg0} ^ {arg1}"),
            BinOp::DivU => format!("{arg0} / {arg1}"),
            BinOp::ModU => format!("{arg0} % {arg1}"),
            BinOp::DivS => todo!(),
            BinOp::ModS => todo!(),
            BinOp::Shl => format!("{arg0} << {arg1}"),
            BinOp::ShrU => format!("{arg0} >> {arg1}"),
            BinOp::ShrS => todo!(),
        })
    }
}
impl<C> ScrOp<C, Java> for BinOp {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = Java>) -> Java {
        let arg0 = args.next().unwrap();
        let arg1 = args.next().unwrap();
        Java(match self {
            BinOp::Add => format!("{arg0} + {arg1}"),
            BinOp::Sub => format!("{arg0} - {arg1}"),
            BinOp::Mul => format!("{arg0} * {arg1}"),
            BinOp::And => format!("{arg0} & {arg1}"),
            BinOp::Or => format!("{arg0} | {arg1}"),
            BinOp::Xor => format!("{arg0} ^ {arg1}"),
            BinOp::DivU => todo!(),
            BinOp::ModU => todo!(),
            BinOp::DivS => format!("{arg0} / {arg1}"),
            BinOp::ModS => format!("(({arg0} % {arg1}) + {arg1}) % {arg1}"),
            BinOp::Shl => format!("{arg0} << {arg1}"),
            BinOp::ShrS => format!("{arg0} >> {arg1}"),
            BinOp::ShrU => format!("{arg0} >>> {arg1}"),
        })
    }
}
impl<C> ScrOp<C, Java> for ObjectOriented {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = Java>) -> Java {
        Java(match self {
            ObjectOriented::NewObj(m) => format!(
                "new {m}({})",
                args.map(|a| a.0).collect::<Vec<_>>().join(",")
            ),
            ObjectOriented::GetField(f) => format!("{}.{f}", args.next().unwrap()),
            ObjectOriented::SetField(f) => {
                format!("{}.{f} = {}", args.next().unwrap(), args.next().unwrap())
            }
            ObjectOriented::CallMethod(m) => format!(
                "{}.{m}({})",
                args.next().unwrap(),
                args.map(|a| a.0).collect::<Vec<_>>().join(",")
            ),
        })
    }
}
#[derive(Clone, Copy, Debug, serde::Serialize, serde::Deserialize)]
pub struct WasmBindgen<X>(pub X);
impl<C, X: ScrOp<C, ECMAScript>> RustOp<C> for WasmBindgen<X> {
    fn rust(
        &self,
        ctx: &mut C,
        args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        let args: Vec<_> = args.collect();
        let js_body = self.0.op(
            ctx,
            args.iter()
                .enumerate()
                .map(|a| ECMAScript(format!("${}", a.0))),
        );
        let js_header = format!(
            "export function go({}){{return {js_body};}}",
            args.iter()
                .enumerate()
                .map(|a| format!("${}", a.0))
                .collect::<Vec<_>>()
                .join(",")
        );
        let rargs = args
            .iter()
            .enumerate()
            .map(|a| format_ident!("p{}", a.0))
            .collect::<Vec<_>>();
        quote! {
            {
                #[wasm_bindgen(inline_js = #js_header)]
                extern "C"{
                    fn go(#(#rargs : JsValue),*) -> JsValue;
                }
                go(#(#args .unchecked_ref().clone()),*).unckecked_into()
            }
        }
    }
}
impl<C, X: Push<C>> Push<C> for WasmBindgen<X> {
    fn push(b: C) -> either::Either<Self, C> {
        X::push(b).map_left(WasmBindgen)
    }
}

script!(Swift);
impl CLike for Swift {
    fn r#break(id: u16) -> Self {
        Self(format!("break l{id};"))
    }

    fn r#continue(id: u16) -> Self {
        Self(format!("continue l{id};"))
    }

    fn r#loop(&self, id: u16) -> Self {
        Self(format!("l{id}: while(1){{{}}};", self.0))
    }
}
impl<C, O: ScrOp<C, Java>, T, Y, S, W: ExportTerm<ScrCLikeAst<Java>, C, O, T, Y, S>>
    ExportTerm<ScrCLikeAst<Java>, C, O, T, Y, S> for Catch<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> ScrCLikeAst<Java>,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ScrCLikeAst<Java>,
    ) -> anyhow::Result<ScrCLikeAst<Java>> {
        use rat_ast::export::EmitTarget;
        let Some(k) = self.catch.as_ref() else {
            return self.wrapped.go(ctx, s, f, body);
        };
        Ok(ScrCLikeAst(
            format!(
                "try{{
                    {}
                }}catch(Throwable _catch){{
                    {}
                }}",
                self.wrapped.go(ctx, &mut s, f, body)?.0,
                s.target(ctx, f, k, vec![ScrCLikeAst(format!("_catch").into())])
            )
            .into(),
        ))
    }
}
impl<
        C,
        O: ScrOp<C, ECMAScript>,
        T,
        Y,
        S,
        W: ExportTerm<ScrCLikeAst<ECMAScript>, C, O, T, Y, S>,
    > ExportTerm<ScrCLikeAst<ECMAScript>, C, O, T, Y, S> for Catch<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> ScrCLikeAst<ECMAScript>,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ScrCLikeAst<ECMAScript>,
    ) -> anyhow::Result<ScrCLikeAst<ECMAScript>> {
        use rat_ast::export::EmitTarget;
        let Some(k) = self.catch.as_ref() else {
            return self.wrapped.go(ctx, s, f, body);
        };
        Ok(ScrCLikeAst(
            format!(
                "try{{
                    {}
                }}catch(_catch){{
                    {}
                }}",
                self.wrapped.go(ctx, &mut s, f, body)?.0,
                s.target(ctx, f, k, vec![ScrCLikeAst(format!("_catch").into())])
            )
            .into(),
        ))
    }
}
impl<X, O: ScrOp<X, C>, T, Y, S, W: ExportTerm<ScrCLikeAst<C>, X, O, T, Y, S>>
    ExportTerm<ScrCLikeAst<C>, X, O, T, Y, S> for Catch<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut X,
        mut s: impl FnMut(&mut X, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> ScrCLikeAst<C>,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ScrCLikeAst<C>,
    ) -> anyhow::Result<ScrCLikeAst<C>> {
        use rat_ast::export::EmitTarget;
        let Some(k) = self.catch.as_ref() else {
            return self.wrapped.go(ctx, s, f, body);
        };
        Ok(ScrCLikeAst(
            format!(
                "try{{
                    {}
                }}catch(...){{
                    {}
                }}",
                self.wrapped.go(ctx, &mut s, f, body)?.0,
                s.target(
                    ctx,
                    f,
                    k,
                    vec![ScrCLikeAst(format!("std::current_exception()").into())]
                )
            )
            .into(),
        ))
    }
}
