use std::{iter::once, sync::Arc};

use rat_ast::export::{rust::RustOp, CffAst, ExportAst, ExportTerm, ReloopAst, ToIdAst};
use rat_ir::util::{BinOp, Catch, If, Push};
use swc_core::{
    atoms::Atom,
    base::{Compiler, PrintArgs},
    common::{SourceMap, Span, Spanned},
    ecma::ast::{
        AssignExpr, AssignOp, BinExpr, BinaryOp, BindingIdent, BlockStmt, Bool, BreakStmt,
        CatchClause, ContinueStmt, Decl, ExportDecl, ExportNamedSpecifier, ExportSpecifier, Expr,
        ExprStmt, FnDecl, Function, Ident, IfStmt, LabeledStmt, Lit, Module, ModuleItem,
        NamedExport, Null, Number, Param, Pat, ReturnStmt, Stmt, SwitchCase, SwitchStmt, TryStmt,
        UnaryExpr, UnaryOp, WhileStmt,
    },
};
#[derive(Clone, Copy, Debug, serde::Serialize, serde::Deserialize)]
pub struct WasmBindgen<X>(pub X);
impl<C: Spanned, X: JsOp<C>> RustOp<C> for WasmBindgen<X> {
    fn rust(
        &self,
        ctx: &mut C,
        args: impl Iterator<Item = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        use quote::{format_ident, quote};
        let args: Vec<_> = args.collect();
        let s = ctx.span();
        let js_body = self.0.op(
            ctx,
            args.iter()
                .enumerate()
                .map(|a| Expr::Ident(Ident::new(format!("${}", a.0).into(), s.clone()))),
        );
        let orig = Ident::new("go".into(), s.clone());
        let js_header = Module {
            span: s.clone(),
            body: vec![ModuleItem::ModuleDecl(
                swc_core::ecma::ast::ModuleDecl::ExportDecl(ExportDecl {
                    span: s.clone(),
                    decl: Decl::Fn(FnDecl {
                        ident: orig,
                        declare: false,
                        function: Box::new(Function {
                            params: args
                                .iter()
                                .enumerate()
                                .map(|a| Ident::new(format!("${}", a.0).into(), s.clone()))
                                .map(|a| Param {
                                    span: s.clone(),
                                    decorators: vec![],
                                    pat: Pat::Ident(a.into()),
                                })
                                .collect(),
                            decorators: vec![],
                            span: s.clone(),
                            body: Some(BlockStmt {
                                span: s.clone(),
                                stmts: vec![Stmt::Return(ReturnStmt {
                                    span: s.clone(),
                                    arg: Some(Box::new(js_body)),
                                })],
                            }),
                            is_generator: false,
                            is_async: false,
                            type_params: None,
                            return_type: None,
                        }),
                    }),
                }),
            )],
            shebang: None,
        };
        let c = Compiler::new(Arc::new(SourceMap::default()));
        let js_header = c.print(&js_header, PrintArgs::default()).unwrap().code;
        // let js_header = format!(
        //     "export function go({}){{return {js_body};}}",
        //     args.iter()
        //         .enumerate()
        //         .map(|a| format!("${}", a.0))
        //         .collect::<Vec<_>>()
        //         .join(",")
        // );
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
#[derive(Clone)]
pub enum ExprOrStmt {
    Stmt(Stmt),
    Expr(Expr),
}
impl ExprOrStmt {
    pub fn stmt(self) -> Stmt {
        match self {
            ExprOrStmt::Stmt(s) => s,
            ExprOrStmt::Expr(e) => Stmt::Expr(ExprStmt {
                span: e.span(),
                expr: Box::new(e),
            }),
        }
    }
}
pub trait JsOp<C> {
    fn op(&self, ctx: &mut C, args: impl Iterator<Item = Expr>) -> Expr;
}
pub trait JsTy<C> {}
pub trait JsSel<C> {
    fn select(&self, ctx: &mut C, e: Expr) -> Expr;
}
impl<C: Spanned, O: JsOp<C>, T, Y: JsTy<C>, S: JsSel<C>> ExportAst<C, O, T, S, Y> for ExprOrStmt {
    type Var = Atom;

    fn get(ctx: &mut C, var: Self::Var, y: &Y) -> Self {
        Self::Expr(Expr::Ident(Ident::new(var, ctx.span())))
    }

    fn assign(&self, ctx: &mut C, var: Self::Var, y: &Y) -> Self {
        Self::Expr(Expr::Assign(AssignExpr {
            span: ctx.span(),
            op: AssignOp::Assign,
            left: swc_core::ecma::ast::AssignTarget::Simple(
                swc_core::ecma::ast::SimpleAssignTarget::Ident(BindingIdent::from(Ident::new(
                    var,
                    ctx.span(),
                ))),
            ),
            right: Box::new(match self.clone() {
                Self::Expr(e) => e,
                Self::Stmt(s) => return Self::Stmt(s),
            }),
        }))
    }

    fn select(&self, ctx: &mut C, s: &S) -> Self {
        match self.clone() {
            ExprOrStmt::Stmt(s) => Self::Stmt(s),
            ExprOrStmt::Expr(e) => Self::Expr(s.select(ctx, e)),
        }
    }

    fn append(&mut self, ctx: &mut C, i: impl Iterator<Item = Self>) {
        replace_with::replace_with_or_abort(self, move |x| {
            let x = x.stmt();
            return Self::Stmt(Stmt::Block(BlockStmt {
                span: x.span(),
                stmts: vec![x].into_iter().chain(i.map(|a| a.stmt())).collect(),
            }));
        })
    }

    fn unit(ctx: &mut C) -> Self {
        Self::Expr(Expr::Lit(Lit::Null(Null { span: ctx.span() })))
    }

    fn op(ctx: &mut C, o: &O, args: &[Self]) -> Self {
        Self::Expr(
            o.op(
                ctx,
                args.iter()
                    .filter_map(|x| match x {
                        ExprOrStmt::Stmt(_) => None,
                        ExprOrStmt::Expr(a) => Some(a),
                    })
                    .cloned(),
            ),
        )
    }
}
impl<C: Spanned, O: JsOp<C>, T, Y: JsTy<C>, S: JsSel<C>> CffAst<C, O, T, S, Y> for ExprOrStmt {
    fn br_id(ctx: &mut C, a: usize) -> Self {
        let s = ctx.span();
        let mut a = <Self as CffAst<C, O, T, S, Y>>::set_id(ctx, a);
        <Self as ExportAst<C, O, T, S, Y>>::append(
            &mut a,
            ctx,
            once(ExprOrStmt::Stmt(Stmt::Break(BreakStmt {
                span: Span::dummy_with_cmt(),
                label: Some(Ident::new("cff".into(), s)),
            }))),
        );
        return a;
    }

    fn set_id(ctx: &mut C, a: usize) -> Self {
        return Self::Expr(Expr::Assign(AssignExpr {
            span: Span::dummy_with_cmt(),
            op: AssignOp::Assign,
            left: swc_core::ecma::ast::AssignTarget::Simple(
                swc_core::ecma::ast::SimpleAssignTarget::Ident(
                    Ident::new(Atom::from("cff"), ctx.span()).into(),
                ),
            ),
            right: Box::new(Expr::Lit(Lit::Num(a.into()))),
        }));
    }

    fn switch(ctx: &mut C, m: &std::collections::BTreeMap<usize, Self>) -> Self {
        return Self::Stmt(Stmt::Labeled(LabeledStmt {
            span: ctx.span(),
            label: Ident::new("cff".into(), ctx.span()),
            body: Box::new(Stmt::Switch(SwitchStmt {
                span: ctx.span(),
                discriminant: Box::new(Expr::Ident(Ident::new("cff".into(), ctx.span()))),
                cases: m
                    .iter()
                    .map(|(a, y)| SwitchCase {
                        span: ctx.span(),
                        test: Some(Box::new(Expr::Lit(Lit::Num((*a).into())))),
                        cons: vec![y.clone().stmt()],
                    })
                    .collect(),
            })),
        }));
    }

    fn forever(&self, ctx: &mut C) -> Self {
        todo!()
    }
}
impl<C: Spanned, O: JsOp<C>, T, Y: JsTy<C>, S: JsSel<C>> ReloopAst<C, O, T, S, Y> for ExprOrStmt {
    fn r#break(ctx: &mut C, id: u16) -> Self {
        ExprOrStmt::Stmt(Stmt::Break(BreakStmt {
            span: ctx.span(),
            label: Some(Ident::new(format!("l{id}").into(), ctx.span())),
        }))
    }

    fn r#continue(ctx: &mut C, id: u16) -> Self {
        ExprOrStmt::Stmt(Stmt::Continue(ContinueStmt {
            span: ctx.span(),
            label: Some(Ident::new(format!("l{id}").into(), ctx.span())),
        }))
    }

    fn r#loop(&self, ctx: &mut C, id: u16) -> Self {
        ExprOrStmt::Stmt(Stmt::Labeled(LabeledStmt {
            span: ctx.span(),
            label: Ident::new(format!("l{id}").into(), ctx.span()),
            body: Box::new(Stmt::While(WhileStmt {
                span: ctx.span(),
                test: Box::new(Expr::Lit(Lit::Bool(Bool {
                    span: ctx.span(),
                    value: true,
                }))),
                body: Box::new(self.clone().stmt()),
            })),
        }))
    }
}
impl<
        C: Spanned,
        O: JsOp<C>,
        T,
        Y: JsTy<C>,
        S: JsSel<C>,
        W: ExportTerm<ExprOrStmt, C, O, T, Y, S>,
    > ExportTerm<ExprOrStmt, C, O, T, Y, S> for If<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> ExprOrStmt,
        f: &rat_ir::Func<O, T, Y, S>,
        mut body: ExprOrStmt,
    ) -> anyhow::Result<ExprOrStmt> {
        let test = self.val.value.ssa::<C, O, T, S, Y, ExprOrStmt>();
        let u = <ExprOrStmt as ExportAst<C, O, T, S, Y>>::unit(ctx);
        let t = self.then.go(ctx, &mut s, f, u)?;
        let u = <ExprOrStmt as ExportAst<C, O, T, S, Y>>::unit(ctx);
        let e = match self.r#else.as_ref() {
            None => None,
            Some(x) => Some(x.go(ctx, s, f, u)?),
        };
        let i = ExprOrStmt::Stmt(Stmt::If(IfStmt {
            span: ctx.span(),
            test: Box::new(Expr::Ident(Ident::new(test, ctx.span()))),
            cons: Box::new(t.stmt()),
            alt: e.map(|x| Box::new(x.stmt())),
        }));
        <ExprOrStmt as ExportAst<C, O, T, S, Y>>::append(&mut body, ctx, once(i));
        return Ok(body);
    }
}
impl<
        C: Spanned,
        O: JsOp<C>,
        T,
        Y: JsTy<C>,
        S: JsSel<C>,
        W: ExportTerm<ExprOrStmt, C, O, T, Y, S>,
    > ExportTerm<ExprOrStmt, C, O, T, Y, S> for Catch<O, T, Y, S, W>
{
    fn go(
        &self,
        ctx: &mut C,
        mut s: impl FnMut(&mut C, id_arena::Id<rat_ir::Block<O, T, Y, S>>) -> ExprOrStmt,
        f: &rat_ir::Func<O, T, Y, S>,
        body: ExprOrStmt,
    ) -> anyhow::Result<ExprOrStmt> {
        use rat_ast::export::EmitTarget;
        let Some(k) = self.catch.as_ref() else {
            return self.wrapped.go(ctx, s, f, body);
        };
        let kx = self.wrapped.go(ctx, &mut s, f, body)?;
        let i = Ident::new("_catch".into(), ctx.span());
        let t = ExprOrStmt::Expr(Expr::Ident(i.clone()));
        let t = s.target(ctx, f, k, vec![t]);
        Ok(ExprOrStmt::Stmt(Stmt::Try(Box::new(TryStmt {
            span: ctx.span(),
            block: BlockStmt {
                span: ctx.span(),
                stmts: vec![kx.stmt()],
            },
            handler: Some(CatchClause {
                span: ctx.span(),
                param: Some(Pat::Ident(i.into())),
                body: BlockStmt {
                    span: ctx.span(),
                    stmts: vec![t.stmt()],
                },
            }),
            finalizer: None,
        }))))
    }
}
impl<C> JsOp<C> for BinaryOp {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = Expr>) -> Expr {
        let a = args.next().unwrap();
        let b = args.next().unwrap();
        Expr::Bin(BinExpr {
            span: a.span(),
            op: self.clone(),
            left: Box::new(a),
            right: Box::new(b),
        })
    }
}
impl<C> JsOp<C> for UnaryOp {
    fn op(&self, ctx: &mut C, mut args: impl Iterator<Item = Expr>) -> Expr {
        let a = args.next().unwrap();
        // let b = args.next().unwrap();
        Expr::Unary(UnaryExpr {
            span: a.span(),
            op: self.clone(),
            arg: Box::new(a),
        })
    }
}
