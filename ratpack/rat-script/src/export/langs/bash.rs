use rat_ast::export::ExportAst;
use rat_ir::util::{BinOp, ObjectOriented};

use crate::script;

use super::{CLike, ScrOp, Script};

script!(Bash);
impl<C, O: ScrOp<C, Bash>, T, Y, S> ExportAst<C, O, T, Y, S> for Bash {
    type Var = String;

    fn get(ctx: &mut C, var: Self::Var, y: &S) -> Self {
        Self(format!("${var}"))
    }

    fn assign(&self, ctx: &mut C, var: Self::Var, y: &S) -> Self {
        Self(format!("{var} = {self}"))
    }

    fn select(&self, ctx: &mut C, s: &Y) -> Self {
        self.clone()
    }

    fn append(&mut self, ctx: &mut C, i: impl Iterator<Item = Self>) {
        for j in i {
            self.0.extend(j.0.chars())
        }
    }

    fn unit(ctx: &mut C) -> Self {
        Self(format!(""))
    }

    fn op(ctx: &mut C, o: &O, args: &[Self]) -> Self {
        return o.op(ctx, args.iter().cloned());
    }
}
