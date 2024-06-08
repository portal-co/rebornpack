use rat_ast::export::ExportAst;
use rat_ir::util::{BinOp, ObjectOriented};

use crate::script;

use super::{CLike, ScrOp, Script};

script!(Bash);
impl<O: ScrOp<Bash>,T,Y,S> ExportAst<O,T,Y,S> for Bash{
    type Var = String;

    fn get(var: Self::Var, y: &S) -> Self {
        Self(format!("${var}"))
    }

    fn assign(&self, var: Self::Var, y: &S) -> Self {
        Self(format!("{var} = {self}"))
    }

    fn select(&self, s: &Y) -> Self {
        self.clone()
    }

    fn append(&mut self, i: impl Iterator<Item = Self>) {
        for j in i{
            self.0.extend(j.0.chars())
        }
    }

    fn unit() -> Self {
        Self(format!(""))
    }

    fn op(o: &O, args: &[Self]) -> Self {
        return o.op(args.iter().cloned());
    }
}