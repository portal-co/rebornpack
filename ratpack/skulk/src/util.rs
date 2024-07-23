use lexpr::cons::ListIter;
use rat_ast::import::build_fn;
use rat_ir::{
    util::{BinOp, If},
    BuildFn,
};

use crate::{LispOp, LispSeed};

impl<O, T, Y, S> LispOp<O, T, Y, S> for BinOp {
    fn lisp(x: &lexpr::Value) -> anyhow::Result<Self> {
        let (lexpr::Value::Symbol(s) | lexpr::Value::Keyword(s)) = x else {
            anyhow::bail!("not a symbol or keyword")
        };
        Ok(match s.as_ref() {
            "add" => BinOp::Add,
            "and" => BinOp::And,
            "divs" => BinOp::DivS,
            "divu" => BinOp::DivU,
            "mods" => BinOp::ModS,
            "modu" => BinOp::ModU,
            "mul" => BinOp::Mul,
            "or" => BinOp::Or,
            "shl" => BinOp::Shl,
            "shrs" => BinOp::ShrS,
            "shru" => BinOp::ShrU,
            "sub" => BinOp::Sub,
            "xor" => BinOp::Xor,
            _ => anyhow::bail!("invalid bin op"),
        })
    }
}
pub struct IfSeed<W> {
    pub cond: usize,
    pub then: W,
    pub r#else: Option<W>,
}
impl<'a, O, T, Y, S: Default, W: LispSeed<'a, O, T, Y, S, U>, U> LispSeed<'a, O, T, Y, S, If<O, T, Y, S, U>>
    for IfSeed<W>
{
    fn bind<'b>(
        &self,
        mut go: &'b mut impl crate::TermTargetFn<'a, O, T, Y, S>,
    ) -> anyhow::Result<
        impl rat_ast::import::VarBuilder<
                O,
                T,
                Y,
                S,
                id_arena::Id<rat_ir::Value<O, T, Y, S>>,
                Result =  If<O, T, Y, S, U>,
            > + 'a,
    > {
        let then = self.then.bind(&mut *go)?;
        let r#else = match self.r#else.as_ref() {
            None => None,
            Some(a) => Some(a.bind(&mut *go)?),
        };
        let idx = self.cond;
        return Ok(build_fn(move|f, k, v: &mut [id_arena::Id<rat_ir::Value<O, T, Y, S>>]| {
                use rat_ast::import::VarBuilder;
                let mut vt = v.to_owned();
                let (tu,k) = Box::new(then).build_with_vars(f, k, &mut vt)?;
                let mut vt = v.to_owned();
                let (tv,k) = match r#else{
                    None => (None,k),
                    Some(a) => {
                        let (a,b) = Box::new(a).build_with_vars(f, k, &mut vt)?;
                        (Some(a),b)
                    }
                };
                let cond = v[idx];
                return Ok((If{ val: rat_ir::Use { value: cond, select: S::default() }, then: tu, r#else: tv },k));
            },
        ));
    }
}
