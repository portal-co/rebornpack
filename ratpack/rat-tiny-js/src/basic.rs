use std::iter::{empty, once};

use either::Either;
use rat_ir::{Bound, SaneTerminator};

use crate::{BasicTinyOp, BasicTinyTerm};

impl<O, T, Y, S> SaneTerminator<O, T, Y, S> for BasicTinyTerm<O, T, Y, S> {
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a rat_ir::Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            BasicTinyTerm::Panic => Either::Left(Either::Left(empty())),
            BasicTinyTerm::Just(t) => Either::Right(Either::Left(t.args.iter())),
            BasicTinyTerm::Ret(a) => Either::Left(Either::Right(once(a))),
            BasicTinyTerm::If {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Right(
                once(cond)
                    .chain(if_true.args.iter())
                    .chain(if_false.args.iter()),
            )),
        }
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut rat_ir::Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            BasicTinyTerm::Panic => Either::Left(Either::Left(empty())),
            BasicTinyTerm::Just(t) => Either::Right(Either::Left(t.args.iter_mut())),
            BasicTinyTerm::Ret(a) => Either::Left(Either::Right(once(a))),
            BasicTinyTerm::If {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Right(
                once(cond)
                    .chain(if_true.args.iter_mut())
                    .chain(if_false.args.iter_mut()),
            )),
        }
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a rat_ir::BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            BasicTinyTerm::Panic => Either::Left(empty()),
            BasicTinyTerm::Just(a) => Either::Right(Either::Left(once(a))),
            BasicTinyTerm::Ret(_) => Either::Left(empty()),
            BasicTinyTerm::If {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Right(vec![if_true, if_false].into_iter())),
        }
    }

    fn t2s_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = &'a mut rat_ir::BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        match self {
            BasicTinyTerm::Panic => Either::Left(empty()),
            BasicTinyTerm::Just(a) => Either::Right(Either::Left(once(a))),
            BasicTinyTerm::Ret(_) => Either::Left(empty()),
            BasicTinyTerm::If {
                cond,
                if_true,
                if_false,
            } => Either::Right(Either::Right(vec![if_true, if_false].into_iter())),
        }
    }
}
pub struct Basic{}
impl Bound for Basic{
    type O<O, T, Y, S> = BasicTinyOp<O,T,Y,S>;

    type T<O, T, Y, S> = BasicTinyTerm<O,T,Y,S>;

    type Y<O, T, Y, S> = ();

    type S<O, T, Y, S> = ();
}