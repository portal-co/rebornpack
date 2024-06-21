pub mod rat;
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    hash::Hash,
    marker::PhantomData,
    ops::Index,
};

use chumsky::{prelude::*, text::keyword, Error};
use either::Either::{self, Left, Right};
use lambda_calculus::Term::{self, Abs, App, Var};
use num_bigint::{BigUint, ToBigUint};
use num_traits::{One, Zero};
use serde::{Deserialize, Serialize};
// pub fn scott_bn() -> lambda_calculus::Term {
//     return lambda_calculus::parse(
//         "\\bn.bn (\\n.\\z.\\o.n) (\\x.\\n.\\z.\\o.z x) (\\x.\\n.\\z.\\o.o x)",
//         lambda_calculus::term::Notation::Classic,
//     )
//     .unwrap();
// }
// pub fn unscott_bn() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::combinators::Y(),
//         lambda_calculus::parse(
//             "\\go.\\m.\\n.\\z.\\o.m n (\\x.z (go x n z o)) (\\x.o (go x n z o))",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),
//     );
// }
// pub fn bit() -> lambda_calculus::Term {
//     return lambda_calculus::parse(
//         "\\n.\\z.\\b.n z (\\a.\\b. a (\\a.\\b.a)) (\\a.\\b. a (\\a.\\b.b))",
//         lambda_calculus::term::Notation::Classic,
//     )
//     .unwrap();
// }
// pub fn bsucc() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::combinators::Y(),
//         lambda_calculus::parse(
//             "\\go.\\m.m (\\n.\\z.\\o.o (\\n.\\z.\\o.n)) (\\zv.\\n.\\z.\\o.o zv) (\\ov.\\n.\\z.\\o.z (go ov))",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),
//     );
// }
// pub fn bpre() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::combinators::Y(),
//         lambda_calculus::parse(
//             "\\go.\\m.m (\\n.\\z.\\o.n) (\\ov.\\n.\\z.\\o.o (go ov)) (\\zv.\\n.\\z.\\o.z zv)",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),
//     );
// }
// pub fn baddsub() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::combinators::Y(),
//         lambda_calculus::app(lambda_calculus::parse(
//             "\\bit.\\go.\\d.\\s.\\m.\\n.\\c.bit m (c (d n) n) (\\a.\\at.bit n (c (d m) m) (\\b.\\bt.\\n.\\z.\\o.(\\h.h z o (go d s at bt ((\\aob.\\aab.(\\caaob.aab aab caoab) (c aob c)) (a a (s b)) (a (s b) a)))) ((\\aneb.aneb (\\e.\\f.c f e) c) (a (\\c.\\d.s b d c) (s b)))))",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),bit()),
//     );
// }
// pub fn badd() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::app(baddsub(), bsucc()),
//         lambda_calculus::combinators::I(),
//     );
// }
// pub fn bsub() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::app(baddsub(), bpre()),
//         lambda_calculus::data::boolean::not(),
//     );
// }
// pub fn beq() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::combinators::Y(),
//         lambda_calculus::app(lambda_calculus::parse(
//             "\\bit.\\go.\\m.\\n.bit m (bit n (\\a.\\b.a) (\\_.\\_.\\a.\\b.b)) (\\a.\\at.bit n (\\a.\\b.b) (\\b.\\bt.b (a (go at bt) a) b))",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),bit()),
//     );
// }
// pub fn patch() -> lambda_calculus::Term {
//     return lambda_calculus::app(
//         lambda_calculus::parse(
//             "\\eq.\\k.\\v.\\f.\\w.eq k w v (f w)",
//             lambda_calculus::term::Notation::Classic,
//         )
//         .unwrap(),
//         beq(),
//     );
// }
// pub fn bc(x: num_bigint::BigUint) -> lambda_calculus::Term {
//     if x == Zero::zero() {
//         return lambda_calculus::parse("\\n.\\z.\\o.n", lambda_calculus::term::Notation::Classic)
//             .unwrap();
//     }
//     let two: BigUint = BigUint::one() + BigUint::one();
//     let b = x.clone() % two.clone() == One::one();
//     let b = if b {
//         lambda_calculus::data::boolean::tru()
//     } else {
//         lambda_calculus::data::boolean::fls()
//     };
//     let d = bc(x / two);
//     return lambda_calculus::app(
//         lambda_calculus::app(
//             lambda_calculus::parse(
//                 "\\b.\\d.\\n.\\z.\\o.b o z d",
//                 lambda_calculus::term::Notation::Classic,
//             )
//             .unwrap(),
//             b,
//         ),
//         d,
//     );
// }
// pub fn bcc(x: usize) -> lambda_calculus::Term {
//     return bc(x.to_biguint().unwrap());
// }
// pub fn bc64(x: u64) -> lambda_calculus::Term {
//     return bc(x.to_biguint().unwrap());
// }

pub struct Ref<'a, V: Binder, M> {
    pub current: &'a mut GTerm<V, M>,
}
impl<'a, V: Binder, M> Ref<'a, V, M> {
    pub fn app(&mut self, x: GTerm<V, M>) {
        let mut b = Box::new((x, GTerm::Undef));
        let c = unsafe { std::mem::transmute::<_, &'a mut (GTerm<V, M>, GTerm<V, M>)>(&mut *b) };

        *self.current = GTerm::App(b);
        self.current = &mut c.1;
    }
    pub fn abs(&mut self, x: V) {
        let mut b = Box::new((x, GTerm::Undef));
        let c = unsafe { std::mem::transmute::<_, &'a mut (V, GTerm<V, M>)>(&mut *b) };

        *self.current = GTerm::Abs(b);
        self.current = &mut c.1;
    }
    pub fn wrap(&mut self, v: V, x: GTerm<V, M>)
    where
        V: Clone,
    {
        self.app(app(x, var(v.clone().get_var())));
        self.abs(v.clone());
    }
}

pub trait Binder {
    type Var: Binder<Var = Self::Var>;
    type Wrap<X: Binder>: Binder<Var = X::Var>;
    fn get_var(self) -> Self::Var;
    fn get_var_ref(&self) -> &Self::Var;
    fn get_var_mut(&mut self) -> &mut Self::Var;
    fn inside<X: Binder<Var = X>>(self, f: &mut impl FnMut(Self::Var) -> X) -> Self::Wrap<X>;
}
pub trait Backend<V: Binder + Clone + Eq, M: Eq + Clone>
where
    V::Var: Eq + Ord + Clone,
{
    type Output;
    fn undef(&mut self) -> Self::Output;
    fn var(&mut self, v: V::Var) -> Self::Output;
    fn selfreference(&mut self, v: Self::Output) -> Self::Output;
    fn abs(&mut self, b: V, o: Self::Output) -> Self::Output;
    fn app(&mut self, a: Self::Output, b: Self::Output) -> Self::Output;
    fn mix(&mut self, x: M) -> Self::Output;
    fn compile(&mut self, a: GTerm<V, M>) -> Self::Output {
        if let Some(b) = a.is_sapp() {
            let c = self.compile(b);
            return self.selfreference(c);
        }
        match a {
            GTerm::Undef => self.undef(),
            GTerm::Var(v) => self.var(v),
            GTerm::Abs(b) => {
                let (a, b) = *b;
                let b = self.compile(b);
                return self.abs(a, b);
            }
            GTerm::App(b) => {
                let (a, b) = *b;
                let a = self.compile(a);
                let b = self.compile(b);
                return self.app(a, b);
            }
            GTerm::Mix(m) => self.mix(m),
        }
    }
}
pub struct Lam {}
impl<V: Binder + Clone + Eq, M: Clone + Eq> Backend<V, M> for Lam
where
    V::Var: Eq + Ord + Clone,
{
    type Output = GTerm<V, M>;
    fn undef(&mut self) -> Self::Output {
        GTerm::Undef
    }

    fn var(&mut self, v: V::Var) -> Self::Output {
        crate::var(v)
    }

    fn selfreference(&mut self, v: Self::Output) -> Self::Output {
        crate::app(v.clone(), v.clone())
    }

    fn abs(&mut self, b: V, o: Self::Output) -> Self::Output {
        crate::abs(b, o)
    }

    fn app(&mut self, a: Self::Output, b: Self::Output) -> Self::Output {
        crate::app(a, b)
    }
    fn mix(&mut self, x: M) -> Self::Output {
        return GTerm::Mix(x);
    }
}
pub trait Incr {
    fn incr(&mut self);
}
impl Incr for usize {
    fn incr(&mut self) {
        *self += 1;
    }
}
impl Incr for String {
    fn incr(&mut self) {
        *self = format!("%${self}")
    }
}
#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash)]
#[repr(transparent)]
pub struct Id(pub String);
impl Incr for Id {
    fn incr(&mut self) {
        self.0.incr();
    }
}
impl From<usize> for Id {
    fn from(value: usize) -> Self {
        return Id(format!("%{value}"));
    }
}
impl From<String> for Id {
    fn from(value: String) -> Self {
        return Id(value);
    }
}
pub trait SimpleBinder {}
impl<T: SimpleBinder> Binder for T {
    type Var = T;
    type Wrap<X: Binder> = X;
    fn get_var(self) -> Self::Var {
        return self;
    }
    fn get_var_ref(&self) -> &Self::Var {
        return self;
    }
    fn get_var_mut(&mut self) -> &mut Self::Var {
        return self;
    }
    fn inside<X: Binder<Var = X>>(self, f: &mut impl FnMut(Self::Var) -> X) -> Self::Wrap<X> {
        return f(self);
    }
}
macro_rules! simple_binder {
    ($x:ty) => {
        // impl Binder for $x {
        //     type Var = $x;
        //     type Wrap<X: Binder> = X;
        //     fn get_var(self) -> Self::Var {
        //         return self;
        //     }
        //     fn get_var_ref(&self) -> &Self::Var {
        //         return self;
        //     }
        //     fn get_var_mut(&mut self) -> &mut Self::Var {
        //         return self;
        //     }
        //     fn inside<X: Binder<Var = X>>(self, f: &mut impl FnMut(Self::Var) -> X) -> Self::Wrap<X> {
        //         return f(self);
        //     }
        // }
        impl SimpleBinder for $x {}
    };
}
simple_binder!(usize);
simple_binder!(String);
simple_binder!(Id);

#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum GTerm<V: Binder, M> {
    Undef,
    Var(V::Var),
    Abs(Box<(V, GTerm<V, M>)>),
    App(Box<(GTerm<V, M>, GTerm<V, M>)>),
    Mix(M),
}

pub fn parser<'a, Y: 'a + 'static, X: 'a + 'static + Binder, E: Error<char> + 'a + 'static>(
    x: impl Parser<char, X, Error = E> + 'a + 'static + Clone,
    v: impl Parser<char, X::Var, Error = E> + 'a + 'static + Clone,
    y: impl Parser<char, Y, Error = E> + 'a + 'static + Clone,
    splice: impl Parser<char, GTerm<X, Y>, Error = E> + 'a + 'static + Clone,
) -> impl Parser<char, GTerm<X, Y>, Error = E> + 'a + 'static + Clone
where
    <X as Binder>::Var: 'a + 'static,
    GTerm<X, Y>: 'a + 'static,
{
    return recursive(|go| {
        let var = v
            .clone()
            .map(|v| GTerm::Var(v))
            .then_ignore(text::whitespace());
        let fun = just('\\')
            .then(x.then_ignore(just('.')).separated_by(text::whitespace()))
            .then(go.clone())
            .map(|((_, k), v)| {
                k.into_iter()
                    .rev()
                    .fold(v, |a, b| GTerm::Abs(Box::new((b, a))))
            });
        let b = choice((
            var.clone(),
            fun.clone(),
            go.clone().delimited_by(just('('), just(')')),
            y.map(|x| GTerm::Mix(x)),
        ));
        choice((b
            .clone()
            .then_ignore(text::whitespace())
            .repeated()
            .at_least(1)
            .map(|x| x.into_iter().reduce(|acc, e| app(acc, e)).unwrap()),))
    });
}
// pub fn let_parser<
//     'a,
//     Y: 'a + 'static + Clone,
//     X: 'a + 'static + Binder + Clone + Eq + Ord,
//     E: Error<char> + 'a + 'static,
// >(
//     x: impl Parser<char, X, Error = E> + 'a + 'static + Clone,
//     v: impl Parser<char, X::Var, Error = E> + 'a + 'static + Clone,
//     y: impl Parser<char, Y, Error = E> + 'a + 'static + Clone,
//     splice: impl Parser<char, GTerm<X, Y>, Error = E> + 'a + 'static + Clone,
//     into: &'static impl Fn(usize) -> X,
// ) -> impl Parser<char, GTerm<X, Y>, Error = E> + 'a + 'static + Clone
// where
//     GTerm<X, Y>: Clone,
//     X::Var: Eq + Ord + Clone,
// {
//     return recursive(move |go: Recursive<'_, char, GTerm<X, Y>, E>| {
//         parser(
//             x.clone(),
//             v.clone(),
//             y,
//             keyword("let")
//                 .then_ignore(text::whitespace())
//                 .then(x.clone())
//                 .then_ignore(text::whitespace())
//                 .then_ignore(just('='))
//                 .then_ignore(text::whitespace())
//                 .then(go.clone())
//                 .then_ignore(text::whitespace())
//                 .then_ignore(keyword("in"))
//                 .then_ignore(text::whitespace())
//                 .then(go.clone())
//                 .map(move |(((_, a), b), c)| {
//                     let body = debrijun::<usize, Infallible>(lambda_calculus::combinators::Y());
//                     let y = brujin_map_f(body, &muinto);
//                     let mut body = b;
//                     if body.frees().contains(&a.clone().get_var()) {
//                         body = app(y, abs(a.clone(), body));
//                     }
//                     return app(abs(a, c), body);
//                 })
//                 .or(splice),
//         )
//     });
// }
pub fn void<I: Clone + Hash + Eq, O>() -> impl Parser<I, O, Error = Simple<I>> + Clone {
    return end().try_map(|a, b| Err(Simple::custom(b, "")));
}
pub fn str_parser() -> impl Parser<char, GTerm<String, Infallible>, Error = Simple<char>> + Clone {
    return parser(
        chumsky::text::ident(),
        chumsky::text::ident(),
        void(),
        void(),
    );
}

#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash)]
#[repr(transparent)]
pub struct Scope<T: Binder>(pub BTreeMap<T::Var, T>);

impl<V: Binder, M> GTerm<V, M> {
    pub fn to_args<'a>(&'a self, args: &mut Vec<&'a GTerm<V,M>>) -> &'a GTerm<V,M>{
        let GTerm::App(k) = self else{
            return self;
        };
        let (a,b) = k.as_ref();
        let a = a.to_args(args);
        args.push(b);
        return a;
    }
    pub fn map_vars<X: Binder<Var = X>>(
        self,
        f: &mut impl FnMut(V::Var) -> X,
    ) -> GTerm<V::Wrap<X>, M> {
        match self {
            GTerm::Undef => GTerm::Undef,
            GTerm::Var(v) => GTerm::Var(f(v)),
            GTerm::Abs(k) => {
                let (k, w) = *k;
                let k = k.inside(f);
                let w = w.map_vars(f);
                return abs(k, w);
            }
            GTerm::App(a) => {
                let (a, b) = *a;
                return app(a.map_vars(f), b.map_vars(f));
            }
            GTerm::Mix(m) => GTerm::Mix(m),
        }
    }
    pub fn map_all<X: Binder>(
        self,
        f: &mut impl FnMut(V::Var) -> X::Var,
        g: &mut impl FnMut(V) -> X,
    ) -> GTerm<X, M> {
        match self {
            GTerm::Undef => GTerm::Undef,
            GTerm::Var(v) => GTerm::Var(f(v)),
            GTerm::Abs(k) => {
                let (k, w) = *k;
                let k = g(k);
                let w = w.map_all(f, g);
                return abs(k, w);
            }
            GTerm::App(a) => {
                let (a, b) = *a;
                return app(a.map_all(f, g), b.map_all(f, g));
            }
            GTerm::Mix(m) => GTerm::Mix(m),
        }
    }
    pub fn map_mix<N>(self, f: &mut impl FnMut(M) -> N) -> GTerm<V, N> {
        return self.lower_mix(&mut |a| GTerm::Mix(f(a)));
    }
    pub fn lower_mix<N>(self, f: &mut impl FnMut(M) -> GTerm<V, N>) -> GTerm<V, N> {
        match self {
            GTerm::Undef => GTerm::Undef,
            GTerm::Var(v) => GTerm::Var(v),
            GTerm::Abs(k) => {
                let (k, w) = *k;
                return abs(k, w.lower_mix(f));
            }
            GTerm::App(a) => {
                let (a, b) = *a;
                return app(a.lower_mix(f), b.lower_mix(f));
            }
            GTerm::Mix(m) => f(m),
        }
    }
    pub fn subst(self, f: &mut impl FnMut(&V::Var) -> Option<GTerm<V, M>>) -> GTerm<V, M> {
        match self {
            GTerm::Undef => GTerm::Undef,
            GTerm::Var(v) => match f(&v) {
                Some(a) => a,
                None => GTerm::Var(v),
            },
            GTerm::Abs(k) => {
                let (k, w) = *k;
                match f(k.get_var_ref()) {
                    Some(_) => abs(k, w),
                    None => abs(k, w.subst(f)),
                }
            }
            GTerm::App(b) => {
                let (a, b) = *b;
                return app(a.subst(f), b.subst(f));
            }
            GTerm::Mix(m) => GTerm::Mix(m),
        }
    }
}
impl<V: Binder, M> GTerm<V, M>
where
    V::Var: Eq,
{
    pub fn subst_var_fun(self, v: &V::Var, f: &mut impl FnMut() -> GTerm<V, M>) -> GTerm<V, M> {
        return self.subst(&mut |x| if x == v { Some(f()) } else { None });
    }
}
impl<V: Binder + Clone, M: Clone> GTerm<V, M>
where
    V::Var: Eq + Clone,
{
    pub fn subst_var(self, v: &V::Var, f: GTerm<V, M>) -> GTerm<V, M> {
        return self.subst_var_fun(v, &mut || f.clone());
    }
    pub fn apply(&mut self, o: GTerm<V, M>) {
        if let GTerm::Abs(a) = self {
            let (ref mut k, ref mut w) = **a;
            *self = w.clone().subst_var(k.get_var_mut(), o);
            return;
        }
        *self = app(self.clone(), o);
    }
}
#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash)]
pub struct Scott<V: Binder, M> {
    pub cases: Vec<V::Var>,
    pub current_case: usize,
    pub with: Vec<GTerm<V, M>>,
}
impl<V: Binder + Clone, M: Clone> Scott<V, M>
where
    V::Var: Eq + Ord + Clone,
{
    pub fn apply(mut self, mut other: GTerm<V, M>) -> Either<GTerm<V, M>, Scott<V, M>> {
        if self.current_case == 0 {
            for x in self.with.into_iter() {
                other.apply(x);
            }
            return Left(other);
        }
        self.current_case -= 1;
        self.cases.pop();
        return Right(self);
    }
    pub fn render(mut self) -> GTerm<V, M>
    where
        V: Binder<Var = V>,
    {
        let mut r = var(self.cases[self.current_case].clone());
        for w in self.with.into_iter() {
            r = app(r, w);
        }
        for c in self.cases.into_iter().rev() {
            r = abs(c, r);
        }
        return r;
    }
}
impl<V: Binder + Clone + Eq, M: Clone + Eq> GTerm<V, M>
where
    V::Var: Eq + Ord + Clone,
{
    pub fn is_sapp(&self) -> Option<GTerm<V, M>> {
        if let GTerm::App(a) = self {
            let (ref a, ref b) = **a;
            if a.clone() == b.clone() {
                return Some(a.clone());
            }
        }
        None
    }
}
impl<V: Binder + Clone, M: Clone> GTerm<V, M>
where
    V::Var: Eq + Ord + Clone,
{
    pub fn frees_internal(&self, o: &mut BTreeSet<V::Var>) {
        match self {
            GTerm::Undef => {}
            GTerm::Var(s) => {
                o.insert(s.clone());
            }
            GTerm::Abs(a) => {
                let (ref k, ref w) = **a;
                let mut r = w.frees();
                r.remove(k.get_var_ref());
                o.append(&mut r);
            }
            GTerm::App(a) => {
                let (ref a, ref b) = **a;
                a.frees_internal(o);
                b.frees_internal(o);
            }
            GTerm::Mix(m) => {}
        }
    }
    pub fn frees(&self) -> BTreeSet<V::Var> {
        let mut r = BTreeSet::new();
        self.frees_internal(&mut r);
        return r;
    }
    pub fn scott(&self) -> Option<Scott<V, M>> {
        let mut this = self;
        let mut v = vec![];
        while let GTerm::Abs(k) = this {
            let (ref k, ref w) = **k;
            v.push(k.clone().get_var());
            this = w;
        }
        let mut args = vec![];
        loop {
            if let GTerm::App(b) = this {
                let (ref a, ref b) = **b;
                // if let GTerm::Var(r) = a {
                //     // if v{
                //     //     return Some((v.len(),args));
                //     // }else{
                //     //     return None;
                //     // }
                //     match v.iter().enumerate().find(|a| a.1 == r.get_var_ref()) {
                //         Some((a, b)) => {
                //             return Some(Scott {
                //                 case_count: v.len(),
                //                 current_case: a,
                //                 with: args.into_iter().rev().collect(),
                //                 pda: PhantomData,
                //             })
                //         }
                //         None => return None,
                //     }
                // }
                for f in b.frees() {
                    if !v.contains(&f) {
                        return None;
                    }
                }
                args.push(b.clone());
                this = a;
            } else {
                break;
            }
        }
        if let GTerm::Var(r) = this {
            match v.iter().enumerate().find(|a| a.1 == r.get_var_ref()) {
                Some((a, b)) => {
                    return Some(Scott {
                        cases: v,
                        current_case: a,
                        with: args.into_iter().rev().collect(),
                    })
                }
                None => return None,
            }
        }
        return None;
    }
}

pub fn var<V: Binder, M>(a: V::Var) -> GTerm<V, M> {
    return GTerm::Var(a);
}

pub fn abs<V: Binder, M>(a: V, b: GTerm<V, M>) -> GTerm<V, M> {
    return GTerm::Abs(Box::new((a, b)));
}

pub fn app<V: Binder, M>(a: GTerm<V, M>, b: GTerm<V, M>) -> GTerm<V, M> {
    return GTerm::App(Box::new((a, b)));
}

pub fn debrijun_internal<X: From<usize> + Binder, Y>(x: Term, depth: usize) -> GTerm<X, Y>
where
    <X as Binder>::Var: From<usize>,
{
    match x {
        Var(0) => GTerm::Undef,
        Var(v) => GTerm::Var((depth - v).into()),
        Abs(a) => GTerm::Abs(Box::new((depth.into(), debrijun_internal(*a, depth + 1)))),
        App(b) => {
            let (a, b) = *b;
            let a = debrijun_internal(a, depth);
            let b = debrijun_internal(b, depth);
            return GTerm::App(Box::new((a, b)));
        }
    }
}
pub fn debrijun<X: From<usize> + Binder, Y>(x: Term) -> GTerm<X, Y>
where
    <X as Binder>::Var: From<usize>,
{
    return debrijun_internal(x, 1);
}
pub fn brujin_internal<X: Binder>(
    t: GTerm<X, Infallible>,
    m: &BTreeMap<<X as Binder>::Var, usize>,
) -> Term
where
    <X as Binder>::Var: Eq + Ord + Clone,
{
    match t {
        GTerm::Undef => Var(0),
        GTerm::Var(a) => Var(m[&a]),
        GTerm::Abs(a) => {
            let (k, w) = *a;
            let mut n = BTreeMap::new();
            for (a, b) in m.iter() {
                n.insert(a.clone(), *b + 1);
            }
            n.insert(k.get_var(), 1);
            return Abs(Box::new(brujin_internal(w, &n)));
        }
        GTerm::App(b) => {
            let (a, b) = *b;
            let a = brujin_internal(a, m);
            let b = brujin_internal(b, m);
            return App(Box::new((a, b)));
        }
        GTerm::Mix(x) => match x {},
    }
}
pub fn brujin<X: Binder>(t: GTerm<X, Infallible>) -> Term
where
    <X as Binder>::Var: Eq + Ord + Clone,
{
    return brujin_internal(t, &BTreeMap::new());
}
pub fn brujin_map_f_internal<X: Binder, Y: Binder, M>(
    t: GTerm<X, Infallible>,
    m: &BTreeMap<<X as Binder>::Var, usize>,
    depth: usize,
    into: &mut impl FnMut(usize) -> Y,
) -> GTerm<Y, M>
where
    <X as Binder>::Var: Eq + Ord + Clone,
{
    match t {
        GTerm::Undef => GTerm::Undef,
        GTerm::Var(a) => GTerm::Var(into(depth - m[&a]).get_var()),
        GTerm::Abs(a) => {
            let (k, w) = *a;
            let mut n = BTreeMap::new();
            for (a, b) in m.iter() {
                n.insert(a.clone(), *b + 1);
            }
            n.insert(k.get_var(), 1);
            return abs(into(depth), brujin_map_f_internal(w, &n, depth + 1, into));
        }
        GTerm::App(b) => {
            let (a, b) = *b;
            let a = brujin_map_f_internal(a, m, depth, into);
            let b = brujin_map_f_internal(b, m, depth, into);
            return app(a, b);
        }
        GTerm::Mix(x) => match x {},
    }
}
pub fn brujin_map<X: Binder, Y: Binder + From<usize>, M>(t: GTerm<X, Infallible>) -> GTerm<Y, M>
where
    <X as Binder>::Var: Eq + Ord + Clone,
    <Y as Binder>::Var: From<usize>,
{
    return brujin_map_f_internal(t, &BTreeMap::new(), 1, &mut |a| a.into());
}
pub fn brujin_map_f<X: Binder, Y: Binder, M>(
    t: GTerm<X, Infallible>,
    into: &mut impl FnMut(usize) -> Y,
) -> GTerm<Y, M>
where
    <X as Binder>::Var: Eq + Ord + Clone,
{
    return brujin_map_f_internal(t, &BTreeMap::new(), 1, into);
}

#[cfg(test)]
mod tests {
    use chumsky::Stream;

    use super::*;
    #[test]
    fn rtrip() {
        let a = lambda_calculus::parse(
            "\\a.\\b.\\c.c a b (\\d.d c b a)",
            lambda_calculus::term::Notation::Classic,
        )
        .unwrap();
        assert_eq!(
            brujin::<usize>(brujin_map(debrijun::<usize, Infallible>(a.clone()))),
            a
        );
    }
    #[test]
    fn parse() {
        let s = "\\a.\\b.\\c.c a b (\\d.d c b a)";
        let a = lambda_calculus::parse(s, lambda_calculus::term::Notation::Classic).unwrap();
        let b = str_parser().map(|x| brujin(x)).parse(Stream::from(s));
        assert_eq!(b.unwrap(), a);
    }
    #[test]
    fn r#let() {
        let s = "let a = \\b.a in \\c.c a";
        let b = str_parser().parse(s);
        assert!(b.is_ok());
    }
    #[test]
    fn scott_simple() {
        let a = debrijun::<usize, Infallible>(
            lambda_calculus::parse("\\a.a", lambda_calculus::term::Notation::Classic).unwrap(),
        );
        // assert_eq!(
        //     a.scott().unwrap(),
        //     Scott {
        //         case_count: 1,
        //         current_case: 0,
        //         with: vec![]
        //     }
        // )
        let a = a.scott().unwrap();
        // assert_eq!(a.case_count, 1);
        assert_eq!(a.current_case, 0);
        assert_eq!(a.with, vec![]);
    }
    #[test]
    fn scott_field() {
        let ab = debrijun::<usize, Infallible>(
            lambda_calculus::parse(
                "\\c.\\a.a (\\b.b)",
                lambda_calculus::term::Notation::Classic,
            )
            .unwrap(),
        );
        let b = debrijun::<usize, Infallible>(
            lambda_calculus::parse("\\a.a", lambda_calculus::term::Notation::Classic).unwrap(),
        );
        // assert_eq!(
        //     a.scott().unwrap(),
        //     Scott {
        //         case_count: 1,
        //         current_case: 0,
        //         with: vec![]
        //     }
        // )
        let a = ab.scott().unwrap();
        assert_eq!(brujin(a.clone().render()), brujin(ab.clone()));
        // assert_eq!(a.case_count, 2);
        assert_eq!(a.current_case, 1);
        assert_eq!(a.with.len(), 1);
        assert_eq!(brujin(b), brujin(a.with[0].clone()))
    }
    #[test]
    fn scott_bool() {
        let ab = debrijun::<usize, Infallible>(
            lambda_calculus::parse("\\b.\\a.a", lambda_calculus::term::Notation::Classic).unwrap(),
        );
        // assert_eq!(
        //     a.scott().unwrap(),
        //     Scott {
        //         case_count: 1,
        //         current_case: 0,
        //         with: vec![]
        //     }
        // )
        let a = ab.scott().unwrap();
        assert_eq!(brujin(a.clone().render()), brujin(ab.clone()));
        // assert_eq!(a.case_count, 2);
        assert_eq!(a.current_case, 1);
        assert_eq!(a.with, vec![]);
    }
}
