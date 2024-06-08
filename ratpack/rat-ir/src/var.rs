// use std::{
//     collections::{BTreeMap, BTreeSet},
//     marker::PhantomData,
// };

// use id_arena::Id;

// use crate::{Block, Builder, Func, Then, Unit};
// #[derive(Clone)]
// pub struct RootData<K> {
//     pub mapper: BTreeMap<K, usize>,
//     pub ordering: Vec<K>,
// }
// pub trait StrVarBuilder<O, T, Y, S, V, K> {
//     type Result;
//     fn vars(&self, go: &mut BTreeSet<K>);
//     fn build(self, go: BTreeMap<K, usize>)
//         -> impl VarBuilder<O, T, Y, S, V, Result = Self::Result>;
//     fn with<B, C>(self, b: B, c: C) -> LiftW2<Self, B, C>
//     where
//         Self: Sized,
//     {
//         return LiftW2 {
//             first: self,
//             second: b,
//             combine: c,
//         };
//     }
//     fn root_data(&self) -> RootData<K>
//     where
//         K: Clone + Eq + Ord + 'static,
//     {
//         let mut v = BTreeSet::new();
//         self.vars(&mut v);
//         let m = v.iter().enumerate().map(|(a, b)| (b.clone(), a)).collect();
//         return RootData {
//             mapper: m,
//             ordering: v.iter().map(|a| a.clone()).collect(),
//         };
//     }
//     fn root_with<F>(self, boot: F) -> Root<Self, F, K>
//     where
//         Self: Sized,
//     {
//         return Root {
//             wrapped: self,
//             boot,
//             phantom: PhantomData,
//         };
//     }
//     fn root(
//         self,
//         m: BTreeMap<K, V>,
//     ) -> impl Builder<O, T, Y, S, Result = (Self::Result, BTreeMap<K, V>)>
//     where
//         V: Clone,
//         K: Eq + Ord + Clone + 'static,
//         Self: Sized,
//     {
//         return self.root_with(move |k| {
//             let n = m.get(&k).cloned().unwrap();
//             return Unit(n);
//         });
//     }
//     fn then<X>(self, other: X, keys: Vec<K>) -> ThenWithKeys<Self,X,K> where Self: Sized{
//         return ThenWithKeys{first: self, then: other, keys};
//     }
// }
// #[derive(Clone)]
// pub struct Root<X, F, K> {
//     pub wrapped: X,
//     pub boot: F,
//     pub phantom: PhantomData<K>,
// }
// impl<
//         O,
//         T,
//         Y,
//         S,
//         V: Clone,
//         K: Eq + Ord + Clone + 'static,
//         X: StrVarBuilder<O, T, Y, S, V, K>,
//         F: FnMut(K) -> B,
//         B: Builder<O, T, Y, S, Result = V>,
//     > Builder<O, T, Y, S> for Root<X, F, K>
// {
//     type Result = (X::Result, BTreeMap<K, V>);

//     fn build(
//         mut self: Box<Self>,
//         func: &mut Func<O, T, Y, S>,
//         mut root: Id<Block<O, T, Y, S>>,
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
//         let d = self.wrapped.root_data();
//         let mut m = vec![];
//         for o in d.ordering.into_iter() {
//             let (a, b) = Box::new((self.boot)(o)).build(func, root)?;
//             root = b;
//             m.push(a);
//         }
//         let w = self.wrapped.build(d.mapper.clone());
//         let (r, root) = Box::new(w).build_with_vars(func, root, &mut m)?;
//         let mut n = BTreeMap::new();
//         for (mm, i) in d.mapper.iter() {
//             n.insert(mm.clone(), m[*i].clone());
//         }
//         return Ok(((r, n), root));
//     }
// }
// #[derive(Clone)]
// pub struct ThenWithKeys<A, B, K> {
//     pub first: A,
//     pub then: B,
//     pub keys: Vec<K>,
// }
// impl<
//         O,
//         T,
//         Y,
//         S,
//         V,
//         K: Eq + Ord + Clone,
//         A: StrVarBuilder<O, T, Y, S, V, K>,
//         B: FnOnce(A::Result) -> C,
//         C: StrVarBuilder<O, T, Y, S, V, K>,
//     > StrVarBuilder<O, T, Y, S, V, K> for ThenWithKeys<A, B, K>
// {
//     type Result = C::Result;

//     fn vars(&self, go: &mut BTreeSet<K>) {
//         self.first.vars(go);
//         for k in self.keys.iter() {
//             go.insert(k.clone());
//         }
//     }

//     fn build(
//         self,
//         go: BTreeMap<K, usize>,
//     ) -> impl VarBuilder<O, T, Y, S, V, Result = Self::Result> {
//         let mut s = BTreeSet::new();
//         self.vars(&mut s);
//         return Box::new(self.first.build(go.clone())).then(move |value| {
//             let t = go
//                 .into_iter()
//                 .filter(|a| s.contains(&a.0))
//                 .map(|(a, b)| (a.clone(), b))
//                 .collect();
//             (self.then)(value).build(t)
//         });
//     }
// }
// #[derive(Clone)]
// pub struct LiftW2<A, B, C> {
//     pub first: A,
//     pub second: B,
//     pub combine: C,
// }
// impl<
//         O,
//         T,
//         Y,
//         S,
//         V,
//         K: Clone,
//         A: StrVarBuilder<O, T, Y, S, V, K>,
//         B: StrVarBuilder<O, T, Y, S, V, K>,
//         C: FnOnce(A::Result, B::Result) -> D,
//         D: Builder<O, T, Y, S>,
//     > StrVarBuilder<O, T, Y, S, V, K> for LiftW2<A, B, C>
// {
//     type Result = D::Result;

//     fn vars(&self, go: &mut BTreeSet<K>) {
//         self.first.vars(go);
//         self.second.vars(go);
//     }

//     fn build(
//         self,
//         go: BTreeMap<K, usize>,
//     ) -> impl VarBuilder<O, T, Y, S, V, Result = Self::Result> {
//         let a = self.first.build(go.clone());
//         let b = self.second.build(go);
//         return Box::new(a)
//             .then(move |x| Box::new(b).then(move |y| Wrap(Box::new((self.combine)(x, y)))));
//     }
// }

// pub trait VarBuilder<O, T, Y, S, V> {
//     type Result;
//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, Y, S>,
//         root: Id<Block<O, T, Y, S>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)>;
//     fn then<B>(self: Box<Self>, b: B) -> Then<Self, B> {
//         return Then {
//             first: self,
//             then: b,
//         };
//     }
// }
// #[derive(Clone)]
// pub struct Wrap<A>(pub Box<A>);
// impl<O, T, S, Y, V, A: Builder<O, T, S, Y>> VarBuilder<O, T, S, Y, V> for Wrap<A> {
//     type Result = A::Result;

//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, S, Y>,
//         root: Id<Block<O, T, S, Y>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
//         return self.0.build(func, root);
//     }
// }
// impl<O, T, S, Y, V, K, A: Builder<O, T, S, Y>> StrVarBuilder<O, T, S, Y, V, K> for Wrap<A> {
//     type Result = A::Result;

//     fn vars(&self, go: &mut BTreeSet<K>) {}

//     fn build(
//         self,
//         go: BTreeMap<K, usize>,
//     ) -> impl VarBuilder<O, T, S, Y, V, Result = Self::Result> {
//         return self;
//     }
// }
// #[derive(Clone)]
// pub struct Thread<A, V> {
//     pub wrapped: Box<A>,
//     pub vars: Vec<V>,
// }
// impl<O, T, S, Y, V, A: VarBuilder<O, T, S, Y, V>> Builder<O, T, S, Y> for Thread<A, V> {
//     type Result = (A::Result, Vec<V>);

//     fn build(
//         mut self: Box<Self>,
//         func: &mut Func<O, T, S, Y>,
//         root: Id<Block<O, T, S, Y>>,
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
//         let (w, k) = self.wrapped.build_with_vars(func, root, &mut self.vars)?;
//         return Ok(((w, self.vars), k));
//     }
// }
// #[derive(Clone, Copy)]
// pub struct Get<T> {
//     pub v: T,
// }
// impl<O, T, Y, S, V: Clone> VarBuilder<O, T, Y, S, V> for Get<usize> {
//     type Result = V;

//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, Y, S>,
//         root: Id<Block<O, T, Y, S>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
//         return Ok((vars[self.v].clone(), root));
//     }
// }
// impl<O, T, Y, S, V: Clone, K: Eq + Ord + Clone> StrVarBuilder<O, T, Y, S, V, K> for Get<K> {
//     type Result = V;

//     fn vars(&self, go: &mut BTreeSet<K>) {
//         go.insert(self.v.clone());
//     }

//     fn build(
//         self,
//         go: BTreeMap<K, usize>,
//     ) -> impl VarBuilder<O, T, Y, S, V, Result = Self::Result> {
//         return Get {
//             v: go.get(&self.v).copied().unwrap(),
//         };
//     }
// }
// #[derive(Clone)]
// pub struct Set<A, T> {
//     pub v: T,
//     pub wrapped: A,
// }
// impl<
//         O,
//         T,
//         S,
//         Y,
//         V: Clone,
//         K: Eq + Ord + Clone,
//         A: StrVarBuilder<O, T, S, Y, V, K, Result = (V, X)>,
//         X,
//     > StrVarBuilder<O, T, S, Y, V, K> for Set<A, K>
// {
//     type Result = (V, X);

//     fn vars(&self, go: &mut BTreeSet<K>) {
//         self.wrapped.vars(go);
//         go.insert(self.v.clone());
//     }

//     fn build(
//         self,
//         go: BTreeMap<K, usize>,
//     ) -> impl VarBuilder<O, T, S, Y, V, Result = Self::Result> {
//         let v = go.get(&self.v).copied().unwrap();
//         let w = self.wrapped.build(go);
//         return Set { v: v, wrapped: w };
//     }
// }
// impl<O, T, S, Y, V: Clone, A: VarBuilder<O, T, S, Y, V, Result = (V, X)>, X>
//     VarBuilder<O, T, S, Y, V> for Set<A, usize>
// {
//     type Result = (V, X);

//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, S, Y>,
//         root: Id<Block<O, T, S, Y>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
//         let ((v, x), root) = Box::new(self.wrapped).build_with_vars(func, root, vars)?;
//         vars[self.v] = v.clone();
//         return Ok(((v, x), root));
//     }
// }
// impl<
//         O,
//         T,
//         Y,
//         S,
//         V,
//         A: VarBuilder<O, T, Y, S, V>,
//         B: FnOnce(A::Result) -> C,
//         C: VarBuilder<O, T, Y, S, V>,
//     > VarBuilder<O, T, Y, S, V> for Then<A, B>
// {
//     type Result = C::Result;

//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, Y, S>,
//         root: Id<Block<O, T, Y, S>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, Y, S>>)> {
//         let (a, root) = self.first.build_with_vars(func, root, vars)?;
//         return Box::new((self.then)(a)).build_with_vars(func, root, vars);
//     }
// }
// impl<O, T, S, Y, V, A> VarBuilder<O, T, S, Y, V> for Unit<A> {
//     type Result = A;

//     fn build_with_vars(
//         self: Box<Self>,
//         func: &mut Func<O, T, S, Y>,
//         root: Id<Block<O, T, S, Y>>,
//         vars: &mut [V],
//     ) -> anyhow::Result<(Self::Result, Id<Block<O, T, S, Y>>)> {
//         Ok((self.0, root))
//     }
// }
