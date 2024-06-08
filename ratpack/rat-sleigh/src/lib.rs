use std::{collections::BTreeMap, iter::once};

use id_arena::Id;
use jingle_sleigh::{Instruction, RegisterManager, SpaceManager};
use rat_ir::{util::Push, Block, BlockTarget, Func, SaneTerminator, Use, Value};
pub struct Switch<O, T, Y, S> {
    pub scutinee: Use<O, T, Y, S>,
    pub targets: BTreeMap<u64, BlockTarget<O, T, Y, S>>,
    pub default: Option<BlockTarget<O, T, Y, S>>,
}
impl<O, T, Y: Clone, S: Clone> Clone for Switch<O, T, Y, S> {
    fn clone(&self) -> Self {
        Self {
            scutinee: self.scutinee.clone(),
            targets: self.targets.clone(),
            default: self.default.clone(),
        }
    }
}
impl<O, T, Y, S> SaneTerminator<O, T, Y, S> for Switch<O, T, Y, S> {
    fn uses<'a>(&'a self) -> impl Iterator<Item = &'a Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(&self.scutinee)
            .chain(self.default.iter().flat_map(|x| x.uses()))
            .chain(self.targets.iter().flat_map(|x| x.1.uses()))
    }

    fn uses_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Use<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        once(&mut self.scutinee)
            .chain(self.default.iter_mut().flat_map(|x| x.uses_mut()))
            .chain(self.targets.iter_mut().flat_map(|x| x.1.uses_mut()))
    }

    fn t2s<'a>(&'a self) -> impl Iterator<Item = &'a BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.targets.iter().map(|a| a.1).chain(self.default.iter())
    }

    fn t2s_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut BlockTarget<O, T, Y, S>> + 'a
    where
        O: 'a,
        T: 'a,
        Y: 'a,
        S: 'a,
    {
        self.targets
            .iter_mut()
            .map(|a| a.1)
            .chain(self.default.iter_mut())
    }
}

pub struct SleighBoot<'a, 'b, O, T, Y, S, Regs: RegisterManager> {
    pub insts: BTreeMap<
        u64,
        (
            &'a Instruction,
            Id<Block<O, T, Y, S>>,
            Vec<Id<Value<O, T, Y, S>>>,
        ),
    >,
    pub selector: Id<Block<O, T, Y, S>>,
    pub regs: &'b Regs,
}
impl<
        'a,
        'b,
        O,
        T: Default + Push<Switch<O, T, Y, S>>,
        Y: Push<usize> + Clone,
        S: Default + Clone,
        Regs: RegisterManager,
    > SleighBoot<'a, 'b, O, T, Y, S, Regs>
{
    pub fn new(
        x: &BTreeMap<u64, &'a Instruction>,
        regs: &'b Regs,
        new: &mut Func<O, T, Y, S>,
    ) -> Self {
        let selector = new.blocks.alloc(Default::default());
        let args: Vec<_> = regs
            .get_registers()
            .into_iter()
            .map(|a| {
                new.add_blockparam(selector, Y::push(a.0.size).map_right(|_| ()).unwrap_left())
            })
            .map(|a| Use {
                value: a,
                select: S::default(),
            })
            .collect();
        let mut n = BTreeMap::new();
        let mut o = BTreeMap::new();
        for (a, b) in x.iter() {
            let k = new.blocks.alloc(Default::default());
            let a2 = regs
                .get_registers()
                .into_iter()
                .map(|a| new.add_blockparam(k, Y::push(a.0.size).map_right(|_| ()).unwrap_left()))
                .collect();
            n.insert(*a, (*b, k, a2));
            o.insert(
                *a,
                BlockTarget {
                    block: k,
                    args: args.clone(),
                    prepend: vec![],
                },
            );
        }
        let st = Use {
            value: new.add_blockparam(selector, Y::push(8usize).map_right(|_| ()).unwrap_left()),
            select: S::default(),
        };
        new.blocks[selector].term = T::push(Switch {
            scutinee: st,
            targets: o,
            default: None,
        })
        .map_right(|_| ())
        .unwrap_left();
        Self {
            insts: n,
            selector,
            regs,
        }
    }
}
