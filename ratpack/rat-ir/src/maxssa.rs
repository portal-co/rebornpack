use std::collections::{BTreeSet, HashMap};

use id_arena::Id;

use crate::{util::PerID, Block, Func, SaneTerminator, Use, Value};
pub trait MaxSSA {
    fn maxssa(&mut self);
}
impl<O: Clone, T: SaneTerminator<O, T, Y, S> + Clone, Y: Clone, S: Default + Clone + Eq + Ord>
    MaxSSA for Func<O, T, Y, S>
{
    fn maxssa(&mut self) {
        maxssa(self);
    }
}
pub fn maxssa<
    O: Clone,
    T: SaneTerminator<O, T, Y, S> + Clone,
    Y: Clone,
    S: Default + Clone + Eq + Ord,
>(
    f: &mut Func<O, T, Y, S>,
) {
    MaxSSAPass::new().run(f);
}
struct MaxSSAPass<O, T, Y, S> {
    /// Additional block args that must be passed to each block, in
    /// order. Value numbers are *original* values.
    new_args: PerID<Block<O, T, Y, S>, Vec<Use<O, T, Y, S>>>,
    /// For each block, a value map: from original value to local copy
    /// of value.
    value_map: HashMap<(Id<Block<O, T, Y, S>>, Id<Value<O, T, Y, S>>), Id<Value<O, T, Y, S>>>,
}

impl<O: Clone, T: SaneTerminator<O, T, Y, S> + Clone, Y: Clone, S: Default + Clone + Eq + Ord>
    MaxSSAPass<O, T, Y, S>
{
    fn new() -> Self {
        Self {
            new_args: PerID::default(),
            value_map: HashMap::new(),
        }
    }

    fn run(mut self, body: &mut Func<O, T, Y, S>) {
        for block in body.blocks.iter().map(|a| a.0).collect::<Vec<_>>() {
            self.visit(body, block);
        }
        // eprintln!("{:?}",self.new_args.data.iter().enumerate().map(|(a,b)|(a,b.iter().map(|a|a.value.index()).collect::<Vec<_>>())).collect::<Vec<_>>());
        self.update(body);
    }

    fn visit(&mut self, body: &mut Func<O, T, Y, S>, block: Id<Block<O, T, Y, S>>) {
        // For each use in the block, process the use. Collect all
        // uses first to deduplicate and allow more efficient
        // processing (and to appease the borrow checker).
        let mut uses = BTreeSet::default();
        for &inst in &body.blocks[block].insts {
            match &body.opts[inst] {
                crate::Value::Operator(_, b, _, _) => {
                    for c in b {
                        uses.insert(c.clone());
                    }
                }
                crate::Value::BlockParam(_, _, _) => {}
                crate::Value::Alias(u, _) => {
                    uses.insert(u.clone());
                }
            }
        }
        for u in body.blocks[block].term.iter().flat_map(|x|x.uses()) {
            uses.insert(u.clone());
        }

        for u in uses {
            self.visit_use(body, block, u);
        }
    }

    fn visit_use(
        &mut self,
        body: &mut Func<O, T, Y, S>,
        block: Id<Block<O, T, Y, S>>,
        value: Use<O, T, Y, S>,
    ) {
        if self.value_map.contains_key(&(block, value.value)) {
            return;
        }
        // if body.blocks[block].insts.binary_search_by(|a|a.index().cmp(&value.value.index())).is_ok() {
        //     eprintln!("in block value: {}@{}",value.value.index(),block.index());
        //     return;
        // }
        for i in body.blocks[block].insts.iter() {
            if *i == value.value {
                return;
            }
        }
        // eprintln!("{:?}",body.blocks[block].insts.iter().map(|a|a.index()).collect::<Vec<_>>());
        self.new_args[block].push(value.clone());

        // Create a placeholder value.
        let ty = body.opts[value.value].ty().clone();
        let blockparam = body.add_blockparam(block, ty);
        self.value_map.insert((block, value.value), blockparam);

        // Recursively visit preds and use the value there, to ensure
        // they have the value available as well.
        for pred in body.preds(block) {
            // Don't borrow for whole loop while iterating (`body` is
            // taken as mut by recursion, but we don't add preds).
            self.visit_use(body, pred, value.clone());
        }
    }

    fn update_branch_args(&mut self, body: &mut Func<O, T, Y, S>) {
        for (block, blockdata) in body.blocks.iter_mut() {
            if let Some(term) = blockdata.term.as_mut(){
            for target in term.t2s_mut() {
                for new_arg in &self.new_args[target.block] {
                    let actual_value = self
                        .value_map
                        .get(&(block, new_arg.value))
                        .copied()
                        .unwrap_or(new_arg.value);
                    target.args.push(Use {
                        value: actual_value,
                        select: new_arg.select.clone(),
                    });
                }
            }
        }
        }
    }

    fn update_uses(&mut self, body: &mut Func<O, T, Y, S>, block: Id<Block<O, T, Y, S>>) {
        let resolve = |body: &Func<O, T, Y, S>, value: Use<O, T, Y, S>| {
            // let value = body.resolve_alias(value);
            let v = self
                .value_map
                .get(&(block, value.value))
                .copied()
                .unwrap_or(value.value);
            Use {
                value: v,
                select: value.select,
            }
        };

        for i in 0..body.blocks[block].insts.len() {
            let inst = body.blocks[block].insts[i];
            let mut def = body.opts[inst].clone();
            match &mut def {
                Value::Operator(_, u, _, _) => {
                    for a in u.iter_mut() {
                        *a = resolve(body, a.clone());
                    }
                }
                Value::BlockParam(_, _, _) => {}
                Value::Alias(a, _) => {
                    *a = resolve(body, a.clone());
                }
            }
            body.opts[inst] = def;
        }
        let mut term = body.blocks[block].term.clone();
        for a in term.iter_mut().flat_map(|x|x.uses_mut()) {
            *a = resolve(body, a.clone());
        }
        body.blocks[block].term = term;
    }

    fn update(&mut self, body: &mut Func<O, T, Y, S>) {
        self.update_branch_args(body);
        for block in body.blocks.iter().map(|a| a.0).collect::<Vec<_>>() {
            self.update_uses(body, block);
        }
    }
}

fn iter_all_same<Item: PartialEq + Eq + Clone, I: Iterator<Item = Item>>(iter: I) -> Option<Item> {
    let mut item = None;
    for val in iter {
        if item.get_or_insert(val.clone()).clone() != val {
            return None;
        }
    }
    item
}
