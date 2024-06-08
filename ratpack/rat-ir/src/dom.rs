/*
 * Derives from the dominator tree implementation in regalloc.rs, which is
 * licensed under the Apache Public License 2.0 with LLVM Exception. See:
 * https://github.com/bytecodealliance/regalloc.rs
 */

// This is an implementation of the algorithm described in
//
//   A Simple, Fast Dominance Algorithm
//   Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
//   Department of Computer Science, Rice University, Houston, Texas, USA
//   TR-06-33870
//   https://www.cs.rice.edu/~keith/EMBED/dom.pdf

use std::collections::{BTreeMap, BTreeSet};

use id_arena::Id;

use crate::{Func, SaneTerminator, Value};

type Block<O, T, Y, S> = Option<JustBlock<O, T, Y, S>>;
type JustBlock<O, T, Y, S> = Id<crate::Block<O, T, Y, S>>;
pub type DoMap<O, T, Y, S> = BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>>;
// Helper
fn merge_sets<O, T, Y, S>(
    idom: &BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>>, // map from Block to Block
    block_to_rpo: &BTreeMap<Block<O, T, Y, S>, u32>,
    mut node1: Block<O, T, Y, S>,
    mut node2: Block<O, T, Y, S>,
) -> Block<O, T, Y, S> {
    while node1 != node2 {
        if node1.is_none() || node2.is_none() {
            return None;
        }
        let rpo1 = block_to_rpo.get(&node1).copied();
        let rpo2 = block_to_rpo.get(&node2).copied();
        if rpo1 > rpo2 {
            node1 = Some(*idom.get(&node1).unwrap());
        } else if rpo2 > rpo1 {
            node2 = Some(*idom.get(&node2).unwrap());
        }
    }
    assert!(node1 == node2);
    node1
}

pub fn calculate<O, T, Y, S, PredFn: FnMut(Block<O, T, Y, S>) -> BTreeSet<Block<O, T, Y, S>>>(
    mut preds: PredFn,
    post_ord: &[Block<O, T, Y, S>],
    start: Block<O, T, Y, S>,
) -> BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>> {
    // We have post_ord, which is the postorder sequence.

    // Compute maps from RPO to block number and vice-versa.
    let mut block_to_rpo: BTreeMap<Block<O, T, Y, S>, u32> = BTreeMap::default();
    for (i, rpo_block) in post_ord.iter().rev().enumerate() {
        block_to_rpo.insert(rpo_block.clone(), i as u32);
    }

    let mut idom: BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>> = BTreeMap::default();

    // The start node must have itself as a parent.
    idom.insert(start, start.unwrap());

    let mut changed = true;
    while changed {
        changed = false;
        // Consider blocks in reverse postorder. Skip any that are unreachable.
        for &node in post_ord.iter().rev() {
            let rponum = *block_to_rpo.get(&node).unwrap();

            let mut parent = None;
            for &pred in preds(node).iter() {
                let pred_rpo = match block_to_rpo.get(&pred).copied() {
                    Some(r) => r,
                    None => {
                        // Skip unreachable preds.
                        continue;
                    }
                };
                if pred_rpo < rponum {
                    parent = pred;
                    break;
                }
            }

            if parent.is_some() {
                for &pred in preds(node).iter() {
                    if pred == parent {
                        continue;
                    }
                    if idom.get(&pred).copied().is_none() {
                        continue;
                    }
                    parent = merge_sets(&idom, &block_to_rpo, parent, pred);
                }
            }

            if parent != idom.get(&node).copied() {
                if let Some(parent) = parent {
                    idom.insert(node, parent);
                }
                changed = true;
            }
        }
    }

    // Now set the start node's dominator-tree parent to "invalid";
    // this allows the loop in `dominates` to terminate.
    idom.remove(&start);

    idom
}

pub fn dominates<O, T, Y, S>(
    idom: &BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>>,
    a: Block<O, T, Y, S>,
    mut b: Block<O, T, Y, S>,
) -> bool {
    loop {
        if a == b {
            return true;
        }
        if b.is_none() {
            return false;
        }
        b = idom.get(&b).copied();
    }
}
impl<O, T: SaneTerminator<O, T, Y, S>, Y, S> Func<O, T, Y, S> {
    pub fn values_in<'a>(
        &'a mut self,
        x: JustBlock<O, T, Y, S>,
    ) -> impl Iterator<Item = Id<Value<O, T, Y, S>>> + 'a{
        let m = self.domap();
        return self
            .blocks
            .iter()
            .filter(move |a| dominates(&m, Some(a.0), Some(x)))
            .flat_map(|a| a.1.insts.iter())
            .map(|a| a.clone());
    }
    pub fn values_in_ref<'a>(
        &'a self,
        x: JustBlock<O, T, Y, S>,
    ) -> impl Iterator<Item = Id<Value<O, T, Y, S>>> + 'a{
        let m = self.domap_ref();
        return self
            .blocks
            .iter()
            .filter(move |a| dominates(&m, Some(a.0), Some(x)))
            .flat_map(|a| a.1.insts.iter())
            .map(|a| a.clone());
    }
    pub fn domap(&mut self) -> BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>> {
        let r = crate::cfg::calculate_postorder(self.entry, |x| {
            self.blocks[x].term.targets().into_iter().collect()
        })
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();
        let e = self.entry.clone();
        return calculate(
            |x| {
                x.map(|y| self.preds(y).into_iter().map(Some).collect())
                    .unwrap_or_else(|| Default::default())
            },
            &r,
            Some(e),
        );
    }
    pub fn domap_ref(&self) -> BTreeMap<Block<O, T, Y, S>, JustBlock<O, T, Y, S>> {
        let r = crate::cfg::calculate_postorder(self.entry, |x| {
            self.blocks[x].term.targets().into_iter().collect()
        })
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();
        let e = self.entry.clone();
        return calculate(
            |x| {
                x.map(|y| self.preds_ref(y).into_iter().map(Some).collect())
                    .unwrap_or_else(|| Default::default())
            },
            &r,
            Some(e),
        );
    }
}
