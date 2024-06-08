use std::collections::{BTreeMap, BTreeSet};

use id_arena::Id;

use crate::Block;

pub fn calculate_postorder<
    O,
    T,
    Y,
    S,
    SuccFn: FnMut(Id<Block<O, T, Y, S>>) -> Vec<Id<Block<O, T, Y, S>>>,
>(
    entry: Id<Block<O, T, Y, S>>,
    mut succ_blocks: SuccFn,
) -> Vec<Id<Block<O, T, Y, S>>> {
    let mut ret = vec![];

    // State: visited-block map, and explicit DFS stack.
    let mut visited: BTreeSet<Id<Block<O, T, Y, S>>> = BTreeSet::new();

    #[derive(Debug)]
    struct State<O, T, Y, S> {
        block: Id<Block<O, T, Y, S>>,
        succs: Vec<Id<Block<O, T, Y, S>>>,
        next_succ: usize,
    }
    let mut stack: Vec<State<O, T, Y, S>> = vec![];

    visited.insert(entry);
    stack.push(State {
        block: entry,
        succs: succ_blocks(entry),
        next_succ: 0,
    });

    while let Some(ref mut state) = stack.last_mut() {
        // log::trace!("postorder: TOS is {:?}", state);
        // Perform one action: push to new succ, skip an already-visited succ, or pop.
        if state.next_succ < state.succs.len() {
            let succ = state.succs[state.next_succ];
            // log::trace!(" -> succ {}", succ);
            state.next_succ += 1;
            if !visited.contains(&succ) {
                // log::trace!(" -> visiting");
                visited.insert(succ);
                stack.push(State {
                    block: succ,
                    succs: succ_blocks(succ),
                    next_succ: 0,
                });
            }
        } else {
            // log::trace!("retreating from {}", state.block);
            ret.push(state.block);
            stack.pop();
        }
    }

    ret
}
