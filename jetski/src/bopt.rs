use id_arena::Id;

use crate::{
    detect::{Decr, Detect},
    Block, PMMNCmd, PMMNCmds, PMMNTest, Scope,
};

pub fn incr_decr(s: &mut Scope, kb: Id<Block>) -> bool {
    let mut did = false;
    if let Block::Br(mut i, mut j) = s.blocks[kb].clone() {
        if let Block::BrDec(x, k, e) = s.blocks[j].clone() {
            let mut f = false;
            if let Some(a) = i.get_mut(&x) {
                *a -= 1u32;
                f = true;
            }
            if f {
                j = k;
                s.blocks[kb] = Block::Br(i, j);
                did = true;
            }
        }
    }
    return did;
}
