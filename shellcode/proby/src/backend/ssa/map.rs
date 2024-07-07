use std::mem::take;

use super::*;
pub trait Mapper{
    fn emit(
        &mut self,
        instr: &Instr,
        mapper: &mut BTreeMap<ValueId,ValueId>,
        k: Id<Block>,
        f: &mut SSAFunc,
    ) -> anyhow::Result<(ValueId,Id<Block>)>;
    fn terminate(
        &mut self,
        instr: &Term,
        mapper: &mut BTreeMap<ValueId,ValueId>,
        k: Id<Block>,
        f: &mut SSAFunc,
    ) -> anyhow::Result<()>;
}
pub fn map(s: &mut SSAFunc, m: &mut impl Mapper) -> anyhow::Result<()>{
    for k in s.blocks.iter().map(|a|a.0).collect::<Vec<_>>(){
        map_block(s, m, k)?;
    }
    return Ok(());
}
pub fn map_block(s: &mut SSAFunc, m: &mut impl Mapper, mut k: Id<Block>) -> anyhow::Result<()>{
    let mut n = BTreeMap::new();
    let kb = take(&mut s.blocks[k]);
    for (ky,v) in kb.insts.into_iter(){
        let (w,l) = m.emit(&v, &mut n, k,s)?;
        k = l;
        n.insert(ky, w);
    }
    m.terminate(&kb.term, &mut n, k, s)?;
    return Ok(());
}