use id_arena::Id;
use marl::Marl;
use rat_ir::Value;
pub enum Instr<O>{
    Op(O),
    Dig(usize),
    None
}
impl<O> Default for Instr<O>{
    fn default() -> Self {
        Self::None
    }
}
pub struct Manager<O,T,Y,S>{
    pub stack: Vec<Id<Value<O,T,Y,S>>>,
    pub marl: Marl<Instr<O>,usize,Box<dyn FnMut(usize)->usize>>
}
impl<O,T,Y,S> Manager<O,T,Y,S>{
    pub fn dig(&mut self, v: Id<Value<O,T,Y,S>>){
        let mut j = self.stack.len();
        while self.stack[j] != v{
            j -= 1
        }
        self.marl.emit(Instr::Dig(self.stack.len() - j));
        let w = self.stack.remove(j);
        self.stack.push(w);
        
    }
}