pub trait XCtx {
    fn entry(&self) -> usize;
    fn intern_ty(&mut self, ty: &str) -> usize;
    fn push(&mut self, k: usize, a: &str, rs: &[usize], rts: &[usize]) -> usize;
    fn br(&mut self, k: usize, s: usize);
}
