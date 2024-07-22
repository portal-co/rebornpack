use std::collections::BTreeMap;

pub struct Marl<T, U, F> {
    pub all: Vec<T>,
    pub maps: BTreeMap<String, U>,
    pub preds: BTreeMap<String, Vec<(usize, Box<dyn FnOnce(&U) -> T>)>>,
    pub base: F,
}
impl<T: Default, U, F: FnMut(usize) -> U> Marl<T, U, F> {
    pub fn mark(&mut self, a: String) {
        let t = (self.base)(self.all.len());
        if let Some(b) = self.preds.remove(&a) {
            for c in b {
                self.all[c.0] = c.1(&t);
            }
        }
        self.maps.insert(a.clone(), t);
    }
    pub fn bind(&mut self, a: String, m: impl FnOnce(&U) -> T + 'static) {
        match self.maps.get(&a) {
            None => {
                let l = self.all.len();
                self.all.push(T::default());
                self.preds.entry(a).or_default().push((l, Box::new(m)));
            }
            Some(b) => {
                self.all.push(m(b));
            }
        }
    }
    pub fn emit(&mut self, t: T) {
        self.all.push(t);
    }
    pub fn new(f: F) -> Self {
        Self {
            all: vec![],
            maps: BTreeMap::new(),
            preds: BTreeMap::new(),
            base: f,
        }
    }
}
