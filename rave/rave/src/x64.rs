use std::collections::BTreeMap;

use iced_x86::{Decoder, DecoderOptions, Instruction};

pub struct Interm {
    pub map: BTreeMap<u64, Info>,
}
pub struct Info {
    pub instr: Instruction,
    pub next: u64,
}
impl Interm {
    pub fn decode(bitness: u32, a: &[u8], base: u64) -> Self {
        let mut map = BTreeMap::new();
        let mut d = Decoder::with_ip(bitness, a, base, DecoderOptions::NONE);
        for (i, _) in a.iter().enumerate() {
            d.set_position(i);
            let p = d.ip();
            let i = d.decode();
            map.insert(
                p,
                Info {
                    instr: i,
                    next: d.ip(),
                },
            );
        }
        Self { map }
    }
}
