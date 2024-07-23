use serde::Deserialize;
use serde::Serialize;
use sha3::Digest;
use sha3::Sha3_256;
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub offset: usize,
    pub size: usize,
    pub hash: [u8; 32],
}
impl Span {
    pub fn hashed(file: String, offset: usize, size: usize) -> std::io::Result<Self> {
        let hash = std::fs::read(&file)?;
        let mut h = Sha3_256::new();
        h.update(&hash);
        Ok(Self {
            file,
            offset,
            size,
            hash: h.finalize().into(),
        })
    }
}
