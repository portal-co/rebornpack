use sha3::Sha3_256;
use sha3::Digest;
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Span {
    pub file: String,
    pub offset: usize,
    pub size: usize,
    pub hash: [u8; 32],
}
impl Span{
    pub fn hashed(file: String, offset: usize, size: usize) -> std::io::Result<Self>{
        let hash = std::fs::read(&file)?;
        let mut h = Sha3_256::new();
        h.update(&hash);
        Ok(Self { file, offset, size, hash: h.finalize().into() })
    }
}