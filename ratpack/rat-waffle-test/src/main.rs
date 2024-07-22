use anyhow::Context;

fn main() -> anyhow::Result<()>{
    let mut a = std::env::args();
    a.next();
    let m = a.next().context("in getting the input")?;
    let m = std::fs::read(m)?;
    let mut m = waffle::Module::from_wasm_bytes(&m, &Default::default())?;
    m.expand_all_funcs()?;
    let mut m = m.without_orig_bytes();
    rat_waffle::test_mod(&mut m,&mut |_,_,a|Ok(a),&mut ())?;
    let o = a.next().context("in getting the output")?;
    std::fs::write(o, m.to_wasm_bytes()?)?;
    return Ok(());
}
