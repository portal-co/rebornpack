fn main() -> std::io::Result<()> {
    // prost_build::compile_protos(&["src/runtime.proto"], &["src/"])?;
    if std::env::var("RW_RT").is_err() {
        println!("cargo:rustc-env=RW_RT=lib.rs");
    };
    Ok(())
}
