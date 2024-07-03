cd $(dirname $0)
wat2wasm ./test.wat
RUST_BACKTRACE=1 cargo run -- ./test.wasm ./test.result.wasm
wasm2wat ./test.result.wasm