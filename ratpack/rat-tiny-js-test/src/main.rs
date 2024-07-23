use std::convert::Infallible;
use std::env::args;
use std::io::stderr;
use std::path::Path;

use anyhow::Context;
use rat_ir::{BoundOp, BoundSelect, BoundTerm, BoundType};
use swc_common::errors::EmitterWriter;
use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceMap,
};
use swc_core::ecma::ast::EsVersion;
use swc_ecma_parser::EsSyntax;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

fn main() -> anyhow::Result<()>{
    let mut a = args();
    a.next();
    let input = a.next().context("in getting the input file")?;
    let cm: Lrc<SourceMap> = Default::default();
    let handler =
        Handler::with_emitter(true, false,
        Box::new(EmitterWriter::new(Box::new(stderr()), Some(cm.clone()), false, true)));

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    let fm = cm.load_file(Path::new(input.as_str()))?;
    let mut v = vec![];
    let module = swc_ecma_parser::parse_file_as_module(&fm, Syntax::Es(EsSyntax::default()), EsVersion::latest(), None, &mut v).map_err(|e|anyhow::anyhow!("{e:?}"))?;
    let mut r: rat_ir::module::Module<BoundOp<rat_tiny_js::fe::basic::Basic>,BoundTerm<rat_tiny_js::fe::basic::Basic>,BoundType<rat_tiny_js::fe::basic::Basic>,BoundSelect<rat_tiny_js::fe::basic::Basic>,Infallible> = Default::default();
    let mut spec = rat_tiny_js::fe::funcs(&module, &mut r)?;
    Ok(())
}


