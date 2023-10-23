use std::{env, path::Path};

use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use transform::transform;

mod ir;
mod transform;

fn main() {
    let name = env::args()
        .nth(1)
        .unwrap_or_else(|| "example.js".to_string());
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).unwrap_or_else(|_| panic!("{name} not found"));
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    let ret = Parser::new(&allocator, &source_text, source_type).parse();

    let program = allocator.alloc(ret.program);
    let semantic_ret = SemanticBuilder::new(&source_text, source_type)
        .with_trivias(ret.trivias)
        .build(program);

    if true {
        println!("#lang racket");
    }

    println!("{}", transform(semantic_ret.semantic));
}
