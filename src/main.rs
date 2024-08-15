mod ast;
mod parser;
mod printer;

pub use ast::*;
pub use parser::complete_parse_typing_request;

#[derive(Default)]
pub struct Arenas<'a> {
    str_arena: typed_arena::Arena<u8>,
    pat_arena: typed_arena::Arena<Pattern<'a>>,
    type_arena: typed_arena::Arena<Type<'a>>,
}

fn main() {
    println!("Hello, world!");
}
