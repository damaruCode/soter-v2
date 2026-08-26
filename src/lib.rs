pub mod abstraction;
pub mod analyzer;
pub mod ast;
pub mod erlang;
pub mod state_space;
pub mod util;

pub fn test_ast_builder() {
    let mut clause = ast::Clause::new();
    clause.body = Box::new(ast::TypedCore::Bool(ast::ErlBool::new(false)));
}
