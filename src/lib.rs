pub mod abstraction;
pub mod analyzer;
pub mod ast;
pub mod erlang;
pub mod state_space;
pub mod util;

#[cfg(test)]
mod tests {
    use crate::ast::*;

    #[test]
    pub fn test_ast_builder() {
        let mut case = Case::new();
        {
            case.arg = Box::new(TypedCore::Var(Var::new()));
            let mut clause = Clause::new();
            clause.pats.inner.push(TypedCore::Var(Var::new()));

            case.clauses = AstList::new();
            case.clauses.inner.push(TypedCore::Clause(clause));
        }
        println!("{}", case);
    }
}
