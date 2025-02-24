pub mod db;

fn main() {
    let file = std::fs::read_to_string("erlang-code/fib.erl").expect("File should be readable");

    // erl_syntax
    // let parse = elp_syntax::SourceFile::parse_text(&file);
    // let actual_tree = format!("{:#?}", parse.syntax_node());
    // println!("{}", actual_tree);

    let mut db = db::Db::default();
    db.set_input(file);
    hir::Semantic::new(&db);
}
