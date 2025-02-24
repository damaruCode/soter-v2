pub mod db;
use elp_base_db::FileId;
use elp_base_db::SourceDatabaseExt;

fn main() {
    let file = std::fs::read_to_string("erlang-code/fib.erl").expect("File should be readable");
    let mut db = db::Db::default();

    let f_id = FileId::from_raw(0);
    db.set_file_text(f_id, file.into());

    let sema = hir::Semantic::new(&db);
    let form_list = sema.form_list(f_id);
    dbg!(form_list);
}
