use std::fmt;

use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::FileLoaderDelegate;
use elp_base_db::SourceDatabaseExt;
use std::panic;
use std::sync::Arc;

#[salsa::database(
    elp_base_db::SourceDatabaseExtStorage,
    elp_base_db::SourceDatabaseStorage,
    hir::db::DefDatabaseStorage,
    hir::db::InternDatabaseStorage
)]
#[derive(Default)]
pub(crate) struct Db {
    storage: salsa::Storage<Self>,
}

impl Db {
    pub fn set_input(&mut self, input: String) {
        let f_id = FileId::from_raw(0);
        self.set_file_text(f_id, input.into());
    }
}

impl salsa::Database for Db {}

impl fmt::Debug for Db {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Db").finish()
    }
}

impl panic::RefUnwindSafe for Db {}

impl elp_base_db::FileLoader for Db {
    fn file_text(&self, file_id: elp_base_db::FileId) -> Arc<str> {
        FileLoaderDelegate(self).file_text(file_id)
    }
}
