use std::fmt;
use std::panic;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::FileLoaderDelegate;
use elp_base_db::FileRange;
use elp_base_db::SourceDatabase;
use elp_base_db::Upcast;
use elp_types_db::eqwalizer;
use elp_types_db::IncludeGenerated;
use elp_types_db::TypedSemantic;

use hir::db::InternDatabase;

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

impl salsa::Database for Db {}

impl Upcast<dyn SourceDatabase> for Db {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
        self
    }
}

impl Upcast<dyn InternDatabase> for Db {
    fn upcast(&self) -> &(dyn InternDatabase + 'static) {
        self
    }
}

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

impl TypedSemantic for Db {
    fn eqwalizer_diagnostics(
        &self,
        _file_id: FileId,
        _include_generated: IncludeGenerated,
    ) -> Option<Vec<eqwalizer::EqwalizerDiagnostic>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }

    fn eqwalizer_type_at_position(
        &self,
        _range: elp_base_db::FileRange,
    ) -> Option<Arc<(eqwalizer::types::Type, FileRange)>> {
        panic!("Eqwalizer data is not available in HIR tests")
    }
}
