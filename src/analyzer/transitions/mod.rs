/// K: KontinuationAddress, V: ValueAddress
pub type TransitionResult<K, V> = (Vec<ProcState<K, V>>, Vec<ProcState<K, V>>);

pub mod abs_apply;
pub mod abs_call;
pub mod abs_case;
pub mod abs_module;
pub mod abs_pop_let_closure;
pub mod abs_pop_let_pid;
pub mod abs_push_let;
pub mod abs_receive;
pub mod abs_self;
pub mod abs_send;
pub mod abs_spawn;
pub mod abs_var;

pub use abs_apply::*;
pub use abs_call::*;
pub use abs_case::*;
pub use abs_module::*;
pub use abs_pop_let_closure::*;
pub use abs_pop_let_pid::*;
pub use abs_push_let::*;
pub use abs_receive::*;
pub use abs_self::*;
pub use abs_send::*;
pub use abs_spawn::*;
pub use abs_var::*;

use crate::state_space::ProcState;
