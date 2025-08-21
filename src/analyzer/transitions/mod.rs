/// K: KontinuationAddress, V: ValueAddress
pub type TransitionResult<K, V> = (
    Vec<(ProcState<K, V>, String)>,
    Vec<(ProcState<K, V>, String)>,
);

mod abs_apply;
mod abs_call;
mod abs_case;
mod abs_module;
mod abs_pop_let_closure;
mod abs_pop_let_pid;
mod abs_pop_seq;
mod abs_push_let;
mod abs_push_seq;
mod abs_receive;
mod abs_self;
mod abs_send;
mod abs_spawn;
mod abs_var;

pub use abs_apply::*;
pub use abs_call::*;
pub use abs_case::*;
pub use abs_module::*;
pub use abs_pop_let_closure::*;
pub use abs_pop_let_pid::*;
pub use abs_pop_seq::*;
pub use abs_push_let::*;
pub use abs_push_seq::*;
pub use abs_receive::*;
pub use abs_self::*;
pub use abs_send::*;
pub use abs_spawn::*;
pub use abs_var::*;

use crate::state_space::ProcState;
