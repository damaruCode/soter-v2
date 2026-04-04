/// K: KontinuationAddress, V: ValueAddress
pub type TransitionPair<K, V> = (ProcState<K, V>, String);

pub struct TransitionResult<K: KontinuationAddress, V: ValueAddress> {
    pub new: Vec<TransitionPair<K, V>>,
    pub revisit: Vec<TransitionPair<K, V>>,
}
impl<K: KontinuationAddress, V: ValueAddress> TransitionResult<K, V> {
    pub fn new() -> Self {
        Self {
            new: Vec::new(),
            revisit: Vec::new(),
        }
    }

    pub fn append(&mut self, other: &mut Self) {
        self.new.append(&mut other.new);
        self.revisit.append(&mut other.revisit);
    }
}

mod abs_apply;
mod abs_call;
mod abs_case;
mod abs_module;
mod abs_pid;
mod abs_pop_let_closure;
mod abs_pop_let_pid;
mod abs_pop_let_valuelist;
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
pub use abs_pid::*;
pub use abs_pop_let_closure::*;
pub use abs_pop_let_pid::*;
pub use abs_pop_let_valuelist::*;
pub use abs_pop_seq::*;
pub use abs_push_let::*;
pub use abs_push_seq::*;
pub use abs_receive::*;
pub use abs_self::*;
pub use abs_send::*;
pub use abs_spawn::*;
pub use abs_var::*;

use crate::state_space::{KontinuationAddress, ProcState, ValueAddress};
