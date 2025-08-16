use core::panic;

use crate::{
    abstraction::Abstraction,
    ast::{Call, TypedCore},
    state_space::{KontinuationAddress, Mailboxes, Pid, ProcState, Store, ValueAddress, VarName},
    util::{AstHelper, SetMap},
};

use super::{abs_self, abs_send, abs_spawn, TransitionResult};

pub fn abs_call<K: KontinuationAddress, V: ValueAddress>(
    call: &Call,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    // check module name
    match &*call.module {
        TypedCore::Literal(l_mod) => match &*l_mod.val {
            TypedCore::String(s_mod) => {
                if s_mod.inner != "erlang" {
                    todo!("Can't handle modules other than 'erlang'")
                }
            }
            _ => panic!(),
        },
        _ => panic!(),
    };

    // NOTE only operations from the erlang module
    match &*call.name {
        TypedCore::Literal(l) => match &*l.val {
            TypedCore::String(s) => match s.inner.as_str() {
                "spawn" => abs_spawn(
                    &VarName::from(&call.args.inner[0]),
                    proc_state,
                    mailboxes,
                    store,
                    ast_helper,
                    abstraction,
                ),
                "!" | "send" => abs_send(
                    &call.args.inner[0],
                    &call.args.inner[1],
                    proc_state,
                    mailboxes,
                    store,
                    seen_proc_states,
                    ast_helper,
                ),
                "self" => abs_self(proc_state),
                _ => panic!("{:#?}", s),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }
}
