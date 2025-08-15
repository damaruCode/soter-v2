use crate::{
    ast::{Call, TypedCore},
    state_space::r#abstract::{
        AddressBuilder, KontinuationAddress, Mailboxes, Pid, ProcState, Store, ValueAddress,
        VarName,
    },
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
    address_builder: &Box<dyn AddressBuilder<K, V>>,
) -> TransitionResult<K, V> {
    match &*call.name {
        TypedCore::Literal(l) => match &*l.val {
            TypedCore::String(s) => match s.inner.as_str() {
                "spawn" => abs_spawn(
                    &VarName::from(&call.args.inner[0]),
                    proc_state,
                    mailboxes,
                    store,
                    ast_helper,
                    address_builder,
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
                _ => panic!(),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }
    // NOTE look up module field in call
}
