use crate::{
    abstraction::Abstraction,
    ast::{Call, TypedCore},
    state_space::{
        Env, FailureType, KontinuationAddress, Mailboxes, Pid, ProcState, Store, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::{abs_self, abs_send, abs_spawn, TransitionResult};

pub fn abs_call<K: KontinuationAddress, V: ValueAddress>(
    call: &Call,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    module_env: &Env<V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    // check module name
    let mut result = TransitionResult::new();

    match &*call.module {
        TypedCore::Literal(l_mod) => match &*l_mod.val {
            TypedCore::String(s_mod) => {
                if s_mod.inner != "erlang" {
                    // TODO implement other modules as well
                    result.new.push((
                        proc_state.fail(FailureType::NotImplemented(format!(
                            "Modules other than \"erlang\" are not supported yet, found \"{}\"",
                            s_mod
                        ))),
                        "abs_call".to_string(),
                    ));

                    return result;
                }
            }
            tc => {
                result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Expected literal string, found literal {}",
                        tc
                    ))),
                    "abs_call".to_string(),
                ));
                return result;
            }
        },
        tc => {
            result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected literal, found {}",
                    tc
                ))),
                "abs_call".to_string(),
            ));
            return result;
        }
    };

    // NOTE only operations from the erlang module
    match &*call.name {
        TypedCore::Literal(l) => match &*l.val {
            TypedCore::String(s) => match s.inner.as_str() {
                "spawn" => abs_spawn(
                    &call.args.inner[0],
                    proc_state,
                    mailboxes,
                    store,
                    module_env,
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
                "error" => TransitionResult::new(), // NOTE no-op for now
                name => {
                    result.new.push((
                        proc_state.fail(FailureType::NotImplemented(format!(
                            "Unknown function \"{}\"",
                            name
                        ))),
                        "abs_call".to_string(),
                    ));
                    return result;
                }
            },
            tc => {
                result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Expected literal string, found literal {}",
                        tc
                    ))),
                    "abs_call".to_string(),
                ));
                return result;
            }
        },
        tc => {
            result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected literal, found {}",
                    tc
                ))),
                "abs_call".to_string(),
            ));
            return result;
        }
    }
}
