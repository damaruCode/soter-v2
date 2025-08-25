use core::panic;

use crate::{
    abstraction::Abstraction,
    ast::TypedCore,
    state_space::{KontinuationAddress, Mailboxes, ProcState, Value, ValueAddress},
    util::AstHelper,
};

use super::{abs_self, abs_spawn, TransitionResult};

pub fn abs_call<K: KontinuationAddress, V: ValueAddress>(
    kont_module: &TypedCore,
    kont_op_value: &Value<V>,
    kont_value_list: &Vec<Value<V>>,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    ast_helper: &AstHelper,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    // check module name
    let kont_module_atom = match kont_module {
        TypedCore::Literal(l) => l.clone(),
        _ => panic!(),
    };

    match &*kont_module_atom.val {
        TypedCore::String(s_mod) => {
            if s_mod.inner != "erlang" {
                todo!("Can't handle modules other than 'erlang'")
            }
        }
        _ => panic!(),
    };

    match &*kont_op_value {
        Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
            TypedCore::Literal(l) => match &*l.val {
                TypedCore::String(s) => match s.inner.as_str() {
                    "spawn" => abs_spawn(
                        &kont_value_list[0],
                        proc_state,
                        mailboxes,
                        ast_helper,
                        abstraction,
                    ),
                    "self" => abs_self(proc_state),
                    _ => todo!("{:#?}", s),
                },
                _ => panic!(),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }
}
