use crate::{
    abstraction::Abstraction,
    analyzer::transitions::abs_pop_seq,
    state_space::{Kont, KontinuationAddress, Pid, ProcState, Store, ValueAddress},
    util::{AstHelper, SetMap},
};

use super::{abs_pop_let_pid, TransitionResult};

pub fn abs_pid<K: KontinuationAddress, V: ValueAddress>(
    pid: &Pid,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let konts = store.kont.get(&proc_state.k_addr).unwrap().clone();
    for kont in konts {
        match kont {
            Kont::Let(var_list, body, env, k_addr) => {
                return abs_pop_let_pid(
                    pid,
                    &var_list,
                    body,
                    &env,
                    &k_addr,
                    proc_state,
                    store,
                    seen_proc_states,
                    abstraction,
                    ast_helper,
                )
            }
            Kont::Seq(next_prog_loc, env, k_addr) => {
                return abs_pop_seq(proc_state, next_prog_loc, &env, &k_addr)
            }
            Kont::Stop => {} // successful halt
            Kont::Send(..) | Kont::Apply(..) | Kont::Spawn(..) => panic!(),
        };
    }

    return (Vec::new(), Vec::new());
}
