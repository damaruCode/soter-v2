use crate::{
    abstraction::Abstraction,
    ast::{Index, TypedCore},
    state_space::{
        KontinuationAddress, Mailbox, Mailboxes, Pid, ProcState, ProgLocOrPid, Store, Time, Value,
        ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_spawn<K: KontinuationAddress, V: ValueAddress>(
    var_name: &VarName,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    ast_helper: &AstHelper,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let values = store
        .value
        .get(proc_state.env.inner.get(&var_name).unwrap())
        .unwrap();

    for value in values {
        match value {
            Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                TypedCore::Fun(f) => {
                    if f.vars.inner.len() != 0 {
                        panic!();
                    }
                    let mut new_time =
                        abstraction.tick(&proc_state.pid.time, proc_state.pid.prog_loc);
                    new_time.append(proc_state.time.inner.clone());
                    let new_pid = Pid {
                        prog_loc: match proc_state.prog_loc_or_pid {
                            ProgLocOrPid::ProgLoc(pl) => pl,
                            ProgLocOrPid::Pid(_) => panic!("Expected a ProgLoc not Pid"),
                        },
                        time: new_time,
                    };

                    let mut new_proc_state_one = proc_state.clone();
                    new_proc_state_one.prog_loc_or_pid = ProgLocOrPid::Pid(new_pid.clone());

                    v_new.push(new_proc_state_one);

                    match &*f.body {
                        TypedCore::Case(c) => match &c.clauses.inner[0] {
                            TypedCore::Clause(c) => {
                                let new_proc_state_two = ProcState::new(
                                    new_pid.clone(),
                                    ProgLocOrPid::ProgLoc((*c.body).get_index().unwrap()),
                                    clo.env.clone(),
                                    abstraction.init_kaddr(),
                                    Time::init(),
                                );
                                v_new.push(new_proc_state_two);

                                mailboxes.inner.insert(new_pid, Mailbox::init());
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    (v_new, Vec::new())
}
