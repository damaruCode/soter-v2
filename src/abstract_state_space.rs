#![allow(warnings)]

use crate::ast;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone)]
pub struct State<'a> {
    pub procs: Procs<'a>,
    pub mailboxes: Mailboxes<'a>,
    pub value_store: ValueStore<'a>,
    pub continuation_store: ContinuationStore<'a>,
}
impl<'a> State<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        State {
            procs: Procs::init(prog_loc),
            mailboxes: Mailboxes::init(),
            value_store: ValueStore::init(),
            continuation_store: ContinuationStore::init(),
        }
    }

    fn step(self) -> Self {
        // TODO
        let old_state = self.clone();

        for (pid, proc_states) in old_state.procs.inner {
            for (proc_state) in proc_states {
                // call step on each proc_state
            }
        }

        return self;
    }
}

#[derive(Clone)]
pub struct Procs<'a> {
    pub inner: HashMap<Pid<'a>, HashSet<ProcState<'a>>>,
}
impl<'a> Procs<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        Procs {
            inner: HashMap::from([(Pid::init(prog_loc.clone()), HashSet::from([ProcState::init(prog_loc.clone())]))]),
        }
    }
}
#[derive(Clone)]
pub struct Mailboxes<'a> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a>>,
}
impl<'a> Mailboxes<'a> {
    fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
#[derive(Clone)]
pub struct ValueStore<'a> {
    pub inner: HashMap<VAddr<'a>, HashSet<Value<'a>>>,
}
impl<'a> ValueStore<'a> {
    fn init() -> Self {
        ValueStore {
            inner: HashMap::new(),
        }
    }
}
#[derive(Clone)]
pub struct ContinuationStore<'a> {
    pub inner: HashMap<KAddr<'a>, HashSet<Kont<'a>>>,
}
impl ContinuationStore<'_> {
    fn init() -> Self {
        ContinuationStore {
            inner: HashMap::new(),
        }
    }
}

// Pid := ProgLoc x Time
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Pid<'a> {
    prog_loc: ProgLoc<'a>,
    time: Time<'a>,
}
impl<'a> Pid<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        Pid {
            prog_loc: prog_loc,
            time: Time::init(),
        }
    }

    fn new(self, prog_loc: ProgLoc<'a>, time: Time<'a>) -> Self {
        let mut pid_time = self.time.tick(self.prog_loc);
        let mut time_inner_clone = time.inner.clone();

        time_inner_clone.append(&mut pid_time.inner);

        let new_time = Time {
            inner: time_inner_clone,
        };

        return Pid {
            prog_loc: prog_loc,
            time: new_time,
        };
    } // TODO Review with David
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum ProgLocOrPid<'a> {
    ProgLoc(ProgLoc<'a>),
    Pid(Pid<'a>),
}
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ProcState<'a> {
    prog_loc_or_pid: ProgLocOrPid<'a>,
    env: Env<'a>,
    k_addr: KAddr<'a>,
    time: Time<'a>,
}
impl<'a> ProcState<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        ProcState {
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc.clone()),
            env: Env::init(),
            k_addr: KAddr::init(prog_loc.clone()),
            time: Time::init(),
        }
    }

    fn step(self) -> Self {
        // TODO
        let old_state = self.clone();

        match self.prog_loc_or_pid {
            ProgLocOrPid::Pid(ref pid) => {} // Considered a value
            ProgLocOrPid::ProgLoc(ref prog_loc) => {
                match prog_loc.inner {
                    ast::TypedCore::Apply(exp) => {}   // FunEval
                    ast::TypedCore::Var(var) => {}     // Vars
                    ast::TypedCore::Receive(exp) => {} // Receive
                    _ => {}                            // most likely a value
                                                        // there should probably be a unified
                                                        // `handle_value` function under which all
                                                        // transition rules on values are executed
                                                        // if applicable
                }
            }
        }

        return self;
    }
}

// Mailbox := P(Value)
#[derive(Clone)]
pub struct Mailbox<'a> {
    inner: HashSet<Value<'a>>,
}
impl Mailbox<'_> {
    fn mmatch(self) {} // TODO
    fn enq(self) {} // TODO
}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var: Var<'a>,
    data: Data<'a>,
    time: Time<'a>,
}
impl<'a> VAddr<'a> {
    fn new(pid: Pid<'a>, var: Var<'a>, data: Data<'a>, time: Time<'a>) -> Self {
        return VAddr {
            pid: pid.clone(),
            var: var.clone(),
            data: data.clone(),
            time: time.clone(),
        };
    } // TODO Review with David
}

// Value := Closure U+ Pid
#[derive(Clone)]
pub enum ClosureOrPid<'a> {
    Closure(Closure<'a>),
    Pid(Pid<'a>),
}
#[derive(Clone)]
pub struct Value<'a> {
    inner: ClosureOrPid<'a>,
}

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct KAddr<'a> {
    pid: Pid<'a>,
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
    time: Time<'a>,
}
impl<'a> KAddr<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        KAddr {
            pid: Pid::init(prog_loc.clone()),
            prog_loc: prog_loc,
            env: Env::init(),
            time: Time::init(),
        }
    }

    fn new_push(pid: Pid<'a>, prog_loc: ProgLoc<'a>, env: Env<'a>, time: Time<'a>) -> Self {
        return KAddr {
            pid: pid.clone(),
            prog_loc: prog_loc.clone(),
            env: env.clone(),
            time: time.clone(),
        };
    } // TODO Review with David
    fn new_pop(pid: Pid<'a>, kont: Kont<'a>, time: Time<'a>) -> Self {
        let next_prog_loc = // l_{i + 1} => derive from TypedCore::Call, how to look at the next
                               // argument of a function call

        return KAddr { pid: pid.clone(), prog_loc: next_prog_loc, env: kont.env.clone(), time: time.clone() }
    } // TODO Review with David
}

// Kont := index x ProgLoc x Data* x Env x KAddr
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone)]
pub struct Kont<'a> {
    index: usize,
    prog_loc: ProgLoc<'a>,
    vec_data: Vec<Data<'a>>,
    env: Env<'a>,
    k_addr: KAddr<'a>,
}

// Time := ProgLoc^k
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Time<'a> {
    inner: Vec<ProgLoc<'a>>,
}
impl<'a> Time<'a> {
    fn init() -> Self {
        Time { inner: Vec::new() }
    }

    fn tick(self, prog_loc: ProgLoc<'a>) -> Self {
        let mut new_time = self.clone();
        new_time.inner.push(prog_loc);

        return new_time;
    } // TODO Review with David
}

// Closure := ProgLoc x Env
#[derive(Clone)]
pub struct Closure<'a> {
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
}

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Env<'a> {
    inner: BTreeMap<Var<'a>, VAddr<'a>>, // TODO Hash or BTree ???
}
impl Env<'_> {
    fn init() -> Self {
        Env {
            inner: BTreeMap::new(), // TODO
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Var<'a> {
    inner: &'a ast::Var,
}

// NOTE this references the Exps in the Ast
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ProgLoc<'a> {
    inner: &'a ast::TypedCore,
}

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Data<'a> {
    inner: &'a ast::TypedCore, // TODO
}
