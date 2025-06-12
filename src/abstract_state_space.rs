#![allow(warnings)]

use crate::ast;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))

pub struct State {
    pub procs: Procs,
    pub mailboxes: Mailboxes,
    pub value_store: ValueStore,
    pub continuation_store: ContinuationStore,
}
impl State {
    fn init() -> Self {
        State {
            procs: Procs::init(),
            mailboxes: Mailboxes::init(),
            value_store: ValueStore::init(),
            continuation_store: ContinuationStore::init(),
        }
    }
}
pub struct Procs {
    pub inner: HashMap<Pid, HashSet<ProcState>>,
}
impl Procs {
    fn init() -> Self {
        Procs {
            inner: HashMap::from([(Pid::init(), HashSet::from([ProcState::init()]))]),
        }
    }
}
pub struct Mailboxes {
    pub inner: HashMap<Pid, Mailbox>,
}
impl Mailboxes {
    fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
pub struct ValueStore {
    pub inner: HashMap<VAddr, HashSet<Value>>,
}
impl ValueStore {
    fn init() -> Self {
        ValueStore {
            inner: HashMap::new(),
        }
    }
}
pub struct ContinuationStore {
    pub inner: HashMap<KAddr, HashSet<Kont>>,
}
impl ContinuationStore {
    fn init() -> Self {
        ContinuationStore {
            inner: HashMap::new(),
        }
    }
}

// Pid := ProgLoc x Time
#[derive(Eq, PartialEq, Hash)]
pub struct Pid {
    prog_loc: ProgLoc,
    time: Time,
}
impl Pid {
    fn init() -> Self {
        Pid {
            prog_loc: ProgLoc::init(),
            time: Time::init(),
        }
    }
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Eq, PartialEq, Hash)]
pub enum ProgLocOrPid {
    ProgLoc(ProgLoc),
    Pid(Pid),
}
#[derive(Eq, PartialEq, Hash)]
pub struct ProcState {
    prog_loc_or_pid: ProgLocOrPid,
    env: Env,
    k_addr: KAddr,
    time: Time,
}
impl ProcState {
    fn init() -> Self {
        ProcState {
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(ProgLoc::init()),
            env: Env::init(),
            k_addr: KAddr::init(),
            time: Time::init(),
        }
    }
}

// Mailbox := P(Value)
pub struct Mailbox {
    inner: HashSet<Value>,
}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash)]
pub struct VAddr {
    pid: Pid,
    var: Var,
    data: Data,
    time: Time,
}

// Value := Closure U+ Pid
pub enum ClosureOrPid {
    Closure(Closure),
    Pid(Pid),
}
pub struct Value {
    inner: ClosureOrPid,
}

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash)]
pub struct KAddr {
    pid: Pid,
    prog_loc: ProgLoc,
    env: Env,
    time: Time,
}
impl KAddr {
    fn init() -> Self {
        KAddr {
            pid: Pid::init(),
            prog_loc: ProgLoc::init(),
            env: Env::init(),
            time: Time::init(),
        }
    }
}

// Kont := index x ProgLoc x Data* x Env x KAddr
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
pub struct Kont {
    index: usize,
    prog_loc: ProgLoc,
    vec_data: Vec<Data>,
    env: Env,
    k_addr: KAddr,
}

// Time := ProgLoc^k
#[derive(Eq, PartialEq, Hash)]
pub struct Time {
    inner: Vec<ProgLoc>,
}
impl Time {
    fn init() -> Self {
        Time { inner: Vec::new() }
    }
}

// Closure := ProgLoc x Env
pub struct Closure {
    prog_loc: ProgLoc,
    env: Env,
}

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash)]
pub struct Env {
    inner: HashMap<Var, VAddr>, // TODO ??? why not work ???
}
impl Env {
    fn init() -> Self {
        Env {
            inner: HashMap::new(), // TODO
        }
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct Var {
    inner: usize, // ast::Var, // TODO
}

// NOTE this references the Exps in the Ast
#[derive(Eq, PartialEq, Hash)]
pub struct ProgLoc {
    inner: usize, // ast::TypedCore, // TODO
}
impl ProgLoc {
    fn init() -> Self {
        ProgLoc {
            inner: 0, //ast::TypedCore::Null, // TODO
        }
    }
}

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Eq, PartialEq, Hash)]
pub struct Data {
    inner: usize, // ast::TypedCore, // TODO
}
