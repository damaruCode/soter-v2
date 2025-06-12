#![allow(warnings)]

use crate::ast;
use std::collections::HashMap;
use std::collections::HashSet;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
pub struct State<'a> {
    pub procs: Procs<'a>,
    pub mailboxes: Mailboxes<'a>,
    pub value_store: ValueStore<'a>,
    pub continuation_store: ContinuationStore<'a>,
}

pub fn init_state(prog_loc: &ast::TypedCore) -> State {
    let pid = init_pid(prog_loc);
    let time = init_time(prog_loc);

    let procs = init_procs(pid, prog_loc, time);
    let mailboxes = Mailboxes::init(pid.clone());
    let val_store = ValueStore {
        inner: HashMap::new(),
    };
    let kont_store = ContinuationStore {
        inner: HashMap::new(),
    };

    return State {
        procs: procs,
        mailboxes: mailboxes,
        value_store: val_store,
        continuation_store: kont_store,
    };
}

pub struct Procs<'a> {
    pub inner: HashMap<Pid<'a>, HashSet<ProcState<'a>>>,
}

pub fn init_procs<'a>(
    pid_loc: &'a Pid,
    prog_loc: &ast::TypedCore,
    time_loc: &'a Time<'a>,
) -> Procs<'a> {
    let inner = HashMap::new();
    inner.insert(pid_loc, init_proc_state(pid_loc, prog_loc, time_loc));

    return Procs { inner: inner };
}

pub struct Mailboxes<'a> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a>>,
}

impl Mailboxes<'_> {
    fn init(pid: Pid) -> Self {
        let mut mailbox_map = HashMap::new();
        mailbox_map.insert(
            pid,
            Mailbox {
                inner: HashSet::new(),
            },
        );

        return Mailboxes { inner: mailbox_map };
    }
}

pub struct ValueStore<'a> {
    pub inner: HashMap<VAddr<'a>, HashSet<Value<'a>>>,
}
pub struct ContinuationStore<'a> {
    pub inner: HashMap<KAddr<'a>, HashSet<Kont<'a>>>,
}

// Pid := ProgLoc x Time
#[derive(Eq)]
pub struct Pid<'a> {
    //prog_loc: &'a ast::TypedCore,
    time: Time<'a>,
}

pub fn init_pid(prog_loc: &ast::TypedCore) -> Pid {
    return Pid {
        prog_loc: prog_loc,
        time: init_time(prog_loc),
    };
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
pub enum ProgLocOrPid<'a> {
    ProgLoc(&'a ast::TypedCore),
    Pid(Pid<'a>),
}
pub struct ProcState<'a> {
    prog_loc_or_pid: ProgLocOrPid<'a>,
    env: Env<'a>,
    k_addr: KAddr<'a>,
    time: &'a Time<'a>,
}

pub fn init_proc_state<'a>(
    pid: &'a Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    time: &'a Time<'a>,
) -> ProcState<'a> {
    let env = init_env();
    let kaddr = init_kaddr(pid, prog_loc, &env, time);

    return ProcState {
        prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc),
        env: env,
        k_addr: kaddr,
        time: time,
    };
}

// Mailbox := P(Value)
pub struct Mailbox<'a> {
    inner: HashSet<Value<'a>>,
}

// VAddr := Pid x Var x Data x Time
#[derive(Clone, Copy)]
pub struct VAddr<'a> {
    pid: &'a Pid<'a>,
    var: &'a ast::Var,
    data: Data<'a>,
    time: Time<'a>,
}

// Value := Closure U+ Pid
pub enum ClosureOrPid<'a> {
    Closure(Closure<'a>),
    Pid(Pid<'a>),
}
pub struct Value<'a> {
    inner: ClosureOrPid<'a>,
}

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
pub struct KAddr<'a> {
    pid: &'a Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    env: &'a Env<'a>,
    time: &'a Time<'a>,
}

pub fn init_kaddr<'a>(
    pid: &'a Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    env: &'a Env<'a>,
    time: &'a Time<'a>,
) -> KAddr<'a> {
    return KAddr {
        pid: pid,
        prog_loc: prog_loc,
        env: env,
        time: time,
    };
}

// Kont := index x ProgLoc x Data* x Env x KAddr
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
pub struct Kont<'a> {
    index: usize,
    prog_loc: &'a ast::TypedCore,
    vec_data: [Data<'a>; 0],
    env: Env<'a>,
    k_addr: KAddr<'a>,
}

// Time := ProgLoc^k
#[derive(Clone, Eq)]
pub struct Time<'a> {
    inner: usize, //Vec<&'a ast::TypedCore>,
}

pub fn init_time(prog_loc: &ast::TypedCore) -> Time {
    return Time { inner: [] };
}

// Closure := ProgLoc x Env
pub struct Closure<'a> {
    prog_loc: &'a ast::TypedCore,
    env: Env<'a>,
}

// Env := Var -> VAddr
pub struct Env<'a> {
    inner: HashMap<&'a ast::Var, VAddr<'a>>,
}

pub fn init_env<'a>() -> Env<'a> {
    return Env {
        inner: HashMap::new(),
    };
}

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Clone, Copy)]
pub struct Data<'a> {
    inner: &'a ast::TypedCore,
}
