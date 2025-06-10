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

    return State {
        procs: init_procs(&pid, prog_loc, &time),
        mailboxes: init_mailboxes(&pid),
        value_store: ValueStore {
            inner: HashMap::new(),
        },
        continuation_store: ContinuationStore {
            inner: HashMap::new(),
        },
    };
}

pub struct Procs<'a> {
    pub inner: HashMap<Pid<'a>, HashSet<ProcState<'a>>>,
}

pub fn init_procs<'a>(pid_loc: &Pid, prog_loc: &ast::TypedCore) -> Procs<'a> {
    let inner = HashMap::new();
    inner.insert(pid_loc, init_proc_state(prog_loc));

    return Procs { inner: inner };
}

pub struct Mailboxes<'a> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a>>,
}

pub fn init_mailboxes<'a>(pid: &Pid) -> Mailboxes<'a> {
    let mailbox_map = HashMap::new();
    mailbox_map.insert(
        pid,
        Mailbox {
            inner: HashSet::new(),
        },
    );

    return Mailboxes { inner: mailbox_map };
}

pub struct ValueStore<'a> {
    pub inner: HashMap<VAddr<'a>, HashSet<Value<'a>>>,
}
pub struct ContinuationStore<'a> {
    pub inner: HashMap<KAddr<'a>, HashSet<Kont<'a>>>,
}

// Pid := ProgLoc x Time
pub struct Pid<'a> {
    prog_loc: &'a ast::TypedCore,
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
    time: Time<'a>,
}

pub fn init_proc_state<'a>(
    pid: Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    time: Time<'a>,
) -> ProcState<'a> {
    let env = init_env();
    let kaddr = init_kaddr(pid, prog_loc, env.clone(), time.clone());

    return ProcState {
        prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc),
        env: env.clone(),
        k_addr: kaddr,
        time: time.clone(),
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
    pid: Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    env: Env<'a>,
    time: Time<'a>,
}

pub fn init_kaddr<'a>(
    pid: Pid<'a>,
    prog_loc: &'a ast::TypedCore,
    env: Env<'a>,
    time: Time<'a>,
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
#[derive(Clone, Copy)]
pub struct Time<'a> {
    inner: [&'a ast::TypedCore; 0],
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
#[derive(Clone, Copy)]
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
