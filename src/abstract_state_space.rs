#![allow(warnings)]

use crate::ast;
use std::collections::HashMap;
use std::collections::HashSet;

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

pub struct Procs {
    pub inner: HashMap<Pid, HashSet<ProcState>>,
}
pub struct Mailboxes {
    pub inner: HashMap<Pid, Mailbox>,
}
pub struct ValueStore {
    pub inner: HashMap<VAddr, HashSet<Value>>,
}
pub struct ContinuationStore {
    pub inner: HashMap<KAddr, HashSet<Kont>>,
}

// Pid := ProgLoc x Time
pub struct Pid {
    prog_loc: ProgLoc,
    time: Time,
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
pub enum ProgLocOrPid {
    ProgLoc(ProgLoc),
    Pid(Pid),
}
pub struct ProcState {
    prog_loc_or_pid: ProgLocOrPid,
    env: Env,
    k_addr: KAddr,
    time: Time,
}

// Mailbox := P(Value)
pub struct Mailbox {
    inner: HashSet<Value>,
}

// VAddr := Pid x Var x Data x Time
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
pub struct KAddr {
    pid: Pid,
    prog_loc: ProgLoc,
    env: Env,
    time: Time,
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
pub struct Time {
    inner: Vec<ProgLoc>,
}

// Closure := ProgLoc x Env
pub struct Closure {
    prog_loc: ProgLoc,
    env: Env,
}

// Env := Var -> VAddr
pub struct Env {
    inner: HashMap<Var, VAddr>,
}

pub struct Var {
    inner: ast::Var,
}

// NOTE this references the Exps in the Ast
pub struct ProgLoc {
    inner: ast::TypedCore,
}

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
pub struct Data {
    inner: ast::TypedCore,
}
