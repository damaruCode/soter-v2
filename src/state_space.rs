#![allow(warnings)]

use crate::ast;
use std::collections::HashMap;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> ProcState
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> Value) x (KAddr -> Kont)
pub struct State {
    procs: HashMap<Pid, usize>,
    mailboxes: HashMap<Pid, usize>,
    value_store: HashMap<usize, usize>,
    continuation_store: HashMap<usize, usize>,
}

// Pid := ProgLoc x Time
pub struct Pid {
    prog_loc: ProgLoc,
    time: Time,
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
pub struct ProcState {}

// Mailbox := Value*
pub struct Mailbox {
    inner: Vec<usize>,
}

// VAddr := Pid x Var x Data x Time
pub struct VAddr {}

// Value := Closure U+ Pid
pub struct Value {}

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
pub struct KAddr {}

// Kont := index x ProgLoc x Data* x Env x KAddr
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
pub struct Kont {}

// Time := ProgLoc*
pub struct Time {
    inner: Vec<ProgLoc>,
}

// Closure := ProgLoc x Env
pub struct Closure {}

// Env := Var -> VAddr
pub struct Env {}

// NOTE maybe use ast::Var directly?
pub struct Var {
    inner: ast::Var,
}

// NOTE maybe use ast::TypedCore directly?
// this is just to reference the Exps in the Ast
pub struct ProgLoc {
    inner: ast::TypedCore,
}

// NOTE the free vars of the TypedCore is replaced with the values of the higher scopes and has therefore no
// free vars anymore
pub struct Data {
    inner: ast::TypedCore,
}
