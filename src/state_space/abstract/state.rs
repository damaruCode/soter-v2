use super::{Mailboxes, Pid, ProcState, ProgLoc, Store};
use crate::{ast::TypedCore, util::SetMap};

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone, Debug, PartialEq)]
pub struct State<'a> {
    pub procs: SetMap<Pid<'a>, ProcState<'a>>,
    pub mailboxes: Mailboxes<'a>,
    pub store: Store<'a>,
}

impl<'a> State<'a> {
    pub fn init(ast: &'a TypedCore) -> Self {
        let mut procs = SetMap::new();
        let prog_loc = ProgLoc::init(ast);
        procs.push(Pid::init(prog_loc.clone()), ProcState::init(prog_loc));

        State {
            procs,
            mailboxes: Mailboxes::init(),
            store: Store::init(),
        }
    }
}
