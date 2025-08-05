use super::{KontinuationAddress, Mailboxes, Pid, ProcState, ProgLoc, Store, ValueAddress};
use crate::{ast::TypedCore, util::SetMap};

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone, Debug, PartialEq)]
pub struct State<'a, K: KontinuationAddress, V: ValueAddress> {
    pub procs: SetMap<Pid<'a>, ProcState<'a, K, V>>,
    pub mailboxes: Mailboxes<'a, V>,
    pub store: Store<'a, K, V>,
}

impl<'a, K: KontinuationAddress, V: ValueAddress> State<'a, K, V> {
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
