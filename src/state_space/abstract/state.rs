use super::{KontinuationAddress, Mailboxes, Pid, ProcState, Store, ValueAddress};
use crate::util::SetMap;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone, Debug, PartialEq)]
pub struct State<K: KontinuationAddress, V: ValueAddress> {
    pub procs: SetMap<Pid, ProcState<K, V>>,
    pub mailboxes: Mailboxes<V>,
    pub store: Store<K, V>,
}

impl<K: KontinuationAddress, V: ValueAddress> State<K, V> {
    pub fn init(init_k_addr: K) -> Self {
        let mut procs = SetMap::new();
        procs.push(Pid::init(), ProcState::<K, V>::init(init_k_addr));

        State {
            procs,
            mailboxes: Mailboxes::<V>::init(),
            store: Store::init(),
        }
    }
}
