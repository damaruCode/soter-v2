use std::fmt::{Debug, Display};

use super::{Kont, KontinuationAddress, Value, ValueAddress};
use crate::state_space::Env;
use crate::util::SetMap;

#[derive(Clone, Debug)]
pub struct Store<K: KontinuationAddress, V: ValueAddress> {
    pub kont: SetMap<K, Kont<K, V>>,
    pub value: SetMap<V, Value<V>>,
}

impl<K: KontinuationAddress, V: ValueAddress> Store<K, V> {
    pub fn init(stop_k_addr: K) -> Self {
        let mut kont = SetMap::new();
        kont.push(stop_k_addr, Kont::Stop);

        Store {
            kont,
            value: SetMap::new(),
        }
    }

    pub fn unpack(&self, env: &Env<V>, var_id: &usize) -> &Vec<Value<V>> {
        self.value.get(env.inner.get(var_id).unwrap()).unwrap()
    }
}

impl<K: KontinuationAddress, V: ValueAddress> Display for Store<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\nKONT: {}\nVALUE: {}", self.kont, self.value)
    }
}
