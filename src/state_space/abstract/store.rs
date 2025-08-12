use std::collections::HashSet;

use super::{Kont, KontinuationAddress, Value, ValueAddress};
use crate::util::SetMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Store<K: KontinuationAddress, V: ValueAddress> {
    kont: SetMap<K, Kont<K, V>>,
    value: SetMap<V, Value<V>>,
}

impl<K: KontinuationAddress, V: ValueAddress> Store<K, V> {
    pub fn init() -> Self {
        Store {
            kont: SetMap::new(),
            value: SetMap::new(),
        }
    }

    pub fn get_kont(&self, key: &K) -> Option<&HashSet<Kont<K, V>>> {
        self.kont.get(key)
    }

    pub fn get_value(&self, key: &V) -> Option<&HashSet<Value<V>>> {
        self.value.get(key)
    }

    pub fn insert_kont(&mut self, key: K, kont: Kont<K, V>) {
        self.kont.push(key, kont);
    }

    pub fn insert_value(&mut self, key: V, value: Value<V>) {
        self.value.push(key, value);
    }
}
