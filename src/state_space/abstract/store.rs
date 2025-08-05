use std::collections::HashSet;

use super::{Kont, KontinuationAddress, Value, ValueAddress};
use crate::util::SetMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Store<'a, K: KontinuationAddress, V: ValueAddress> {
    kont: SetMap<K, Kont<'a, K, V>>,
    value: SetMap<V, Value<'a, V>>,
}

impl<'a, K: KontinuationAddress, V: ValueAddress> Store<'a, K, V> {
    pub fn init() -> Self {
        Store {
            kont: SetMap::new(),
            value: SetMap::new(),
        }
    }

    pub fn get_kont(&self, key: &'a K) -> Option<&HashSet<Kont<'a, K, V>>> {
        self.kont.get(key)
    }

    pub fn get_value(&self, key: &'a V) -> Option<&HashSet<Value<'a, V>>> {
        self.value.get(key)
    }

    pub fn insert_kont(&mut self, key: K, kont: Kont<'a, K, V>) {
        self.kont.push(key, kont);
    }

    pub fn insert_value(&mut self, key: V, value: Value<'a, V>) {
        self.value.push(key, value);
    }
}
