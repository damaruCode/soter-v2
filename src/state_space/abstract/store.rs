use super::{Kont, KontinuationAddress, Value, ValueAddress};
use crate::util::SetMap;

#[derive(Clone, Debug)]
pub struct Store<K: KontinuationAddress, V: ValueAddress> {
    pub kont: SetMap<K, Kont<K, V>>,
    pub value: SetMap<V, Value<V>>,
}

impl<K: KontinuationAddress, V: ValueAddress> Store<K, V> {
    pub fn init() -> Self {
        Store {
            kont: SetMap::new(),
            value: SetMap::new(),
        }
    }
}
