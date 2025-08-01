use std::collections::HashSet;

use super::{KAddr, Kont, VAddr, Value};
use crate::util::SetMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Store<'a> {
    kont: SetMap<KAddr<'a>, Kont<'a>>,
    value: SetMap<VAddr<'a>, Value<'a>>,
}
impl<'a> Store<'a> {
    pub fn init() -> Self {
        Store {
            kont: SetMap::new(),
            value: SetMap::new(),
        }
    }

    pub fn get_kont(&self, key: &'a KAddr) -> Option<&HashSet<Kont<'a>>> {
        self.kont.get(key)
    }

    pub fn get_value(&self, key: &'a VAddr) -> Option<&HashSet<Value<'a>>> {
        self.value.get(key)
    }

    pub fn insert_kont(&mut self, key: KAddr<'a>, kont: Kont<'a>) {
        self.kont.push(key, kont);
    }

    pub fn insert_value(&mut self, key: VAddr<'a>, value: Value<'a>) {
        self.value.push(key, value);
    }
}
