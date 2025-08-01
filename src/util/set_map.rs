use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetMap<K, V>
where
    K: PartialEq + Eq + Hash,
    V: PartialEq + Eq + Hash,
{
    inner: HashMap<K, HashSet<V>>,
}
// NOTE Might have to revisit the actual implementation of Eq on the constituent parts; Otherwise
// keys that should be identical will not be handled as such!
impl<K: Clone + Eq + Hash, V: Clone + Eq + Hash> SetMap<K, V> {
    pub fn new() -> Self {
        SetMap {
            inner: HashMap::new(),
        }
    }

    /// Inserts a new value into the value set for the given key
    pub fn push(&mut self, key: K, value: V) {
        self.inner
            .entry(key) // Either take the existent HashSet
            .or_insert_with(HashSet::new) // or create a new one
            .insert(value);
    }

    /// Returns a reference to the set of values associated with a given key, if the key exists
    pub fn get(&self, key: &K) -> Option<&HashSet<V>> {
        self.inner.get(key)
    }

    /// Returns a mutable reference to the the set of values associated with a given key, if the
    /// key exists
    pub fn get_mut(&mut self, key: &K) -> Option<&mut HashSet<V>> {
        self.inner.get_mut(key)
    }
}
impl<K, V, const N: usize> From<[(K, HashSet<V>); N]> for SetMap<K, V>
where
    K: Eq + Hash,
    V: Eq + Hash,
{
    fn from(value: [(K, HashSet<V>); N]) -> Self {
        SetMap {
            inner: HashMap::from(value),
        }
    }
}
pub struct SetMapIterator<'a, K, V> {
    map_iter: std::collections::hash_map::Iter<'a, K, HashSet<V>>,
    set_iter: Option<std::collections::hash_set::Iter<'a, V>>,
    current_key: Option<&'a K>,
}
impl<'a, K, V> Iterator for SetMapIterator<'a, K, V>
where
    V: Eq + Hash,
    K: Eq + Hash,
{
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(iter) = &mut self.set_iter {
                if let Some(value) = iter.next() {
                    return Some((self.current_key.unwrap(), value));
                } else {
                    self.set_iter = None; // Move to next key
                }
            }

            if let Some((key, set)) = self.map_iter.next() {
                self.current_key = Some(key);
                self.set_iter = Some(set.iter());
            } else {
                return None; // No more elements
            }
        }
    }
}

impl<'a, K, V> IntoIterator for &'a SetMap<K, V>
where
    K: 'a + Eq + Hash,
    V: 'a + Eq + Hash,
{
    type Item = (&'a K, &'a V);
    type IntoIter = SetMapIterator<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        let map_iter = self.inner.iter();
        SetMapIterator {
            map_iter,
            set_iter: None,
            current_key: None,
        }
    }
}
