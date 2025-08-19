use std::fmt::{Debug, Display};

#[derive(Clone, Debug)]
pub struct SetMap<K, V>
where
    K: Eq,
    V: Eq,
{
    pub inner: Vec<(K, Vec<V>)>,
}

impl<K: Clone + Eq, V: Clone + Eq> SetMap<K, V> {
    pub fn new() -> Self {
        SetMap { inner: Vec::new() }
    }

    /// Inserts a new value into the value set for the given key
    pub fn push(&mut self, key: K, value: V) {
        for (k, v) in &mut self.inner {
            if k == &key {
                if v.contains(&value) {
                    return;
                }
                v.push(value);
                return;
            }
        }
        self.inner.push((key, vec![value]));
    }

    /// Returns a reference to the set of values associated with a given key, if the key exists
    pub fn get(&self, key: &K) -> Option<&Vec<V>> {
        for (k, v) in &self.inner {
            if k == key {
                return Some(v);
            }
        }
        None
    }

    /// Returns a mutable reference to the the set of values associated with a given key, if the
    /// key exists
    pub fn get_mut(&mut self, key: &K) -> Option<&mut Vec<V>> {
        for (k, v) in &mut self.inner {
            if k == key {
                return Some(v);
            }
        }
        None
    }
}

impl<K: Eq + Display, V: Eq + Display> Display for SetMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items: Vec<String> = self
            .inner
            .iter()
            .map(|(key, values)| {
                format!(
                    "{}: {{{}}}",
                    key,
                    values
                        .iter()
                        .map(|value| format!("{}", value))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            })
            .collect();
        write!(f, "{:#?}", items)
    }
}
