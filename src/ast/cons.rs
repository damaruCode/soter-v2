use crate::ast::*;
use serde::{Deserialize, Serialize};

/// -record(c_cons, {anno=[] :: list(), hd :: cerl:cerl(),
///    tl :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Cons {
    pub anno: AstList<TypedCore>,
    pub hd: Box<TypedCore>, // some var or value (literal, cons, tuple)
    pub tl: Box<TypedCore>, // cons or literal []
    pub index: MaybeIndex,
}

impl Default for Cons {
    fn default() -> Self {
        Self::new()
    }
}

impl Cons {
    pub fn new() -> Self {
        Self {
            anno: AstList::from(TypedCore::new()),
            hd: Box::new(TypedCore::new()),
            tl: Box::new(TypedCore::new()),
            index: MaybeIndex::None,
        }
    }
}

impl From<Map<String, Value>> for Cons {
    fn from(map: Map<String, Value>) -> Self {
        Cons {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            hd: Box::new(TypedCore::from(map.get("hd").unwrap().clone())),
            tl: Box::new(TypedCore::from(map.get("tl").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Cons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}cons {} {}", self.index, *self.hd, *self.tl)
    }
}

// -----------------------------------
// Iterator
// -----------------------------------
enum ConsPos<'a> {
    Cons(&'a Cons),
    End(&'a TypedCore), // the tail once it stops being a Cons (e.g. `[]` for a proper list)
}

pub struct ConsIter<'a> {
    pos: Option<ConsPos<'a>>,
}

impl Cons {
    pub fn iter(&self) -> ConsIter<'_> {
        ConsIter {
            pos: Some(ConsPos::Cons(self)),
        }
    }

    /// Collects mutable references to every element `hd_1, hd_2, ..., tail`.
    /// The final element is the list's tail once it stops being a Cons
    /// (`[]` for a proper list, a var/etc. for an improper one).
    pub fn iter_mut_collect(&mut self) -> Vec<&mut TypedCore> {
        let mut out = Vec::new();
        Self::collect_mut(self, &mut out);
        out
    }

    fn collect_mut<'a>(node: &'a mut Cons, out: &mut Vec<&'a mut TypedCore>) {
        out.push(node.hd.as_mut());
        match node.tl.as_mut() {
            TypedCore::Cons(next) => Self::collect_mut(next, out), // <-- adjust variant name
            other => out.push(other),
        }
    }
}

impl<'a> Iterator for ConsIter<'a> {
    type Item = &'a TypedCore;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pos.take() {
            Some(ConsPos::Cons(c)) => {
                let hd = c.hd.as_ref();
                self.pos = Some(match c.tl.as_ref() {
                    TypedCore::Cons(next) => ConsPos::Cons(next),
                    other => ConsPos::End(other),
                });
                Some(hd)
            }
            Some(end @ ConsPos::End(_)) => {
                self.pos = Some(end); // stash it back so `tail()` still works after iteration
                None
            }
            None => None,
        }
    }
}

impl<'a> ConsIter<'a> {
    /// What the list ended on. `None` if iteration isn't finished yet.
    /// Once finished: `Some(literal [])` for a proper list
    pub fn tail(&self) -> Option<&'a TypedCore> {
        match &self.pos {
            Some(ConsPos::End(t)) => Some(t),
            _ => None,
        }
    }
}

impl<'a> IntoIterator for &'a Cons {
    type Item = &'a TypedCore;
    type IntoIter = ConsIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
