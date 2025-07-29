#![allow(warnings)]

use crate::ast;
use crate::ast::helper::VarInner;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone, Debug, PartialEq)]
pub struct State<'a> {
    pub procs: Procs<'a>,
    pub mailboxes: Mailboxes<'a>,
    pub store: Store<'a>,
}
impl<'a> State<'a> {
    pub fn init(ast: &'a ast::TypedCore) -> Self {
        State {
            procs: Procs::init(ProgLoc::init(ast)),
            mailboxes: Mailboxes::init(),
            store: Store::init(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Procs<'a> {
    pub inner: SetMap<Pid<'a>, ProcState<'a>>,
}
impl<'a> Procs<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        Procs {
            inner: SetMap::from([(
                Pid::init(prog_loc.clone()),
                HashSet::from([ProcState::init(prog_loc.clone())]),
            )]),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Mailboxes<'a> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a>>,
}
impl<'a> Mailboxes<'a> {
    fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
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
    pub fn init() -> Self {
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

#[derive(Clone, Debug, PartialEq)]
pub struct Store<'a> {
    pub kont: SetMap<KAddr<'a>, Kont<'a>>,
    pub value: SetMap<VAddr<'a>, Value<'a>>,
}
impl Store<'_> {
    fn init() -> Self {
        Store {
            kont: SetMap::init(),
            value: SetMap::init(),
        }
    }
}

// Pid := ProgLoc x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Pid<'a> {
    prog_loc: ProgLoc<'a>,
    time: Time<'a>,
}
impl<'a> Pid<'a> {
    fn new(&self, prog_loc: ProgLoc<'a>, time: Time<'a>) -> Self {
        // Generated the Pid for the new ProcState using its ProgLoc and Time
        let mut vec = time.inner;
        vec.append(&mut self.time.tick(self.prog_loc.clone()).inner);

        Pid {
            prog_loc,
            time: Time::new(vec),
        }
    }

    fn init(prog_loc: ProgLoc<'a>) -> Self {
        Pid {
            prog_loc,
            time: Time::init(),
        }
    }
}

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum ProgLocOrPid<'a> {
    ProgLoc(ProgLoc<'a>),
    Pid(Pid<'a>),
}
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ProcState<'a> {
    prog_loc_or_pid: ProgLocOrPid<'a>,
    env: Env<'a>,
    k_addr: KAddr<'a>,
    time: Time<'a>,
}
impl<'a> ProcState<'a> {
    fn new(
        prog_loc_or_pid: ProgLocOrPid<'a>,
        env: Env<'a>,
        k_addr: KAddr<'a>,
        time: Time<'a>,
    ) -> Self {
        ProcState {
            prog_loc_or_pid,
            env,
            k_addr,
            time,
        }
    }
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        ProcState {
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc.clone()),
            env: Env::init(),
            k_addr: KAddr::init(prog_loc.clone()),
            time: Time::init(),
        }
    }

    // TODO
    fn step(&self, pid: &Pid<'a>, store: &mut Store<'a>) -> Self {
        match self.prog_loc_or_pid {
            ProgLocOrPid::Pid(ref pid) => self.clone(),
            ProgLocOrPid::ProgLoc(ref prog_loc) => match prog_loc.inner {
                // NOTE could have more than one def
                ast::TypedCore::Module(module) => match &*module.defs.inner[0].scnd {
                    ast::TypedCore::Fun(fun) => match &*fun.body {
                        // NOTE could have more clauses than one
                        ast::TypedCore::Case(case) => match &case.clauses.inner[0] {
                            ast::TypedCore::Clause(clause) => {
                                return ProcState::new(
                                    // NOTE the pattern and guard of the clause could be
                                    // interesting in general
                                    ProgLocOrPid::ProgLoc(ProgLoc::new(&clause.body)),
                                    self.env.clone(),
                                    self.k_addr.clone(),
                                    self.time.clone(),
                                );
                            }
                            _ => self.clone(),
                        },
                        _ => self.clone(),
                    },
                    _ => self.clone(),
                },
                ast::TypedCore::Let(l) => {
                    // TODO handle the .expect properly (in a standard way)
                    let var_list =
                        Vec::<VarInner>::try_from(&l.vars).expect("Forked up conversion");
                    let arg = &l.arg;
                    let body = &l.body;

                    // Push-Let
                    let k_let = Kont::Let(
                        var_list,
                        ProgLoc { inner: body },
                        self.env.clone(),
                        self.k_addr.clone(),
                    );

                    let k_addr = KAddr::new(
                        pid.clone(),
                        prog_loc.clone(),
                        self.env.clone(),
                        self.time.clone(),
                    );

                    store.kont.push(k_addr.clone(), k_let);

                    return ProcState::new(
                        ProgLocOrPid::ProgLoc(ProgLoc::new(arg)),
                        self.env.clone(),
                        k_addr,
                        self.time.clone(),
                    );
                }
                ast::TypedCore::Fun(f) => {
                    // NOTE think through what the proper handling of this option type should be in
                    // this scenario
                    let konts = store.kont.get(&self.k_addr);
                    match konts {
                        Some(set) => {
                            // NOTE need to consider each possible continuation
                            // non-deterministically
                        }
                        None => panic!("Dafuq"),
                    }

                    self.clone()
                }
                _ => self.clone(),
            },
        }
    }
}

// Mailbox := P(Value)
#[derive(Clone, Debug, PartialEq)]
pub struct Mailbox<'a> {
    inner: HashSet<Value<'a>>,
}
impl Mailbox<'_> {
    fn mmatch(self) {} // TODO
    fn enq(self) {} // TODO
}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var: Var<'a>,
    data: Data<'a>,
    time: Time<'a>,
}
impl<'a> VAddr<'a> {
    fn new(pid: Pid<'a>, var: Var<'a>, data: Data<'a>, time: Time<'a>) -> Self {
        VAddr {
            pid,
            var,
            data,
            time,
        }
    }
}

// Value := Closure U+ Pid
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClosureOrPid<'a> {
    Closure(Closure<'a>),
    Pid(Pid<'a>),
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value<'a> {
    inner: ClosureOrPid<'a>,
}

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct KAddr<'a> {
    pid: Pid<'a>,
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
    time: Time<'a>,
}
impl<'a> KAddr<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        KAddr {
            pid: Pid::init(prog_loc.clone()),
            prog_loc,
            env: Env::init(),
            time: Time::init(),
        }
    }
    fn new(pid: Pid<'a>, prog_loc: ProgLoc<'a>, env: Env<'a>, time: Time<'a>) -> Self {
        KAddr {
            pid,
            prog_loc,
            env,
            time,
        }
    }
}

// Kont :=
//       | List<Name>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Kont<'a> {
    Let(Vec<VarInner>, ProgLoc<'a>, Env<'a>, KAddr<'a>),
    Do(ProgLoc<'a>, Env<'a>, KAddr<'a>),
    Stop,
}

// Time := ProgLoc^k
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Time<'a> {
    inner: Vec<ProgLoc<'a>>,
}
impl<'a> Time<'a> {
    fn new(time: Vec<ProgLoc<'a>>) -> Self {
        Time { inner: time }
    }

    fn init() -> Self {
        Time { inner: Vec::new() }
    }

    fn tick(&self, prog_loc: ProgLoc<'a>) -> Self {
        let mut ticked_time = self.clone();
        ticked_time.inner.push(prog_loc);
        ticked_time
    }
}

// Closure := ProgLoc x Env
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Closure<'a> {
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
}

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Env<'a> {
    inner: BTreeMap<Var<'a>, VAddr<'a>>,
}
impl Env<'_> {
    fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Var<'a> {
    inner: &'a ast::Var,
}

// NOTE this references the Exps in the Ast
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ProgLoc<'a> {
    inner: &'a ast::TypedCore,
}
impl<'a> ProgLoc<'a> {
    fn new(inner: &'a ast::TypedCore) -> Self {
        ProgLoc { inner }
    }
    fn init(ast: &'a ast::TypedCore) -> Self {
        ProgLoc { inner: ast }
    }
}

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Data<'a> {
    inner: &'a ast::TypedCore,
}
