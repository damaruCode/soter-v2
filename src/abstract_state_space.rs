#![allow(warnings)]

use crate::ast;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;

// State := Procs x Mailboxes x Store
//
// Procs := Pid -> P(ProcState)
// Mailboxes := Pid -> Mailbox
// Store := (VAddr -> P(Value)) x (KAddr -> P(Kont))
#[derive(Clone, Debug)]
pub struct State<'a> {
    pub procs: Procs<'a>,
    pub mailboxes: Mailboxes<'a>,
    pub value_store: ValueStore<'a>,
    pub continuation_store: ContinuationStore<'a>,
}
impl<'a> State<'a> {
    pub fn init(ast: &'a ast::TypedCore) -> Self {
        State {
            procs: Procs::init(ProgLoc::init(ast)),
            mailboxes: Mailboxes::init(),
            value_store: ValueStore::init(),
            continuation_store: ContinuationStore::init(),
        }
    }

    // TODO
    pub fn step(&self) -> Self {
        let mut new_state = self.clone();

        for (pid, proc_states) in &self.procs.inner {
            let set = new_state.procs.inner.get_mut(pid).unwrap();
            set.clear();
            for (proc_state) in proc_states {
                set.insert(proc_state.step());
            }
        }

        new_state
    }
}

#[derive(Clone, Debug)]
pub struct Procs<'a> {
    pub inner: HashMap<Pid<'a>, HashSet<ProcState<'a>>>,
}
impl<'a> Procs<'a> {
    fn init(prog_loc: ProgLoc<'a>) -> Self {
        Procs {
            inner: HashMap::from([(
                Pid::init(prog_loc.clone()),
                HashSet::from([ProcState::init(prog_loc.clone())]),
            )]),
        }
    }
}
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct ValueStore<'a> {
    pub inner: HashMap<VAddr<'a>, HashSet<Value<'a>>>,
}
impl<'a> ValueStore<'a> {
    fn init() -> Self {
        ValueStore {
            inner: HashMap::new(),
        }
    }
}
#[derive(Clone, Debug)]
pub struct ContinuationStore<'a> {
    pub inner: HashMap<KAddr<'a>, HashSet<Kont<'a>>>,
}
impl ContinuationStore<'_> {
    fn init() -> Self {
        ContinuationStore {
            inner: HashMap::new(),
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
    fn step(&self) -> Self {
        match self.prog_loc_or_pid {
            ProgLocOrPid::Pid(ref pid) => self.clone(),
            ProgLocOrPid::ProgLoc(ref prog_loc) => match prog_loc.inner {
                // NOTE could have more than one def
                ast::TypedCore::Module(m) => match &*m.defs.inner[0].scnd {
                    ast::TypedCore::Fun(f) => {
                        return ProcState::new(
                            ProgLocOrPid::ProgLoc(ProgLoc::new(&*f.body)),
                            self.env.clone(),
                            self.k_addr.clone(),
                            self.time.clone(),
                        );
                    }
                    _ => self.clone(),
                },
                _ => self.clone(),
            },
        }
    }
}

// Mailbox := P(Value)
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub enum ClosureOrPid<'a> {
    Closure(Closure<'a>),
    Pid(Pid<'a>),
}
#[derive(Clone, Debug)]
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

// Kont := index x ProgLoc x Data* x Env x KAddr
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug)]
pub struct Kont<'a> {
    index: usize,
    prog_loc: ProgLoc<'a>,
    vec_data: Vec<Data<'a>>,
    env: Env<'a>,
    k_addr: KAddr<'a>,
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
#[derive(Clone, Debug)]
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
