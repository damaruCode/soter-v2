use crate::ast::Case;
use crate::ast::Index;
use crate::ast::TypedCore;
use crate::state_space::r#abstract::*;
use crate::util::AstHelper;
use crate::util::SetMap;

use std::collections::VecDeque;

mod allocation_schemes;
pub use allocation_schemes::*;

pub enum TransitionError {
    ErroneousTransition,
    NoValidTransition,
}

pub struct Analyzer<'analyzer, K: KontinuationAddress, V: ValueAddress> {
    ast_helper: AstHelper<'analyzer>,
    address_builder: Box<dyn AddressBuilder<K, V>>,
    mailboxes: Mailboxes<V>,
    store: Store<K, V>,
    queue: VecDeque<ProcState<K, V>>,
    seen: SetMap<Pid, ProcState<K, V>>,
}

impl<'analyzer, K: KontinuationAddress, V: ValueAddress> Analyzer<'analyzer, K, V> {
    pub fn new(
        ast_helper: AstHelper<'analyzer>,
        address_builder: Box<dyn AddressBuilder<K, V>>,
    ) -> Self {
        let k_addr = address_builder.init_kaddr();
        Analyzer {
            ast_helper,
            address_builder,
            mailboxes: Mailboxes::init(),
            store: Store::init(),
            queue: VecDeque::from(vec![ProcState::init(k_addr)]),
            seen: SetMap::new(),
        }
    }

    pub fn push_to_mailboxes(
        ast_helper: &AstHelper,
        seen: &SetMap<Pid, ProcState<K, V>>,
        mailboxes: &mut Mailboxes<V>,
        pid: Pid,
        value: Value<V>,
    ) -> Vec<ProcState<K, V>> {
        mailboxes.push(pid.clone(), value);

        let mut dependencies = Vec::new();
        match seen.get(&pid) {
            Some(set) => {
                for state in set {
                    match state.prog_loc_or_pid {
                        ProgLocOrPid::ProgLoc(location) => {
                            match ast_helper.get(location) {
                                TypedCore::Receive(_) => {
                                    // NOTE cloning here might become a memory issue
                                    dependencies.push(state.clone());
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        dependencies
    }

    pub fn push_to_value_store(
        ast_helper: &AstHelper,
        seen: &SetMap<Pid, ProcState<K, V>>,
        store: &mut Store<K, V>,
        v_addr: V,
        value: Value<V>,
    ) -> Vec<ProcState<K, V>> {
        store.value.push(v_addr.clone(), value);

        let mut dependencies = Vec::new();
        for (_, states) in &seen.inner {
            for state in states {
                match state.prog_loc_or_pid {
                    ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
                        TypedCore::Var(pl_var) => {
                            match state.env.inner.get(&VarName::from(&*pl_var.name)) {
                                Some(pl_vaddr) => {
                                    if pl_vaddr == &v_addr {
                                        // NOTE cloning here might become a memory issue
                                        dependencies.push(state.clone());
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }
        dependencies
    }

    pub fn push_to_kont_store(
        ast_helper: &AstHelper,
        seen: &SetMap<Pid, ProcState<K, V>>,
        store: &mut Store<K, V>,
        k_addr: K,
        kont: Kont<K, V>,
    ) -> Vec<ProcState<K, V>> {
        store.kont.push(k_addr.clone(), kont);

        let mut dependencies = Vec::new();
        for (_, states) in &seen.inner {
            for state in states {
                if state.k_addr != k_addr {
                    continue;
                }
                match state.prog_loc_or_pid {
                    ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
                        // TODO add the arms that are non-reducable
                        _ => {}
                    },
                    ProgLocOrPid::Pid(_) => {
                        // NOTE cloning here might become a memory issue
                        dependencies.push(state.clone());
                    }
                }
            }
        }
        dependencies
    }

    pub fn run(&mut self) {
        // This terminates because it assumes a fixpoint implementation
        while let Some(item) = self.queue.pop_front() {
            let (new_items, revisit_items) = item.process(
                &self.ast_helper,
                &mut self.mailboxes,
                &mut self.store,
                &self.address_builder,
                &self.seen,
            );
            for item in revisit_items {
                self.queue.push_back(item);
            }

            for item in new_items {
                // NOTE cloning here might become a memory issue
                if let Some(items) = self.seen.get_mut(&item.pid) {
                    if items.contains(&item) {
                        continue;
                    }
                }
                self.seen.push(item.pid.clone(), item.clone());
                self.queue.push_back(item);
            }
        }
    }
}

pub trait WorkItem<K: KontinuationAddress, V: ValueAddress>: Eq + Clone {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<Self>, Vec<Self>);
}

impl<K: KontinuationAddress, V: ValueAddress> WorkItem<K, V> for ProcState<K, V> {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<Self>, Vec<Self>) {
        //TODO Delete log
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAST - {:#?}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        let mut v_new = Vec::new();
        let mut v_revisit = Vec::new();

        match &self.prog_loc_or_pid {
            // ABS_POP_LET_PID
            ProgLocOrPid::Pid(_pid) => {}
            ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                TypedCore::Module(m) => match &*m.defs.inner[0].scnd {
                    TypedCore::Fun(f) => match &*f.body {
                        // NOTE could have more clauses than one
                        TypedCore::Case(c) => match &c.clauses.inner[0] {
                            TypedCore::Clause(c) => {
                                let mut new_item = self.clone();
                                let index = (*c.body).get_index().unwrap();
                                new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(index);
                                v_new.push(new_item);
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                TypedCore::Var(v) => match self.env.inner.get(&VarName::from(&*v.name)) {
                    Some(v) => match store.value.get(&v) {
                        Some(values) => {
                            for value in values {
                                // consider each
                                // non-deterministically
                                match value {
                                    Value::Closure(c) => {
                                        let mut new_item = self.clone();
                                        new_item.prog_loc_or_pid =
                                            ProgLocOrPid::ProgLoc(c.prog_loc);
                                        v_new.push(new_item);
                                    }
                                    Value::Pid(_) => {
                                        panic!("Unexpected value: Expected Closure not Pid")
                                    }
                                }
                            }
                        }
                        None => panic!("VAddr does not exist within value store"),
                    },
                    None => panic!("No VAddr exists for given Var"),
                },
                // ABS_APPLY
                TypedCore::Apply(_apply) => {
                    panic!();
                }
                // ABS_CALL
                TypedCore::Call(c) => {
                    match &*c.name {
                        TypedCore::Literal(l) => match &*l.val {
                            TypedCore::String(s) => match s.inner.as_str() {
                                "spawn" => {
                                    let values = store
                                        .value
                                        .get(
                                            self.env
                                                .inner
                                                .get(&VarName::from(&c.args.inner[0]))
                                                .unwrap(),
                                        )
                                        .unwrap();

                                    for value in values {
                                        match value {
                                            Value::Closure(clo) => {
                                                match ast_helper.get(clo.prog_loc) {
                                                    TypedCore::Fun(f) => {
                                                        if f.vars.inner.len() != 0 {
                                                            panic!();
                                                        }
                                                        let mut new_time =
                                                            self.pid.time.tick(self.pid.prog_loc);
                                                        new_time.append(self.time.inner.clone());
                                                        let new_pid = Pid {
                                                            prog_loc: *pl,
                                                            time: new_time,
                                                        };

                                                        let mut new_proc_state_one = self.clone();
                                                        new_proc_state_one.prog_loc_or_pid =
                                                            ProgLocOrPid::Pid(new_pid.clone());

                                                        v_new.push(new_proc_state_one);

                                                        match &*f.body {
                                                            TypedCore::Case(c) => {
                                                                match &c.clauses.inner[0] {
                                                                    TypedCore::Clause(c) => {
                                                                        let new_proc_state_two =
                                                                    ProcState::new(
                                                                        new_pid.clone(),
                                                                        ProgLocOrPid::ProgLoc(
                                                                            (*c.body)
                                                                                .get_index()
                                                                                .unwrap(),
                                                                        ),
                                                                        clo.env.clone(),
                                                                        address_builder
                                                                            .init_kaddr(),
                                                                        Time::init(),
                                                                    );
                                                                        v_new.push(
                                                                            new_proc_state_two,
                                                                        );

                                                                        mailboxes.inner.insert(
                                                                            new_pid,
                                                                            Mailbox::init(),
                                                                        );
                                                                    }
                                                                    _ => panic!(),
                                                                }
                                                            }
                                                            _ => panic!(),
                                                        }
                                                    }
                                                    _ => panic!(),
                                                }
                                            }
                                            _ => panic!(),
                                        }
                                    }
                                }
                                "!" => panic!(),
                                _ => panic!(),
                            },

                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                    // NOTE look up module field in call
                }
                // ABS_LETREC
                TypedCore::LetRec(_let_rec) => {
                    panic!();
                }
                // ABS_CASE
                TypedCore::Case(case) => {
                    let clauses = Vec::from(&case.clauses);
                    let values;
                    match &*case.arg {
                        TypedCore::Var(v) => {
                            values = store
                                .value
                                .get(&self.env.inner.get(&VarName::from(v)).unwrap())
                                .unwrap()
                        }
                        TypedCore::Literal(l) => match *l.val {
                            TypedCore::AstList(al) => {}
                            TypedCore::String(_) => {
                                let var_name = VarName::from(&*l.val);
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }

                    let mats = Case::cmatch(&clauses, values, &store.value, ast_helper);

                    for mat in mats {
                        if let Some((index, env)) = mat {
                            let mut new_item = self.clone();
                            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(
                                (*(clauses[index].body)).get_index().unwrap(),
                            );
                            new_item.env.merge_with(&env);

                            v_new.push(new_item);

                            // stop after first match
                            break;
                        }
                    }
                }
                // ABS_RECEIVE
                TypedCore::Receive(r) => {
                    let mailbox = mailboxes.inner.get(&self.pid).unwrap();
                    let msgs = mailbox.mmatch(&Vec::from(&r.clauses), &store.value, ast_helper);

                    panic!();
                }
                TypedCore::PrimOp(_prim_op) => {
                    // NOTE This would require another
                    // match on the name. However, the PrimOps: self, spawn and send are the only
                    // ones being considered, so parsing them in the ast module would probably be
                    // more sensible

                    // ABS_SELF
                    // ABS_SPAWN
                    // ABS_SEND
                    panic!();
                }
                // TODO ABS_PUSH_DO
                // ABS_PUSH_LET
                TypedCore::Let(l) => {
                    let mut new_item = self.clone();
                    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc((*l.arg).get_index().unwrap());
                    new_item.k_addr = address_builder.new_kaddr(
                        &self,
                        &new_item.prog_loc_or_pid,
                        &new_item.env,
                        &new_item.time,
                    );

                    let kont = Kont::Let(
                        (&l.vars).into(),
                        (*l.body).get_index().unwrap(),
                        self.env.clone(),
                        self.k_addr.clone(),
                    );

                    v_revisit.append(&mut Analyzer::push_to_kont_store(
                        &ast_helper,
                        &seen,
                        store,
                        new_item.k_addr.clone(),
                        kont,
                    ));
                    v_new.push(new_item);
                }
                // ProgLoc is irreducible via the previous transition rules; it's a Value
                // We need to look at the continuation for the next computation
                _ => match store.kont.get(&self.k_addr) {
                    Some(konts) => {
                        let konts = konts.clone();
                        // consider each possible continuation
                        for kont in konts {
                            match kont {
                                Kont::Let(var_list, body, env, k_addr) => {
                                    //TODO ABS_POP_LET_VALUEADDR
                                    match &self.prog_loc_or_pid {
                                        // ABS_POP_LET_PID
                                        ProgLocOrPid::Pid(_pid) => {
                                            panic!();
                                        }
                                        ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                                            // ABS_POP_LET_VALUELIST
                                            TypedCore::AstList(_al) => {
                                                panic!();
                                            }
                                            // ABS_POP_LET_CLOSURE
                                            _ => {
                                                if var_list.len() != 1 {
                                                    panic!();
                                                }

                                                let mut new_item = self.clone();
                                                new_item.prog_loc_or_pid =
                                                    ProgLocOrPid::ProgLoc(body);
                                                new_item.env = env.clone();
                                                new_item.k_addr = k_addr.clone();
                                                let new_var_name =
                                                    VarName::from(ast_helper.get(var_list[0]));
                                                let new_v_addr = address_builder.new_vaddr(
                                                    &self,
                                                    &new_var_name,
                                                    &new_item.prog_loc_or_pid,
                                                    &new_item.env,
                                                    &new_item.time,
                                                );
                                                new_item.env.inner.insert(
                                                    new_var_name.clone(),
                                                    new_v_addr.clone(),
                                                );

                                                v_revisit.append(
                                                    &mut Analyzer::push_to_value_store(
                                                        &ast_helper,
                                                        &seen,
                                                        store,
                                                        new_v_addr,
                                                        Value::Closure(Closure {
                                                            prog_loc: *pl,
                                                            env: self.env.clone(),
                                                        }),
                                                    ),
                                                );
                                                v_new.push(new_item);
                                            }
                                        },
                                    }
                                }
                                // ABS_POP_DO
                                Kont::Do(_body, _env, _k_addr) => {
                                    panic!();
                                }
                                Kont::Stop => {
                                    // NOTE (successful)
                                }
                            }
                        }
                    }
                    None => {
                        // NOTE (fail)
                        panic!();
                    }
                },
            },
        };
        (v_new, v_revisit)
    }
}
