use super::{Env, KAddr, Pid, ProgLoc, Time};

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum ProgLocOrPid<'a> {
    ProgLoc(ProgLoc<'a>),
    Pid(Pid<'a>),
}
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ProcState<'a> {
    pub pid: Pid<'a>,
    pub prog_loc_or_pid: ProgLocOrPid<'a>,
    pub env: Env<'a>,
    pub k_addr: KAddr<'a>,
    pub time: Time<'a>,
}
impl<'a> ProcState<'a> {
    pub fn new(
        pid: Pid<'a>,
        prog_loc_or_pid: ProgLocOrPid<'a>,
        env: Env<'a>,
        k_addr: KAddr<'a>,
        time: Time<'a>,
    ) -> Self {
        ProcState {
            pid,
            prog_loc_or_pid,
            env,
            k_addr,
            time,
        }
    }
    pub fn init(prog_loc: ProgLoc<'a>) -> Self {
        ProcState {
            pid: Pid::init(prog_loc.clone()),
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc.clone()),
            env: Env::init(),
            k_addr: KAddr::init(prog_loc.clone()),
            time: Time::init(),
        }
    }

    // TODO
    // fn step(&self, pid: &Pid<'a>, store: &mut Store<'a>) -> Self {
    //     match self.prog_loc_or_pid {
    //         ProgLocOrPid::Pid(ref pid) => self.clone(),
    //         ProgLocOrPid::ProgLoc(ref prog_loc) => match prog_loc.inner {
    //             // NOTE could have more than one def
    //             TypedCore::Module(module) => match &*module.defs.inner[0].scnd {
    //                 TypedCore::Fun(fun) => match &*fun.body {
    //                     // NOTE could have more clauses than one
    //                     TypedCore::Case(case) => match &case.clauses.inner[0] {
    //                         TypedCore::Clause(clause) => {
    //                             return ProcState::new(
    //                                 // NOTE the pattern and guard of the clause could be
    //                                 // interesting in general
    //                                 ProgLocOrPid::ProgLoc(ProgLoc::new(&clause.body)),
    //                                 self.env.clone(),
    //                                 self.k_addr.clone(),
    //                                 self.time.clone(),
    //                             );
    //                         }
    //                         _ => self.clone(),
    //                     },
    //                     _ => self.clone(),
    //                 },
    //                 _ => self.clone(),
    //             },
    //             TypedCore::Let(l) => {
    //                 // TODO handle the .expect properly (in a standard way)
    //                 let var_list =
    //                     Vec::<VarInner>::try_from(&l.vars).expect("Forked up conversion");
    //                 let arg = &l.arg;
    //                 let body = &l.body;

    //                 // Push-Let
    //                 let k_let = Kont::Let(
    //                     var_list,
    //                     ProgLoc { inner: body },
    //                     self.env.clone(),
    //                     self.k_addr.clone(),
    //                 );

    //                 let k_addr = KAddr::new(
    //                     pid.clone(),
    //                     prog_loc.clone(),
    //                     self.env.clone(),
    //                     self.time.clone(),
    //                 );

    //                 store.kont.push(k_addr.clone(), k_let);

    //                 return ProcState::new(
    //                     ProgLocOrPid::ProgLoc(ProgLoc::new(arg)),
    //                     self.env.clone(),
    //                     k_addr,
    //                     self.time.clone(),
    //                 );
    //             }
    //             TypedCore::Fun(f) => {
    //                 // NOTE think through what the proper handling of this option type should be in
    //                 // this scenario
    //                 let konts = store.kont.get(&self.k_addr);
    //                 match konts {
    //                     Some(set) => {
    //                         // NOTE need to consider each possible continuation
    //                         // non-deterministically
    //                     }
    //                     None => panic!("Dafuq"),
    //                 }

    //                 self.clone()
    //             }
    //             _ => self.clone(),
    //         },
    //     }
    // }
}
