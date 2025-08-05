use super::{Data, Env, Kont, KontinuationAddress, Pid, ProgLoc, Time, Value, ValueAddress};

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum ProgLocOrPid<'a> {
    ProgLoc(ProgLoc<'a>),
    Pid(Pid<'a>),
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ProcState<'a, K: KontinuationAddress, V: ValueAddress> {
    pub pid: Pid<'a>,
    pub prog_loc_or_pid: ProgLocOrPid<'a>,
    pub env: Env<'a, V>,
    pub k_addr: K,
    pub time: Time<'a>,

    visited_data: Vec<Data<'a>>,
    visited_values: Vec<Value<'a, V>>,
    visited_konts: Vec<Kont<'a, K, V>>,
}

impl<'a, K: KontinuationAddress, V: ValueAddress> ProcState<'a, K, V> {
    pub fn new(
        pid: Pid<'a>,
        prog_loc_or_pid: ProgLocOrPid<'a>,
        env: Env<'a, V>,
        k_addr: K,
        time: Time<'a>,
    ) -> Self {
        ProcState {
            pid,
            prog_loc_or_pid,
            env,
            k_addr,
            time,

            visited_data: Vec::new(),
            visited_values: Vec::new(),
            visited_konts: Vec::new(),
        }
    }

    pub fn init(prog_loc: ProgLoc<'a>) -> Self {
        ProcState {
            pid: Pid::init(prog_loc.clone()),
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(prog_loc.clone()),
            env: Env::init(),
            k_addr: K::init(prog_loc.clone()),
            time: Time::init(),

            visited_data: Vec::new(),
            visited_values: Vec::new(),
            visited_konts: Vec::new(),
        }
    }

    pub fn has_visited_data(&self, data: &Data) -> bool {
        self.visited_data.contains(data)
    }

    pub fn has_visited_value(&self, value: &Value<V>) -> bool {
        self.visited_values.contains(value)
    }

    pub fn has_visited_kontinuation(&self, kont: &Kont<K, V>) -> bool {
        self.visited_konts.contains(kont)
    }
}
