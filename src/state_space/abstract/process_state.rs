use super::{Data, Env, Kont, KontinuationAddress, Pid, Time, Value, ValueAddress};

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ProgLocOrPid {
    ProgLoc(usize),
    Pid(Pid),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProcState<K: KontinuationAddress, V: ValueAddress> {
    pub pid: Pid,
    pub prog_loc_or_pid: ProgLocOrPid,
    pub env: Env<V>,
    pub k_addr: K,
    pub time: Time,

    visited_data: Vec<Data>,
    visited_values: Vec<Value<V>>,
    visited_konts: Vec<Kont<K, V>>,
}

impl<K: KontinuationAddress, V: ValueAddress> ProcState<K, V> {
    pub fn new(
        pid: Pid,
        prog_loc_or_pid: ProgLocOrPid,
        env: Env<V>,
        k_addr: K,
        time: Time,
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

    pub fn init(init_k_addr: K) -> Self {
        ProcState {
            pid: Pid::init(),
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(0),
            env: Env::init(),
            k_addr: init_k_addr,
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
