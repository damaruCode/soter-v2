use super::{Data, Env, KAddr, Kont, Pid, ProgLoc, Time, Value};

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

    visited_data: Vec<Data<'a>>,
    visited_values: Vec<Value<'a>>,
    visited_konts: Vec<Kont<'a>>,
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
            k_addr: KAddr::init(prog_loc.clone()),
            time: Time::init(),

            visited_data: Vec::new(),
            visited_values: Vec::new(),
            visited_konts: Vec::new(),
        }
    }

    pub fn has_visited_data(&self, data: &Data) -> bool {
        self.visited_data.contains(data)
    }

    pub fn has_visited_value(&self, value: &Value) -> bool {
        self.visited_values.contains(value)
    }

    pub fn has_visited_kontinuation(&self, kont: &Kont) -> bool {
        self.visited_konts.contains(kont)
    }
}
