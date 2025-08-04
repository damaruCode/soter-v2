use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

pub trait WorkItem: Eq + Hash + Clone {
    fn process(&self) -> Vec<Self>;
}

pub struct WorkList<T: WorkItem> {
    queue: VecDeque<T>,
    seen: HashSet<T>,
}

impl<T: WorkItem> WorkList<T> {
    pub fn new(initial: Vec<T>) -> Self {
        let mut queue = VecDeque::new();
        let mut seen = HashSet::new();
        for item in initial {
            // NOTE cloning here might become a memory issue
            seen.insert(item.clone());
            queue.push_back(item);
        }
        Self { queue, seen }
    }

    pub fn run(&mut self) {
        while let Some(item) = self.queue.pop_front() {
            let new_items = item.process();
            for new_item in new_items {
                // NOTE cloning here might become a memory issue
                if self.seen.insert(new_item.clone()) {
                    self.queue.push_back(new_item);
                }
            }
        }
    }
}
