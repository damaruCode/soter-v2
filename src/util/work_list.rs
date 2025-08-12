use crate::util::AstHelper;
use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

pub trait WorkItem: Eq + Hash + Clone {
    fn process(&self, ast_helper: &AstHelper) -> (Vec<Self>, Vec<Self>);
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

    pub fn run(&mut self, ast_helper: &AstHelper) {
        // This terminates because it assumes a fixpoint implementation
        while let Some(item) = self.queue.pop_front() {
            let (new_items, revisit_items) = item.process(ast_helper);
            for item in revisit_items {
                self.queue.push_back(item);
            }

            for item in new_items {
                // NOTE cloning here might become a memory issue
                if self.seen.insert(item.clone()) {
                    self.queue.push_back(item);
                }
            }
        }
    }
}
