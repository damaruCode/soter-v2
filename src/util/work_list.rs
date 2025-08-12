use std::collections::{HashSet, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;

pub trait WorkItem<'a>: Eq + Hash + Clone {
    fn process(&'a self) -> (Vec<Self>, Vec<Self>);
}

pub struct WorkList<'a, T: WorkItem<'a>> {
    queue: VecDeque<T>,
    seen: HashSet<T>,
    phantom_data: PhantomData<&'a T>,
}

impl<'a, T: WorkItem<'a>> WorkList<'a, T> {
    pub fn new(initial: Vec<T>) -> Self {
        let mut queue = VecDeque::new();
        let mut seen = HashSet::new();
        for item in initial {
            // NOTE cloning here might become a memory issue
            seen.insert(item.clone());
            queue.push_back(item);
        }
        Self {
            queue,
            seen,
            phantom_data: PhantomData,
        }
    }

    pub fn run(&mut self) {
        // This terminates because it assumes a fixpoint implementation
        while let Some(item) = self.queue.pop_front() {
            let (new_items, revisit_items) = item.process();
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
