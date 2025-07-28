use std::{fmt::Debug, marker::PhantomData};

use crate::abstract_state_space::ProcState;

pub trait State<'a>: Clone + Debug + PartialEq {}
impl<'a, T: Clone + Debug + PartialEq> State<'a> for T {}

pub trait Transition<'a, S: State<'a>> {
    type Error;

    fn try_apply(&self, s: &S) -> Result<S, Self::Error>;

    fn is_valid(&self, s: &S) -> bool;
}
pub struct TypedTransition<'a, S, F>
where
    S: State<'a>,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    transition_fn: F,
    _state: PhantomData<&'a S>,
}
impl<'a, S, F> TypedTransition<'a, S, F>
where
    S: State<'a>,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    fn new(transition_fn: F) -> Self {
        Self {
            transition_fn,
            _state: PhantomData,
        }
    }
}
impl<'a, S, F> Transition<'a, S> for TypedTransition<'a, S, F>
where
    S: State<'a>,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    type Error = TransitionError;

    fn try_apply(&self, s: &S) -> Result<S, Self::Error> {
        (self.transition_fn)(s)
    }

    fn is_valid(&self, s: &S) -> bool {
        match (self.transition_fn)(s) {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

pub struct TransitionSystem<'a, S, F>
where
    S: State<'a>,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    transitions: Vec<TypedTransition<'a, S, F>>,
}
impl<'a, S, F> TransitionSystem<'a, S, F>
where
    S: State<'a>,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    pub fn init() -> Self {
        Self {
            transitions: Vec::new(),
        }
    }

    pub fn register_transition(&mut self, transition: TypedTransition<'a, S, F>) {
        self.transitions.push(transition);
    }

    fn try_apply(&self, s: &S) -> Result<S, TransitionError> {
        for transition in &self.transitions {
            if transition.is_valid(s) {
                match transition.try_apply(s) {
                    Ok(new_state) => {
                        return Ok(new_state);
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        Err(TransitionError::InvalidTransition)
    }
}

pub enum TransitionError {
    InvalidTransition,
}
