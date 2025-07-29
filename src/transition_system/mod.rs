use std::{fmt::Debug, marker::PhantomData};

pub enum TransitionError {
    ErroneousTransition,
    NoValidTransition,
}

pub trait State: Clone + Debug + PartialEq {}
impl<T: Clone + Debug + PartialEq> State for T {}

pub trait Transition<S: State> {
    type Error;

    fn apply(&self, s: &S) -> Result<S, Self::Error>;
    fn is_valid(&self, s: &S) -> bool;
}

pub struct TypedTransition<S, F>
where
    S: State,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    transition_fn: F,
    _state: PhantomData<S>,
}
impl<S, F> TypedTransition<S, F>
where
    S: State,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    pub fn new(transition_fn: F) -> Self {
        Self {
            transition_fn,
            _state: PhantomData,
        }
    }
}
impl<S, F> Transition<S> for TypedTransition<S, F>
where
    S: State,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    type Error = TransitionError;

    fn apply(&self, s: &S) -> Result<S, Self::Error> {
        (self.transition_fn)(s)
    }

    fn is_valid(&self, s: &S) -> bool {
        (self.transition_fn)(s).is_ok()
    }
}

pub struct TransitionSystem<S, F>
where
    S: State,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    transitions: Vec<TypedTransition<S, F>>,
}
impl<S, F> TransitionSystem<S, F>
where
    S: State,
    F: Fn(&S) -> Result<S, TransitionError>,
{
    pub fn init() -> Self {
        Self {
            transitions: Vec::new(),
        }
    }

    pub fn register_transition(&mut self, transition: TypedTransition<S, F>) {
        self.transitions.push(transition);
    }

    pub fn try_apply(&self, s: &S) -> Result<S, TransitionError> {
        for transition in &self.transitions {
            match transition.apply(s) {
                Ok(new_state) => {
                    return Ok(new_state);
                }
                Err(e) => return Err(e),
            }
        }
        Err(TransitionError::NoValidTransition)
    }
}
