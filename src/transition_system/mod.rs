use std::{fmt::Debug, marker::PhantomData};

pub enum TransitionError {
    InvalidTransition,
}

pub trait State<'a>: Clone + Debug + PartialEq {}
impl<'a, T: Clone + Debug + PartialEq> State<'a> for T {}

pub trait Transition<'a, S: State<'a>> {
    type Error;

    fn try_apply(&self, s: &S) -> Result<S, Self::Error>;
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
            match transition.try_apply(s) {
                Ok(new_state) => {
                    return Ok(new_state);
                }
                Err(_e) => {}
            }
        }
        Err(TransitionError::InvalidTransition)
    }
}
