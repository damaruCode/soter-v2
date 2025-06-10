use crate::abstract_state_space;
use crate::ast::TypedCore;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn enumerate(tc: TypedCore) {
    let output = match tc {
        TypedCore::Null => "",
        _ => todo!(),
    };

    dbg!(output);
    dbg!(&tc);
}

pub fn inital_state(tc: TypedCore) {}
