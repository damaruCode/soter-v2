use serde_json::Number;

use crate::ast::{AstList, TypedCore};

/* AST Helper
 * ----------
 * Working with the AST defined in ast.rs can be tedious; The helper enforces some common sense
 * relations between the constructs of the AST
 */

/// Represents an unexpected type during conversion
#[derive(Debug)]
pub struct ConversionError;

/// Represents the possible types of var names
#[derive(Debug, Clone)]
pub enum VarInner {
    String(String),
    Number(Number),
}
impl TryFrom<&TypedCore> for VarInner {
    type Error = ConversionError;

    fn try_from(tc: &TypedCore) -> Result<Self, Self::Error> {
        match tc {
            TypedCore::Var(v) => VarInner::try_from(&*v.name),
            TypedCore::String(s) => Ok(VarInner::String(s.clone())),
            TypedCore::Number(n) => Ok(VarInner::Number(n.clone())),
            _ => Err(ConversionError),
        }
    }
}

impl TryFrom<&AstList<TypedCore>> for Vec<VarInner> {
    type Error = ConversionError;

    fn try_from(al: &AstList<TypedCore>) -> Result<Self, Self::Error> {
        al.inner // take the actual vector
            .iter() // turn it into an iterable
            /* and do a fold on it with return type Result<Vec<VarInner>, ConversionError> */
            .fold(Ok(Vec::new()) /* base case is Ok( ) on the empty list */, |acc /* accumulator */, curr_tc /* current element of iterable */| match acc {
                // if the accumulator is Ok( ) until now, continue conversion
                Ok(mut vec) => match VarInner::try_from(curr_tc) {
                    // if successful, add the VarInner to the vector inside the accumulator
                    Ok(var_inner) => {
                        vec.push(var_inner);
                        Ok(vec)
                    }
                    // otherwise pass the ConversionError along
                    Err(e) => Err(e),
                },
                // otherwise stop conversion by passing the ConversionError along
                Err(e) => Err(e),
            })
    }
}
