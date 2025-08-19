use std::fmt::Display;

use crate::ast::AstList;
use crate::ast::TypedCore;
use crate::ast::Var;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum VarName {
    Atom(String),
    Number(u128),
    FnAtom(String, u128),
}

impl From<&TypedCore> for VarName {
    fn from(tc: &TypedCore) -> Self {
        match tc {
            TypedCore::Var(v) => VarName::from(&*v.name),
            TypedCore::String(s) => VarName::Atom(s.inner.clone()),
            TypedCore::Number(n) => VarName::Number(n.inner.as_u128().unwrap()),
            TypedCore::AstList(al) => {
                let s = match &al.inner[0] {
                    TypedCore::String(s) => s.inner.clone(),
                    _ => panic!(),
                };
                let n = match &al.inner[1] {
                    TypedCore::Number(n) => n.inner.as_u128().unwrap(),
                    _ => panic!(),
                };
                VarName::FnAtom(s, n)
            }
            _ => panic!("{:#?}", tc),
        }
    }
}

impl From<&Var> for VarName {
    fn from(var: &Var) -> Self {
        VarName::from(&*var.name)
    }
}

impl From<&AstList<TypedCore>> for Vec<VarName> {
    fn from(var_list: &AstList<TypedCore>) -> Self {
        let mut var_names = Vec::new();
        for tc in &var_list.inner {
            var_names.push(VarName::from(tc));
        }

        var_names
    }
}

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Atom(s) => write!(f, "'{}'", s),
            Self::Number(n) => write!(f, "{}", n),
            Self::FnAtom(s, n) => write!(f, "'{}'/{}", s, n),
        }
    }
}
