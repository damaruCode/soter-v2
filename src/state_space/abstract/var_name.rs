use crate::ast::TypedCore;

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
