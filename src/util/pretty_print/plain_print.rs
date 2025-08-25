use crate::ast::TypedCore;

pub fn print(tc: &TypedCore) -> String {
    match tc {
        TypedCore::AstList(al) => {
            let mut strings = Vec::new();
            for elem in &al.inner {
                strings.push(print(elem));
            }
            format!("[{}]", strings.join(","))
        }
        TypedCore::AstTuple(at) => {
            format!("{{{}, {}}}", print(&*at.frst), print(&*at.scnd))
        }
        TypedCore::String(s) => s.inner.clone(),
        _ => todo!(),
    }
}
