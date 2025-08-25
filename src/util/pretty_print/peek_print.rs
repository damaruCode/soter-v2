use crate::{
    ast::{Index, TypedCore},
    state_space::VarName,
};

fn peek_name(tc: &TypedCore) -> String {
    format!(
        "{}",
        match tc {
            TypedCore::Let(_) => String::from("let ..."),
            TypedCore::Seq(_) => String::from("seq ..."),
            TypedCore::Fun(_) => String::from("fun (...)"),
            TypedCore::Receive(_) => String::from("receive ..."),
            TypedCore::Var(v) => VarName::from(v).to_string(),
            TypedCore::Apply(_) => String::from("apply ..."),
            TypedCore::String(s) => s.inner.clone(),
            TypedCore::Map(_) => String::from("map ..."),
            TypedCore::Try(_) => String::from("try ..."),
            TypedCore::Null(_) => String::from("null"),
            TypedCore::Bool(b) => b.inner.to_string(),
            TypedCore::Call(_) => String::from("call ..."),
            TypedCore::Case(c) => format!(
                "case <{}>\n\t{}\nend",
                peek_name(&*c.arg),
                c.clauses
                    .inner
                    .iter()
                    .map(|e| peek_name(e))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            TypedCore::Cons(_) => String::from("cons ..."),
            TypedCore::Alias(_) => String::from("alias ..."),
            TypedCore::Catch(_) => String::from("catch ..."),
            TypedCore::Tuple(tup) => format!(
                "{{{}}}",
                tup.es
                    .inner
                    .iter()
                    .map(|e| peek_name(e))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedCore::Number(n) => n.inner.to_string(),
            TypedCore::Binary(b) => b.to_string(),
            TypedCore::BitStr(b) => b.to_string(),
            TypedCore::Clause(c) => format!(
                "clause <{}> when {} -> {}",
                c.pats
                    .inner
                    .iter()
                    .map(|e| peek_name(e))
                    .collect::<Vec<String>>()
                    .join(", "),
                peek_name(&*c.guard),
                peek_name(&*c.body)
            ),
            TypedCore::LetRec(_) => String::from("letrec ..."),
            TypedCore::Module(_) => String::from("module ..."),
            TypedCore::Opaque(_) =>
                String::from("If you see this, you are seeing ghosts --- congrats!"),
            TypedCore::PrimOp(_) => String::from("primop ..."),
            TypedCore::Values(_) => String::from("values ..."),
            TypedCore::AstList(al) => format!(
                "[{}]",
                al.inner
                    .iter()
                    .map(|e| peek_name(e))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedCore::Literal(l) => {
                peek_name(&*l.val)
            }
            TypedCore::AstTuple(at) =>
                format!("{{{}, {}}}", peek_name(&*at.frst), peek_name(&*at.scnd)),
            TypedCore::MapPair(_) => String::from("mappair ..."),
        }
    )
}

pub fn print(tc: &TypedCore) -> String {
    format!(
        "{} : {}",
        tc.get_index().unwrap(),
        match tc {
            TypedCore::Module(module) => format!("module {} [...] ...", peek_name(&*module.name)),
            TypedCore::Apply(apply) => format!(
                "apply {} ({})",
                peek_name(&*apply.op),
                apply
                    .args
                    .inner
                    .iter()
                    .map(|e| peek_name(e))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedCore::Seq(seq) => {
                format!("{}, {}", peek_name(&*seq.arg), peek_name(&*seq.body))
            }
            TypedCore::Let(clet) => format!(
                "let <{}> = {} in {}",
                clet.vars
                    .inner
                    .iter()
                    .map(|v| peek_name(v))
                    .collect::<Vec<String>>()
                    .join(", "),
                peek_name(&*clet.arg),
                peek_name(&*clet.body)
            ),
            TypedCore::Call(call) => format!(
                "{}:{}({})",
                peek_name(&*call.module),
                peek_name(&*call.name),
                call.args
                    .inner
                    .iter()
                    .map(|v| peek_name(v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            tc => peek_name(tc),
        }
    )
}
