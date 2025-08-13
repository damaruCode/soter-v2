use crate::ast::TypedCore;
use std::collections::HashMap;

#[derive(Debug)]
pub struct AstHelper<'helper> {
    lookup: HashMap<usize, &'helper TypedCore>,
    next_id: usize,
}

impl<'helper> AstHelper<'helper> {
    pub fn new() -> Self {
        AstHelper {
            lookup: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn get(&self, index: usize) -> &'helper TypedCore {
        self.lookup[&index]
    }

    pub fn build_lookup(&mut self, root: &'helper TypedCore) {
        fn visit<'a>(node: &'a TypedCore, ctx: &mut AstHelper<'a>) {
            let id = ctx.next_id;
            ctx.next_id += 1;
            ctx.lookup.insert(id, node);

            match node {
                TypedCore::AstTuple(t) => {
                    visit(&t.frst, ctx);
                    visit(&t.scnd, ctx);
                }
                TypedCore::AstList(list) => {
                    for child in &list.inner {
                        visit(child, ctx);
                    }
                }
                TypedCore::Alias(a) => {
                    visit(&a.var, ctx);
                    visit(&a.pat, ctx);
                }
                TypedCore::Apply(a) => {
                    visit(&a.op, ctx);
                    for x in &a.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Binary(b) => {
                    for x in &b.segments.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::BitStr(bs) => {
                    visit(&bs.val, ctx);
                    visit(&bs.size, ctx);
                    visit(&bs.unit, ctx);
                    visit(&bs.r#type, ctx);
                    visit(&bs.flags, ctx);
                }
                TypedCore::Call(c) => {
                    visit(&c.module, ctx);
                    visit(&c.name, ctx);
                    for x in &c.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Case(c) => {
                    visit(&c.arg, ctx);
                    for x in &c.clauses.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Catch(c) => {
                    visit(&c.body, ctx);
                }
                TypedCore::Clause(c) => {
                    for x in &c.pats.inner {
                        visit(x, ctx);
                    }
                    visit(&c.body, ctx);
                    visit(&c.guard, ctx);
                }
                TypedCore::Cons(c) => {
                    visit(&c.hd, ctx);
                    visit(&c.tl, ctx);
                }
                TypedCore::Fun(f) => {
                    for x in &f.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&f.body, ctx);
                }
                TypedCore::Let(l) => {
                    for x in &l.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&l.arg, ctx);
                    visit(&l.body, ctx);
                }
                TypedCore::LetRec(lr) => {
                    for tuple in &lr.defs.inner {
                        visit(&tuple.frst, ctx);
                        visit(&tuple.scnd, ctx);
                    }
                    visit(&lr.body, ctx);
                }
                TypedCore::Literal(l) => {
                    visit(&l.val, ctx);
                }
                TypedCore::LiteralMap(lm) => {
                    visit(&lm.arg, ctx);
                    for x in &lm.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::VarMap(vm) => {
                    visit(&vm.arg, ctx);
                    for x in &vm.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::MapPair(mp) => {
                    visit(&mp.op, ctx);
                    visit(&mp.key, ctx);
                    visit(&mp.val, ctx);
                }
                TypedCore::Module(m) => {
                    visit(&m.name, ctx);
                    for x in &m.exports.inner {
                        visit(x, ctx);
                    }
                    for tuple in &m.attrs.inner {
                        visit(&tuple.frst, ctx);
                        visit(&tuple.scnd, ctx);
                    }
                    for tuple in &m.defs.inner {
                        visit(&tuple.frst, ctx);
                        visit(&tuple.scnd, ctx);
                    }
                }
                TypedCore::Opaque(o) => {
                    visit(&o.val, ctx);
                }
                TypedCore::PrimOp(p) => {
                    visit(&p.name, ctx);
                    for x in &p.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Receive(r) => {
                    for x in &r.clauses.inner {
                        visit(x, ctx);
                    }
                    visit(&r.timeout, ctx);
                    visit(&r.action, ctx);
                }
                TypedCore::Seq(s) => {
                    visit(&s.arg, ctx);
                    visit(&s.body, ctx);
                }
                TypedCore::Try(t) => {
                    visit(&t.arg, ctx);
                    for x in &t.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&t.body, ctx);
                    for x in &t.evars.inner {
                        visit(x, ctx);
                    }
                    visit(&t.handler, ctx);
                }
                TypedCore::Tuple(t) => {
                    for x in &t.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Values(v) => {
                    for x in &v.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Var(v) => {
                    visit(&v.name, ctx);
                }
                // Leaf nodes
                TypedCore::Null
                | TypedCore::Bool(_)
                | TypedCore::Number(_)
                | TypedCore::String(_) => {}
            }
        }
        visit(root, self);
    }
}
