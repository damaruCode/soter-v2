use crate::ast::Index;
use crate::ast::TypedCore;
use std::collections::HashMap;

#[derive(Debug)]
pub struct AstHelper<'helper> {
    lookup_core: HashMap<usize, &'helper TypedCore>,
    next_id: usize,
}

impl<'helper> AstHelper<'helper> {
    pub fn new() -> Self {
        AstHelper {
            lookup_core: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn get(&self, index: usize) -> &'helper TypedCore {
        self.lookup_core[&index]
    }

    pub fn build_indecies(&mut self, mut root: TypedCore) -> TypedCore {
        fn visit<'a>(node: &mut TypedCore, ctx: &mut AstHelper<'a>) {
            let id = ctx.next_id;
            ctx.next_id += 1;
            match node {
                TypedCore::AstTuple(t) => {
                    t.index = Some(id);
                    visit(&mut *t.frst, ctx);
                    visit(&mut *t.scnd, ctx);
                }
                TypedCore::AstList(l) => {
                    l.index = Some(id);
                    for child in &mut *l.inner {
                        visit(child, ctx);
                    }
                }
                TypedCore::Alias(a) => {
                    a.index = Some(id);
                    visit(&mut *a.var, ctx);
                    visit(&mut *a.pat, ctx);
                }
                TypedCore::Apply(a) => {
                    a.index = Some(id);
                    visit(&mut *a.op, ctx);
                    for x in &mut *a.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Binary(b) => {
                    b.index = Some(id);
                    for x in &mut *b.segments.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::BitStr(bs) => {
                    bs.index = Some(id);
                    visit(&mut *bs.val, ctx);
                    visit(&mut *bs.size, ctx);
                    visit(&mut *bs.unit, ctx);
                    visit(&mut *bs.r#type, ctx);
                    visit(&mut *bs.flags, ctx);
                }
                TypedCore::Call(c) => {
                    c.index = Some(id);
                    visit(&mut *c.module, ctx);
                    visit(&mut *c.name, ctx);
                    for x in &mut *c.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Case(c) => {
                    c.index = Some(id);
                    visit(&mut *c.arg, ctx);
                    for x in &mut *c.clauses.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Catch(c) => {
                    c.index = Some(id);
                    visit(&mut *c.body, ctx);
                }
                TypedCore::Clause(c) => {
                    c.index = Some(id);
                    for x in &mut *c.pats.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *c.guard, ctx);
                    visit(&mut *c.body, ctx);
                }
                TypedCore::Cons(c) => {
                    c.index = Some(id);
                    visit(&mut *c.hd, ctx);
                    visit(&mut *c.tl, ctx);
                }
                TypedCore::Fun(f) => {
                    f.index = Some(id);
                    for x in &mut *f.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *f.body, ctx);
                }
                TypedCore::Let(l) => {
                    l.index = Some(id);
                    for x in &mut *l.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *l.arg, ctx);
                    visit(&mut *l.body, ctx);
                }
                TypedCore::LetRec(lr) => {
                    lr.index = Some(id);
                    for tuple in &mut *lr.defs.inner {
                        visit(&mut *tuple.frst, ctx);
                        visit(&mut *tuple.scnd, ctx);
                    }
                    visit(&mut *lr.body, ctx);
                }
                TypedCore::Literal(l) => {
                    l.index = Some(id);
                    visit(&mut *l.val, ctx);
                }
                TypedCore::Map(m) => {
                    m.index = Some(id);
                    visit(&mut *m.arg, ctx);
                    for x in &mut *m.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::MapPair(mp) => {
                    mp.index = Some(id);
                    visit(&mut *mp.op, ctx);
                    visit(&mut *mp.key, ctx);
                    visit(&mut *mp.val, ctx);
                }
                TypedCore::Module(m) => {
                    m.index = Some(id);
                    visit(&mut *m.name, ctx);
                    for x in &mut *m.exports.inner {
                        visit(x, ctx);
                    }
                    for tuple in &mut *m.attrs.inner {
                        visit(&mut *tuple.frst, ctx);
                        visit(&mut *tuple.scnd, ctx);
                    }
                    for tuple in &mut *m.defs.inner {
                        visit(&mut *tuple.frst, ctx);
                        visit(&mut *tuple.scnd, ctx);
                    }
                }
                TypedCore::Opaque(o) => {
                    o.index = Some(id);
                    visit(&mut *o.val, ctx);
                }
                TypedCore::PrimOp(p) => {
                    p.index = Some(id);
                    visit(&mut *p.name, ctx);
                    for x in &mut *p.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Receive(r) => {
                    r.index = Some(id);
                    for x in &mut *r.clauses.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *r.timeout, ctx);
                    visit(&mut *r.action, ctx);
                }
                TypedCore::Seq(s) => {
                    s.index = Some(id);
                    visit(&mut *s.arg, ctx);
                    visit(&mut *s.body, ctx);
                }
                TypedCore::Try(t) => {
                    t.index = Some(id);
                    visit(&mut *t.arg, ctx);
                    for x in &mut *t.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *t.body, ctx);
                    for x in &mut *t.evars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *t.handler, ctx);
                }
                TypedCore::Tuple(t) => {
                    t.index = Some(id);
                    for x in &mut *t.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Values(v) => {
                    v.index = Some(id);
                    for x in &mut *v.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Var(v) => {
                    v.index = Some(id);
                    visit(&mut *v.name, ctx);
                }
                TypedCore::Null(n) => {
                    n.index = Some(id);
                }
                TypedCore::Bool(b) => {
                    b.index = Some(id);
                }
                TypedCore::Number(n) => {
                    n.index = Some(id);
                }
                TypedCore::String(s) => {
                    s.index = Some(id);
                }
            }
        }
        visit(&mut root, self);
        root
    }

    pub fn build_lookup(&mut self, root: &'helper TypedCore) {
        fn visit<'a>(node: &'a TypedCore, ctx: &mut AstHelper<'a>) {
            let id = node.get_index().unwrap();
            ctx.lookup_core.insert(id, node);
            match node {
                TypedCore::AstTuple(t) => {
                    visit(&t.frst, ctx);
                    visit(&t.scnd, ctx);
                }
                TypedCore::AstList(l) => {
                    for child in &l.inner {
                        visit(child, ctx);
                    }
                }
                TypedCore::Alias(a) => {
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
                TypedCore::Map(m) => {
                    visit(&m.arg, ctx);
                    for x in &m.es.inner {
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
                TypedCore::Null(_) => {}
                TypedCore::Bool(_) => {}
                TypedCore::Number(_) => {}
                TypedCore::String(_) => {}
            }
        }
        visit(root, self);
    }
}
