use crate::ast::Index;
use crate::ast::MaybeIndex;
use crate::ast::TypedCore;
use crate::ast::Var;
use crate::state_space::VarName;
use std::collections::BTreeMap;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<BTreeMap<VarName, usize>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        scopes.push(BTreeMap::new()); // initial scope (modules)
        Self { scopes }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, original_var_name: VarName, var_id: usize) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(original_var_name, var_id);
    }

    pub fn lookup(&self, var_name: &VarName) -> Option<usize> {
        for scope in &self.scopes {
            if let Some(var_id) = scope.get(var_name) {
                return Some(*var_id);
            }
        }

        return None;
    }
}

#[derive(Debug, Clone)]
pub struct AstHelper<'helper> {
    lookup_core: HashMap<usize, &'helper TypedCore>,
    lookup_var: HashMap<VarName, Vec<&'helper Var>>,
    symbol_table: SymbolTable,
    next_id: usize,
}

impl<'helper> AstHelper<'helper> {
    pub fn new() -> Self {
        AstHelper {
            lookup_core: HashMap::new(),
            lookup_var: HashMap::new(),
            next_id: 0,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn get(&self, index: usize) -> &'helper TypedCore {
        self.lookup_core[&index]
    }

    pub fn get_vars(&self, var_name: &VarName) -> Option<&Vec<&'helper Var>> {
        self.lookup_var.get(var_name)
    }

    pub fn build_indecies(&mut self, mut root: TypedCore) -> TypedCore {
        fn declare_var<'a>(var: &mut Var, ctx: &mut AstHelper<'a>) {
            let var_name = VarName::from(&*var.name);
            let index = match var.index {
                MaybeIndex::Some(id) => id,
                MaybeIndex::None => panic!(),
            };
            var.var_id = MaybeIndex::Some(index);
            ctx.symbol_table.insert(var_name, index);
        }
        fn declare_var_in_pat<'a>(pat: &mut TypedCore, ctx: &mut AstHelper<'a>) {
            match pat {
                TypedCore::Var(v) => declare_var(v, ctx),
                TypedCore::Cons(c) => {
                    for elem in c.iter_mut_collect() {
                        declare_var_in_pat(elem, ctx);
                    }
                }
                TypedCore::Tuple(t) => {
                    for tc in &mut t.es.inner {
                        declare_var_in_pat(tc, ctx);
                    }
                }
                _ => {} // NOTE ignore (not relevant for declarable vars)
            }
        }
        fn visit<'a>(node: &mut TypedCore, ctx: &mut AstHelper<'a>) {
            let id = ctx.next_id;
            ctx.next_id += 1;
            match node {
                TypedCore::AstTuple(t) => {
                    t.index = MaybeIndex::Some(id);
                    visit(&mut *t.frst, ctx);
                    visit(&mut *t.scnd, ctx);
                }
                TypedCore::AstList(l) => {
                    l.index = MaybeIndex::Some(id);
                    for child in &mut *l.inner {
                        visit(child, ctx);
                    }
                }
                TypedCore::Alias(a) => {
                    a.index = MaybeIndex::Some(id);
                    visit(&mut *a.var, ctx);
                    visit(&mut *a.pat, ctx);
                }
                TypedCore::Apply(a) => {
                    a.index = MaybeIndex::Some(id);
                    visit(&mut *a.op, ctx);
                    for x in &mut *a.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Binary(b) => {
                    b.index = MaybeIndex::Some(id);
                    for x in &mut *b.segments.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::BitStr(bs) => {
                    bs.index = MaybeIndex::Some(id);
                    visit(&mut *bs.val, ctx);
                    visit(&mut *bs.size, ctx);
                    visit(&mut *bs.unit, ctx);
                    visit(&mut *bs.r#type, ctx);
                    visit(&mut *bs.flags, ctx);
                }
                TypedCore::Call(c) => {
                    c.index = MaybeIndex::Some(id);
                    visit(&mut *c.module, ctx);
                    visit(&mut *c.name, ctx);
                    for x in &mut *c.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Case(c) => {
                    c.index = MaybeIndex::Some(id);
                    visit(&mut *c.arg, ctx);
                    for x in &mut *c.clauses.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Catch(c) => {
                    c.index = MaybeIndex::Some(id);
                    ctx.symbol_table.begin_scope();
                    visit(&mut *c.body, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Clause(c) => {
                    c.index = MaybeIndex::Some(id);
                    ctx.symbol_table.begin_scope();
                    for x in &mut *c.pats.inner {
                        visit(x, ctx);
                        declare_var_in_pat(x, ctx);
                    }
                    visit(&mut *c.guard, ctx);
                    visit(&mut *c.body, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Cons(c) => {
                    c.index = MaybeIndex::Some(id);
                    visit(&mut *c.hd, ctx);
                    visit(&mut *c.tl, ctx);
                }
                TypedCore::Fun(f) => {
                    f.index = MaybeIndex::Some(id);

                    ctx.symbol_table.begin_scope();
                    for x in &mut *f.vars.inner {
                        visit(x, ctx);

                        match x {
                            TypedCore::Var(v) => declare_var(v, ctx),
                            tc => panic!("Expected variable, found {}", tc),
                        }
                    }
                    visit(&mut *f.body, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Let(l) => {
                    l.index = MaybeIndex::Some(id);
                    ctx.symbol_table.begin_scope();
                    for x in &mut *l.vars.inner {
                        visit(x, ctx);

                        match x {
                            TypedCore::Var(v) => declare_var(v, ctx),
                            tc => panic!("Expected variable, found {}", tc),
                        }
                    }
                    visit(&mut *l.arg, ctx);
                    visit(&mut *l.body, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::LetRec(lr) => {
                    lr.index = MaybeIndex::Some(id);
                    ctx.symbol_table.begin_scope();

                    // one pass for declaration
                    for tuple in &mut *lr.defs.inner {
                        visit(&mut *tuple.frst, ctx);

                        match &mut *tuple.frst {
                            TypedCore::Var(v) => declare_var(v, ctx),
                            tc => panic!("Expected variable, found {}", tc),
                        }
                    }
                    // another for assignment
                    for tuple in &mut *lr.defs.inner {
                        visit(&mut *tuple.scnd, ctx);
                    }

                    visit(&mut *lr.body, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Literal(l) => {
                    l.index = MaybeIndex::Some(id);
                    visit(&mut *l.val, ctx);
                }
                TypedCore::Map(m) => {
                    m.index = MaybeIndex::Some(id);
                    visit(&mut *m.arg, ctx);
                    for x in &mut *m.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::MapPair(mp) => {
                    mp.index = MaybeIndex::Some(id);
                    visit(&mut *mp.op, ctx);
                    visit(&mut *mp.key, ctx);
                    visit(&mut *mp.val, ctx);
                }
                TypedCore::Module(m) => {
                    m.index = MaybeIndex::Some(id);

                    let mod_var_name = VarName::from(&*m.name);
                    ctx.symbol_table.insert(mod_var_name, id);
                    visit(&mut *m.name, ctx);

                    ctx.symbol_table.begin_scope();
                    for x in &mut *m.exports.inner {
                        visit(x, ctx);
                    }
                    for tuple in &mut *m.attrs.inner {
                        visit(&mut *tuple.frst, ctx);
                        visit(&mut *tuple.scnd, ctx);
                    }

                    // Function declarations
                    // ===========================
                    // one pass for var declaration...
                    for tuple in &mut *m.defs.inner {
                        visit(&mut *tuple.frst, ctx);

                        match &mut *tuple.frst {
                            TypedCore::Var(v) => declare_var(v, ctx),
                            _ => panic!(),
                        }
                    }
                    // ... and another for "assignment" to function
                    for tuple in &mut *m.defs.inner {
                        visit(&mut *tuple.scnd, ctx);
                    }
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Opaque(o) => {
                    o.index = MaybeIndex::Some(id);
                    visit(&mut *o.val, ctx);
                }
                TypedCore::PrimOp(p) => {
                    p.index = MaybeIndex::Some(id);
                    visit(&mut *p.name, ctx);
                    for x in &mut *p.args.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Receive(r) => {
                    r.index = MaybeIndex::Some(id);
                    for x in &mut *r.clauses.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *r.timeout, ctx);
                    visit(&mut *r.action, ctx);
                }
                TypedCore::Seq(s) => {
                    s.index = MaybeIndex::Some(id);
                    visit(&mut *s.arg, ctx);
                    // TODO new scope?
                    visit(&mut *s.body, ctx);
                }
                TypedCore::Try(t) => {
                    t.index = MaybeIndex::Some(id);

                    ctx.symbol_table.begin_scope();
                    visit(&mut *t.arg, ctx);
                    for x in &mut *t.vars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *t.body, ctx);
                    for x in &mut *t.evars.inner {
                        visit(x, ctx);
                    }
                    visit(&mut *t.handler, ctx);
                    ctx.symbol_table.end_scope();
                }
                TypedCore::Tuple(t) => {
                    t.index = MaybeIndex::Some(id);
                    for x in &mut *t.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Values(v) => {
                    v.index = MaybeIndex::Some(id);
                    for x in &mut *v.es.inner {
                        visit(x, ctx);
                    }
                }
                TypedCore::Var(v) => {
                    v.index = MaybeIndex::Some(id);
                    v.var_id = match ctx.symbol_table.lookup(&VarName::from(&*v.name)) {
                        Some(var_id) => MaybeIndex::Some(var_id),
                        None => MaybeIndex::None,
                    };

                    visit(&mut *v.name, ctx);
                }
                TypedCore::Null(n) => {
                    n.index = MaybeIndex::Some(id);
                }
                TypedCore::Bool(b) => {
                    b.index = MaybeIndex::Some(id);
                }
                TypedCore::Number(n) => {
                    n.index = MaybeIndex::Some(id);
                }
                TypedCore::String(s) => {
                    s.index = MaybeIndex::Some(id);
                }
                TypedCore::Dummy => {}
                TypedCore::Empty() => {}
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
                    let var_name = &VarName::from(v);
                    if let Some(vals) = ctx.lookup_var.get_mut(var_name) {
                        vals.push(v);
                    } else {
                        ctx.lookup_var.insert(var_name.clone(), vec![v]);
                    }
                    visit(&v.name, ctx);
                }
                // Leaf nodes
                TypedCore::Null(_) => {}
                TypedCore::Bool(_) => {}
                TypedCore::Number(_) => {}
                TypedCore::String(_) => {}
                TypedCore::Dummy => {}
                TypedCore::Empty() => {}
            }
        }
        visit(root, self);
    }
}
