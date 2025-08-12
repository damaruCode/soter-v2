use crate::ast::TypedCore;
use std::collections::HashMap;

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
                TypedCore::AstList(list) => {
                    for child in &list.inner {
                        visit(child, ctx);
                    }
                }
                TypedCore::AstTuple(t) => {
                    visit(&t.frst, ctx);
                    visit(&t.scnd, ctx);
                }
                // TODO
                _ => {}
            }
        }
        visit(root, self);
    }
}
