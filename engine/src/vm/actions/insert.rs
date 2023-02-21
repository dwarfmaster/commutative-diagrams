use crate::data::{Morphism, Object};
use crate::tactics;
use crate::vm::VM;

impl VM {
    pub fn insert_node(&mut self, node: Object) {
        tactics::insert_node(&mut self.graph, node);
        self.layout()
    }

    pub fn insert_mph(&mut self, mph: Morphism) {
        tactics::insert_mph(&mut self.ctx, &mut self.graph, mph);
        self.layout()
    }

    pub fn insert_mph_at(&mut self, src: usize, mph: Morphism) {
        tactics::insert_mph_at(&mut self.ctx, &mut self.graph, src, mph);
        self.layout()
    }
}
