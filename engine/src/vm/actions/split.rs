use crate::tactics;
use crate::vm::graph::GraphId;
use crate::vm::VM;

impl VM {
    pub fn split(&mut self, src: usize, mph: usize) {
        // TODO create a display system for equality sides
        if let Some(fce) = tactics::split_norm(&mut self.ctx, &mut self.graph, src, mph) {
            if self.graph.faces[fce].label.label.is_empty() {
                self.graph.faces[fce].label.label =
                    self.graph.faces[fce].eq.render(&mut self.ctx, 100);
            }
            self.hide(GraphId::Morphism(src, mph))
        }
    }
}
