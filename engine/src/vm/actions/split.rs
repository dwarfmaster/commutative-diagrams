use crate::tactics;
use crate::vm::VM;

impl VM {
    pub fn split(&mut self, src: usize, mph: usize) {
        if let Some(fce) = tactics::split_norm(&mut self.ctx, &mut self.graph, src, mph) {
            if self.graph.faces[fce].label.label.is_empty() {
                self.graph.faces[fce].label.label =
                    self.graph.faces[fce].eq.render(&mut self.ctx, 100);
            }
            // Hide the introduced equality, and the previous morphism
            self.graph.faces[fce].label.hidden = true;
            self.hide_and_replace_morphism(src, mph, fce)
        }
    }
}
