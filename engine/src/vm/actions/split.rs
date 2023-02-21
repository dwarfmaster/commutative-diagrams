use crate::tactics;
use crate::vm::VM;

impl VM {
    pub fn split(&mut self, src: usize, mph: usize) {
        // TODO hide previous morphism
        tactics::split_norm(&mut self.ctx, &mut self.graph, src, mph);
        self.layout()
    }
}
