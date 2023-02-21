use crate::data::Morphism;
use crate::tactics;
use crate::vm::VM;

impl VM {
    pub fn split(&mut self, mph: Morphism) {
        // TODO hide previous morphism
        let (src_id, mph_id, _) = tactics::insert_mph(&mut self.ctx, &mut self.graph, mph);
        tactics::split_norm(&mut self.ctx, &mut self.graph, src_id, mph_id);
        self.layout()
    }
}
