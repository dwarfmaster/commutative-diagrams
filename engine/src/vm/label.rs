use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn relabel(&mut self) {
        for id in 0..self.graph.nodes.len() {
            let (nd, _, lbl) = &mut self.graph.nodes[id];
            lbl.label = self.ctx.get_stored_label(*nd);
        }
        for src in 0..self.graph.nodes.len() {
            for mph_id in 0..self.graph.edges[src].len() {
                let (_, lbl, mph) = &mut self.graph.edges[src][mph_id];
                lbl.label = self.ctx.get_stored_label(*mph);
            }
        }
        for id in 0..self.graph.faces.len() {
            let fce = &mut self.graph.faces[id];
            fce.label.label = "<<todo>>".to_string(); // todo!
        }
    }
}
