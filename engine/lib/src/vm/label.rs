use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub fn relabel(&mut self) {
        for id in 0..self.graph.graph.nodes.len() {
            let (nd, _, lbl) = &mut self.graph.graph.nodes[id];
            lbl.label = self.ctx.get_stored_label(*nd);
        }
        for src in 0..self.graph.graph.nodes.len() {
            for mph_id in 0..self.graph.graph.edges[src].len() {
                let (_, lbl, mph, _) = &mut self.graph.graph.edges[src][mph_id];
                lbl.label = self.ctx.get_stored_label(*mph);
            }
        }
        for id in 0..self.graph.graph.faces.len() {
            let fce = &mut self.graph.graph.faces[id];
            fce.label.label = "<<todo>>".to_string(); // todo!
        }
    }
}
