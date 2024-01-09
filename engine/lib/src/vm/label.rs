use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub async fn relabel(&self) {
        let graph = &mut self.graph.lock().await;
        let ctx = &mut self.ctx.lock().await;
        for id in 0..graph.graph.nodes.len() {
            let (nd, _, lbl) = &mut graph.graph.nodes[id];
            lbl.label = ctx.get_stored_label(*nd);
        }
        for src in 0..graph.graph.nodes.len() {
            for mph_id in 0..graph.graph.edges[src].len() {
                let (_, lbl, mph, _) = &mut graph.graph.edges[src][mph_id];
                lbl.label = ctx.get_stored_label(*mph);
            }
        }
        for id in 0..graph.graph.faces.len() {
            let fce = &mut graph.graph.faces[id];
            fce.label.label = "<<todo>>".to_string(); // todo!
        }
    }
}
