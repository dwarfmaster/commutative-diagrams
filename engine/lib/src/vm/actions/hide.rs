use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::asm;
use crate::vm::graph::{EdgeLabel, FaceLabel, NodeLabel};
use crate::vm::{Interactive, VM};

type Ins = asm::Instruction;

trait HasHidden {
    fn hidden<'a>(&'a mut self) -> &'a mut bool;
    fn is_hidden(&self) -> bool;
}
macro_rules! derive_has_hidden {
    ($t:ty) => {
        impl HasHidden for $t {
            fn hidden<'a>(&'a mut self) -> &'a mut bool {
                &mut self.hidden
            }
            fn is_hidden(&self) -> bool {
                self.hidden
            }
        }
    };
}
derive_has_hidden!(NodeLabel);
derive_has_hidden!(EdgeLabel);
derive_has_hidden!(FaceLabel);

fn hidden_upd<T: HasHidden + Clone>(old: bool, new: bool) -> asm::Updater<T> {
    let direct = move |v: &mut T| {
        *v.hidden() = new;
    };
    let reverse = move |v: &mut T| {
        *v.hidden() = old;
    };
    asm::Updater {
        direct: Box::new(direct),
        reverse: Box::new(reverse),
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    async fn hide_node(&self, id: usize) {
        let old = {
            let graph = self.graph.lock().await;
            if graph.graph.nodes[id].2.hidden {
                return;
            }
            graph.graph.nodes[id].2.is_hidden()
        };

        let () = self
            .register_instruction(Ins::UpdateNodeLabel(id, hidden_upd(old, true)))
            .await;
        let nnodes = self.graph.lock().await.graph.nodes.len();
        for src in 0..nnodes {
            let nmphs = self.graph.lock().await.graph.edges[src].len();
            for mph in 0..nmphs {
                let (dst, old) = {
                    let graph = self.graph.lock().await;
                    (
                        graph.graph.edges[src][mph].0,
                        graph.graph.edges[src][mph].1.is_hidden(),
                    )
                };
                if src == id || dst == id {
                    let () = self
                        .register_instruction(Ins::UpdateMorphismLabel(
                            src,
                            mph,
                            hidden_upd(old, true),
                        ))
                        .await;
                }
            }
        }
    }

    // When hiding a node, hide all adjacent edges
    pub async fn hide(&self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => self.hide_node(n).await,
            Morphism(src, mph) => {
                let old = self.graph.lock().await.graph.edges[src][mph].1.is_hidden();
                let () = self
                    .register_instruction(Ins::UpdateMorphismLabel(src, mph, hidden_upd(old, true)))
                    .await;
            }
            Face(f) => {
                let old = self.graph.lock().await.graph.faces[f].label.is_hidden();
                let () = self
                    .register_instruction(Ins::UpdateFaceLabel(f, hidden_upd(old, true)))
                    .await;
            }
        }
    }

    // When revealing an edge, reveal its source and target nodes
    pub async fn reveal(&self, id: GraphId) {
        use GraphId::*;
        match id {
            Node(n) => {
                let old = self.graph.lock().await.graph.nodes[n].2.is_hidden();
                let () = self
                    .register_instruction(Ins::UpdateNodeLabel(n, hidden_upd(old, false)))
                    .await;
            }
            Morphism(src, mph) => {
                let (dst, old, old_src, old_dst) = {
                    let graph = self.graph.lock().await;
                    let dst = graph.graph.edges[src][mph].0;
                    (
                        dst,
                        graph.graph.edges[src][mph].1.is_hidden(),
                        graph.graph.nodes[src].2.is_hidden(),
                        graph.graph.nodes[dst].2.is_hidden(),
                    )
                };
                let () = self
                    .register_instruction(Ins::UpdateMorphismLabel(
                        src,
                        mph,
                        hidden_upd(old, false),
                    ))
                    .await;
                let () = self
                    .register_instruction(Ins::UpdateNodeLabel(src, hidden_upd(old_src, false)))
                    .await;
                let () = self
                    .register_instruction(Ins::UpdateNodeLabel(dst, hidden_upd(old_dst, false)))
                    .await;
            }
            Face(f) => {
                let old = self.graph.lock().await.graph.faces[f].label.is_hidden();
                let () = self
                    .register_instruction(Ins::UpdateFaceLabel(f, hidden_upd(old, false)))
                    .await;
            }
        }
    }
}
