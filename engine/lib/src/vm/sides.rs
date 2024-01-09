use crate::graph::eq::Morphism;
use crate::remote::Remote;
use crate::vm::graph::EdgeLabel;
use crate::vm::{Graph, Interactive, VM};

fn on_path<F>(
    edges: &mut Vec<Vec<(usize, EdgeLabel, u64, Morphism)>>,
    mut node: usize,
    nexts: &[usize],
    f: F,
) where
    F: Fn(&mut EdgeLabel),
{
    for nid in 0..nexts.len() {
        let (dst, lbl, _, _) = &mut edges[node][nexts[nid]];
        f(lbl);
        node = *dst;
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub fn show_face_impl(graph: &mut Graph, fce: usize) {
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].left,
            |lbl| {
                lbl.style.left = true;
            },
        );
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].right,
            |lbl| {
                lbl.style.right = true;
            },
        );
    }

    pub async fn show_face(&self, fce: usize) {
        Self::show_face_impl(&mut self.graph.lock().await.graph, fce);
    }

    pub fn unshow_face_impl(graph: &mut Graph, fce: usize) {
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].left,
            |lbl| {
                lbl.style.left = false;
            },
        );
        on_path(
            &mut graph.edges,
            graph.faces[fce].start,
            &graph.faces[fce].right,
            |lbl| {
                lbl.style.right = false;
            },
        );
    }

    pub async fn unshow_face(&self, fce: usize) {
        Self::unshow_face_impl(&mut self.graph.lock().await.graph, fce);
    }

    pub async fn deselect_face(&self) {
        let sel = self.graph.lock().await.selected_face.take();
        if let Some(fce) = sel {
            self.unshow_face(fce).await;
        }
    }

    pub async fn select_face(&mut self, fce: usize) {
        let prev = self.graph.lock().await.selected_face.clone();
        if let Some(prev) = prev {
            if prev == fce {
                return;
            }
            self.unshow_face(prev).await;
        }
        self.show_face(fce).await;
        self.graph.lock().await.selected_face = Some(fce);
    }
}
