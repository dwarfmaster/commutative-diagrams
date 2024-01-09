use crate::autofill::solve;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};
use core::ops::DerefMut;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Returns true if it succeeded in solving the face
    pub async fn solve_face(&self, fce: usize, max_size: usize) -> bool {
        let (cat, solved) = {
            let graph = self.graph.lock().await;
            let mut mask = vec![true; graph.graph.faces.len()];
            // Disable all parents
            let mut current = Some(fce);
            while let Some(nxt) = current {
                mask[nxt] = false;
                current = graph.graph.faces[nxt].label.parent;
            }

            let cat = graph.graph.nodes[graph.graph.faces[fce].start].1;
            let ctx = &mut self.ctx.lock().await;
            let solved = solve(ctx.deref_mut(), &graph.graph, &mask, fce, max_size)
                .map(|eq| (graph.graph.faces[fce].eq.clone(), eq));
            (cat, solved)
        };
        if let Some((feq, eq)) = solved {
            self.unify_eq(cat, &feq, &eq);
            true
        } else {
            false
        }
    }
}
