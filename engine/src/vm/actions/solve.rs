use crate::autofill::solve;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    // Returns true if it succeeded in solving the face
    pub fn solve_face(&mut self, fce: usize, max_size: usize) -> bool {
        let mut mask = vec![true; self.graph.faces.len()];
        // Disable all parents
        let mut current = Some(fce);
        while let Some(nxt) = current {
            mask[nxt] = false;
            current = self.graph.faces[nxt].label.parent;
        }

        let cat = self.graph.nodes[self.graph.faces[fce].start].1;
        let solved = solve(&mut self.ctx, &self.graph, &mask, fce, max_size);
        if let Some(eq) = solved {
            let feq = self.graph.faces[fce].eq.clone();
            self.unify_eq(cat, &feq, &eq);
            // self.refine(sigma);
            true
        } else {
            false
        }
    }
}
