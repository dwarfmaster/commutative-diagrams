use crate::autofill::solve;
use crate::vm::{Interactive, VM};

impl<I: Interactive + Sync + Send> VM<I> {
    // Returns true if it succeeded in solving the face
    pub fn solve_face(&mut self, fce: usize, max_size: usize) -> bool {
        let mut mask = vec![true; self.graph.faces.len()];
        // Disable all parents
        let mut current = Some(fce);
        while let Some(nxt) = current {
            mask[nxt] = false;
            current = self.graph.faces[nxt].label.parent;
        }

        let sigma = solve(&mut self.ctx, &self.graph, &mask, fce, max_size);
        if let Some(sigma) = sigma {
            self.refine(sigma);
            true
        } else {
            false
        }
    }
}
