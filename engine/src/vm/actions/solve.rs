use crate::autofill::solve;
use crate::substitution::SubstitutableInPlace;
use crate::vm::VM;

impl VM {
    // Returns true if it succeeded in solving the face
    pub fn solve_face(&mut self, fce: usize, max_size: usize) -> bool {
        let sigma = solve(&mut self.ctx, &self.graph, fce, max_size);
        if let Some(sigma) = sigma {
            self.graph.subst_in_place(&self.ctx, &sigma);
            // TODO relabel all
            self.graph.faces[fce].label.label = self.graph.faces[fce].eq.render(&mut self.ctx, 100);
            self.layout();
            true
        } else {
            false
        }
    }
}
