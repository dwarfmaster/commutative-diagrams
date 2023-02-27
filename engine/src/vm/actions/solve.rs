use crate::anyterm::AnyTerm;
use crate::autofill::solve;
use crate::substitution::SubstitutableInPlace;
use crate::vm::VM;

impl VM {
    // Returns true if it succeeded in solving the face
    pub fn solve_face(&mut self, fce: usize, max_size: usize) -> bool {
        let sigma = solve(&mut self.ctx, &self.graph, fce, max_size);
        if let Some(sigma) = sigma {
            self.do_subst(sigma);
            true
        } else {
            false
        }
    }

    // Apply substitutions to graph, handling relabeling and relayouting
    pub fn do_subst(&mut self, sigma: Vec<(u64, AnyTerm)>) {
        self.graph.subst_in_place(&self.ctx, &sigma);
        self.refinements
            .iter_mut()
            .for_each(|rf| rf.1.subst_in_place(&self.ctx, &sigma));
        self.relabel();
        self.refinements.extend(sigma);
    }
}
