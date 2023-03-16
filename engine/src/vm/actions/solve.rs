use crate::anyterm::AnyTerm;
use crate::autofill::solve;
use crate::substitution::Substitutable;
use crate::vm::VM;

type Ins = crate::vm::asm::Instruction;

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
        // Substitute nodes
        for id in 0..self.graph.nodes.len() {
            let nd = self.graph.nodes[id].0.clone();
            let nsubst = nd.clone().subst(&self.ctx, &sigma);
            self.register_instruction(Ins::UpdateNode(id, nd, nsubst));
        }

        // Substitute morphisms
        for src in 0..self.graph.edges.len() {
            for edge in 0..self.graph.edges[src].len() {
                let mph = self.graph.edges[src][edge].2.clone();
                let msubst = mph.clone().subst(&self.ctx, &sigma);
                self.register_instruction(Ins::UpdateMorphism(src, edge, mph, msubst));
            }
        }

        // Substitute faces
        for id in 0..self.graph.faces.len() {
            let fce = self.graph.faces[id].eq.clone();
            let fsubst = fce.clone().subst(&self.ctx, &sigma).simpl(&self.ctx);
            self.register_instruction(Ins::UpdateFace(id, fce, fsubst));
        }

        self.register_instruction(Ins::ExtendRefinements(sigma));
        self.relabel();
    }
}
