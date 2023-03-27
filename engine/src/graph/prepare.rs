use crate::data::{Context, Morphism, Object};
use crate::graph::{Face, FaceParsed, Graph, GraphParsed};

impl<NL, EL, FL> GraphParsed<NL, EL, FL> {
    pub fn prepare(self, ctx: &Context) -> Graph<NL, EL, FL> {
        let faces = self
            .faces
            .into_iter()
            .map(|fce| fce.prepare(ctx, &self.nodes, &self.edges))
            .collect();
        Graph {
            nodes: self.nodes,
            edges: self.edges,
            faces,
        }
    }
}

impl<FL> FaceParsed<FL> {
    fn prepare<NL, EL>(
        self,
        _ctx: &Context,
        _nodes: &[(Object, NL)],
        _edges: &[Vec<(usize, EL, Morphism)>],
    ) -> Face<FL> {
        Face {
            start: self.start,
            end: self.end,
            left: self.left,
            right: self.right,
            eq: self.eq,
            label: self.label,
        }
    }
}
