use crate::graph::eq::Morphism;
use crate::realizer::realize_morphism;
use crate::remote::Remote;
use crate::vm::{Interactive, VM};

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub fn path_to_edge(&mut self, src: usize, path: &[usize]) -> (usize, usize) {
        let comps = path
            .iter()
            .scan(
                src,
                |src: &mut usize, mph: &usize| -> Option<(u64, u64, u64)> {
                    let ret_src = self.graph.graph.nodes[*src].0;
                    let ret_dst = self.graph.graph.nodes[self.graph.graph.edges[*src][*mph].0].0;
                    let ret = self.graph.graph.edges[*src][*mph].2;
                    *src = self.graph.graph.edges[*src][*mph].0;
                    Some((ret_src, ret_dst, ret))
                },
            )
            .collect::<Vec<_>>();
        let src_mph = self.graph.graph.nodes[src].0;
        let mph = Morphism {
            src: src_mph,
            dst: if let Some((_, dst, _)) = comps.last() {
                *dst
            } else {
                src_mph
            },
            comps,
        };
        let m = realize_morphism(&mut self.ctx, self.graph.graph.nodes[src].1, &mph);
        self.insert_mph_at(src, m)
    }
}
