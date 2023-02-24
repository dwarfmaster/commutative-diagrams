use crate::vm::graph::LabelSource;
use crate::vm::VM;
use hashconsing::HConsed;

fn should_render<T>(src: &LabelSource, obj: &HConsed<T>) -> bool {
    use LabelSource::*;
    match src {
        None => true,
        Render(id) => *id != obj.uid(),
        Manual => false,
    }
}

impl VM {
    pub fn relabel(&mut self) {
        use LabelSource::*;
        let mut relayout = false;
        for (nd, lbl) in self.graph.nodes.iter_mut() {
            if should_render(&lbl.label_source, &nd) {
                lbl.label = nd.render(&mut self.ctx, 100);
                lbl.label_source = Render(nd.uid());
                relayout = true;
            }
        }
        for src in 0..self.graph.nodes.len() {
            for (_, lbl, mph) in self.graph.edges[src].iter_mut() {
                if should_render(&lbl.label_source, &mph) {
                    lbl.label = mph.render(&mut self.ctx, 100);
                    lbl.label_source = Render(mph.uid());
                    relayout = true;
                }
            }
        }
        for fce in self.graph.faces.iter_mut() {
            if should_render(&fce.label.label_source, &fce.eq) {
                fce.label.label = fce.eq.render(&mut self.ctx, 100);
                fce.label.label_source = Render(fce.eq.uid());
                relayout = true;
            }
        }
        if relayout {
            self.layout();
        }
    }
}
