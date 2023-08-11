use super::super::config::Config;
use super::engine::ConnectedComponent;
use super::LayoutEngine;
use egui::{Pos2, Vec2};

impl LayoutEngine {
    pub fn reset_components(&mut self, _cfg: &Config) {
        self.components
            .resize(self.structure.ccs.len(), ConnectedComponent::new());
        for cc in 0..self.components.len() {
            if self.components[cc].part.is_none() {
                self.components[cc].part = Some(self.new_particle(Pos2::ZERO, None));
            }
        }
    }

    pub fn apply_cc_forces(&mut self, _cfg: &Config) {
        self.apply_cc_forces_repulse();
    }

    fn apply_cc_forces_repulse(&mut self) {
        let c = 1f32;
        let dist = 20f32;
        for cc1 in 0..self.components.len() {
            let id1 = self.components[cc1].part.unwrap();
            let off1 = self.particles[id1].pos.to_vec2();
            let r1 = self.components[cc1].rect.translate(off1).expand(dist);
            for cc2 in (cc1 + 1)..self.components.len() {
                let id2 = self.components[cc2].part.unwrap();
                let off2 = self.particles[id2].pos.to_vec2();
                let r2 = self.components[cc2].rect.translate(off2).expand(dist);

                let int = r1.intersect(r2);
                let w = int.width().clamp(0f32, f32::INFINITY);
                let h = int.height().clamp(0f32, f32::INFINITY);

                let dir = r2.center() - r1.center();
                let dir = if dir.length_sq() < 1f32 {
                    Vec2::X
                } else {
                    dir.normalized()
                };
                let f = c * (1f32 + 5f32 * w.min(h)).ln() * dir;
                self.add_force(id1, -f);
                self.add_force(id2, f);
            }
        }
    }
}
