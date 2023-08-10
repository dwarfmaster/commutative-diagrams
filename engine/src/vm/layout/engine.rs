use super::precompute::GraphStructure;
use crate::vm::Graph;
use egui::{Pos2, Rect, Vec2};
use std::time::Instant;

#[derive(Clone, Debug, Default)]
pub struct Particle {
    pub pos: Pos2,
    pub force: Vec2,
    pub cc: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct ConnectedComponent {
    // The bounding rect of the connected component particles, before the offset
    // if applied
    pub rect: Rect,
    pub offset: Vec2,
}

impl ConnectedComponent {
    pub fn new() -> Self {
        Self {
            rect: Rect::NOTHING,
            offset: Vec2::ZERO,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LayoutEngine {
    pub particles: Vec<Particle>,
    // The bounding rect of each connected component
    pub components_rect: Vec<ConnectedComponent>,
    // Config
    pub ideal_distance: f32,
    // Time in second elapsed since the start
    time: Instant,
    time_elapsed: f32,
    // Additional information about the graph
    pub structure: GraphStructure,
}

// Using a decreasing function would allow to simulate annealing, which hasn't
// proved necessary yet.
fn temp_of_time(_t: f32) -> f32 {
    1.0f32
}

impl LayoutEngine {
    pub fn new() -> Self {
        Self {
            particles: Vec::new(),
            components_rect: Vec::new(),
            ideal_distance: 200.0f32,
            time: Instant::now(),
            time_elapsed: 0f32,
            structure: GraphStructure::new(),
        }
    }

    pub fn new_particle(&mut self, pos: Pos2, cc: Option<usize>) -> usize {
        let part = Particle {
            pos,
            force: Vec2::ZERO,
            cc,
        };
        let id = self.particles.len();
        self.particles.push(part);
        id
    }

    pub fn compute_structure(&mut self, graph: &Graph) {
        self.structure = GraphStructure::from_graph(graph);
    }

    pub fn get_pos(&self, part: usize) -> Pos2 {
        self.particles[part].pos
    }

    pub fn set_pos(&mut self, part: usize, pos: Pos2) {
        self.particles[part].pos = pos;
    }

    pub fn add_force(&mut self, part: usize, force: Vec2) {
        self.particles[part].force += force;
    }

    fn step(&mut self, step: f32) {
        self.components_rect
            .resize(self.structure.ccs.len(), ConnectedComponent::new());
        self.components_rect.fill(ConnectedComponent::new());
        let temperature = temp_of_time(self.time_elapsed);
        for id in 0..self.particles.len() {
            let part = &mut self.particles[id];
            let f = step * temperature * part.force;
            part.force = Vec2::ZERO;

            if f.length() >= 1e-6 {
                part.pos += f;
            }
            if let Some(cc) = part.cc {
                self.components_rect[cc].rect.extend_with(part.pos);
            }
        }
    }

    // Update position and speed according to forces and the time since last
    // round. Also clear forces.
    pub fn update(&mut self) {
        let elapsed = self.time.elapsed().as_secs_f32();
        self.time_elapsed += elapsed;
        let t = 10.0f32 * elapsed;
        self.time = Instant::now();
        self.step(t);
    }

    // Run many times to approximate convergence
    pub fn run(&mut self) {
        let count = 1000;
        for _ in 0..count {
            self.step(0.1);
        }
    }
}
