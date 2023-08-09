use crate::vm::Graph;
use super::precompute::GraphStructure;
use egui::{Pos2, Vec2};
use std::time::Instant;

#[derive(Clone, Debug, Default)]
pub struct Particle {
    pub pos: Pos2,
    pub force: Vec2,
}

#[derive(Clone, Debug)]
pub struct LayoutEngine {
    pub particles: Vec<Particle>,
    time: Instant,
    pub ideal_distance: f32,
    // Time in second elapsed since the start
    time_elapsed: f32,
    // Additional information about the graph
    structure: GraphStructure,
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
            time: Instant::now(),
            ideal_distance: 200.0f32,
            time_elapsed: 0f32,
            structure: GraphStructure::new(),
        }
    }

    pub fn new_particle(&mut self, pos: Pos2) -> usize {
        let part = Particle {
            pos,
            force: Vec2::ZERO,
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

    fn step<F>(&mut self, step: f32, fixed: F)
    where
        F: Fn(usize) -> bool,
    {
        let temperature = temp_of_time(self.time_elapsed);
        for id in 0..self.particles.len() {
            let part = &mut self.particles[id];
            if fixed(id) {
                part.force = Vec2::ZERO;
                continue;
            }
            let f = step * temperature * part.force;
            part.force = Vec2::ZERO;

            if f.length() >= 1e-6 {
                part.pos += f;
            }
        }
    }

    // Update position and speed according to forces and the time since last
    // round. Also clear forces. fixed allow to prevent some particles from
    // moving.
    pub fn update<F>(&mut self, fixed: F)
    where
        F: Fn(usize) -> bool,
    {
        let elapsed = self.time.elapsed().as_secs_f32();
        self.time_elapsed += elapsed;
        let t = 10.0f32 * elapsed;
        self.time = Instant::now();
        self.step(t, fixed);
    }

    // Run many times to approximate convergence
    pub fn run(&mut self) {
        let count = 1000;
        for _ in 0..count {
            self.step(0.1, |_: usize| false);
        }
    }
}
