use super::precompute::GraphStructure;
use crate::vm::Graph;
use egui::{Pos2, Vec2};
use std::collections::HashSet;
use std::time::Instant;

#[derive(Clone, Debug, Default)]
pub struct Particle {
    pub pos: Pos2,
    pub force: Vec2,
    pub fixed: u64,
}

#[derive(Clone, Debug)]
pub struct LayoutEngine {
    pub particles: Vec<Particle>,
    time: Instant,
    pub ideal_distance: f32,
    // Time in second elapsed since the start
    time_elapsed: f32,
    // Additional information about the graph
    pub structure: GraphStructure,
    // Nodes that must not move in each connected component
    fixed: Vec<[usize; 3]>,
}

// Using a decreasing function would allow to simulate annealing, which hasn't
// proved necessary yet.
fn temp_of_time(_t: f32) -> f32 {
    1.0f32
}

// Select 3 elements in the set
fn select3(graph: &Graph, set: &HashSet<usize>) -> [usize; 3] {
    assert!(!set.is_empty());
    let first = set.iter().next().unwrap();
    let v: Vec<usize> = set
        .iter()
        .chain(std::iter::repeat(first))
        .take(3)
        .copied()
        .collect();
    [
        graph.nodes[v[2]].2.pos.unwrap(),
        graph.nodes[v[1]].2.pos.unwrap(),
        graph.nodes[v[0]].2.pos.unwrap(),
    ]
}

impl LayoutEngine {
    pub fn new() -> Self {
        Self {
            particles: Vec::new(),
            time: Instant::now(),
            ideal_distance: 200.0f32,
            time_elapsed: 0f32,
            structure: GraphStructure::new(),
            fixed: Vec::new(),
        }
    }

    pub fn new_particle(&mut self, pos: Pos2) -> usize {
        let part = Particle {
            pos,
            force: Vec2::ZERO,
            fixed: 0,
        };
        let id = self.particles.len();
        self.particles.push(part);
        id
    }

    pub fn compute_structure(&mut self, graph: &Graph) {
        self.structure = GraphStructure::from_graph(graph);
    }

    pub fn init_fixed(&mut self, graph: &Graph) {
        self.fixed = self
            .structure
            .ccs
            .iter()
            .map(|set| select3(graph, set))
            .collect();
        for f in self.fixed.iter().flatten() {
            self.particles[*f].fixed += 1;
        }
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

    // Node must be a particle
    pub fn rotate_fixed(&mut self, graph: &Graph, node: usize) {
        let cc = self.structure.nodes_component[node];
        let id = graph.nodes[node].2.pos.unwrap();
        if self.fixed[cc][1] == id {
            self.fixed[cc] = [self.fixed[cc][0], self.fixed[cc][2], id];
        } else if self.fixed[cc][2] != id {
            self.particles[self.fixed[cc][0]].fixed -= 1;
            self.particles[id].fixed += 1;
            self.fixed[cc] = [self.fixed[cc][1], self.fixed[cc][2], id];
        }
    }

    fn step(&mut self, step: f32) {
        let temperature = temp_of_time(self.time_elapsed);
        for id in 0..self.particles.len() {
            let part = &mut self.particles[id];
            if part.fixed > 0 {
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
