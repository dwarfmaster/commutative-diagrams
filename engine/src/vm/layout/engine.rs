use egui::{Pos2,Vec2};
use std::time::Instant;

#[derive(Clone, Debug, Default)]
struct Particle {
    pos: Pos2,
    speed: Vec2,
    force: Vec2,
}

#[derive(Clone, Debug)]
pub struct LayoutEngine {
    particles: Vec<Particle>,
    time: Instant,
}

impl LayoutEngine {
    pub fn new() -> Self {
        Self {
            particles: Vec::new(),
            time: Instant::now(),
        }
    }

    pub fn new_particle(&mut self, pos: Pos2, speed: Vec2) -> usize {
        let part = Particle {
            pos,
            speed,
            force: Vec2::ZERO,
        };
        let id = self.particles.len();
        self.particles.push(part);
        id
    }

    pub fn get_pos(&self, part: usize) -> Pos2 {
        self.particles[part].pos
    }

    pub fn add_force(&mut self, part: usize, force: Vec2) {
        self.particles[part].force += force;
    }

    // Update position and speed according to forces and the time since last
    // round. Also clear forces.
    pub fn update(&mut self) {
        let t = self.time.elapsed().as_secs_f32();
        self.time = Instant::now();

        // We assume a mass of one for all particles, so the force is exactly
        // the acceleration. Actually the integration is very stupid.
        for part in self.particles.iter_mut() {
            let f = t * part.force;
            part.force = Vec2::ZERO;

            if f.length() >= 1e-6 {
                part.speed += f;
                if part.speed.length() >= 1e-6 {
                    part.pos += part.speed;
                } else {
                    part.speed = Vec2::ZERO;
                }
            }
        }
    }
}
