#[derive(Debug)]
pub struct LayoutConfig {
    pub ideal_distance: f32,
    pub speed: f32,
    pub edge_repulse: bool,
}

impl LayoutConfig {
    pub fn new() -> Self {
        Self {
            ideal_distance: 200f32,
            speed: 1f32,
            edge_repulse: true,
        }
    }
}

#[derive(Debug)]
pub struct Config {
    pub layout: LayoutConfig,
}

impl Config {
    pub fn new() -> Self {
        Self {
            layout: LayoutConfig::new(),
        }
    }
}
