use bevy::ecs::system::Resource;

use crate::ui::GraphDisplay;
use crate::vm::ast::AST;

#[derive(Resource)]
pub struct VM {
    pub code: String,
    pub ast: AST,
    pub parsed_until: usize,
    pub display: GraphDisplay,
}

impl VM {
    pub fn new(gd: GraphDisplay) -> Self {
        Self {
            code: String::new(),
            ast: Vec::new(),
            parsed_until: 0,
            display: gd,
        }
    }
}
