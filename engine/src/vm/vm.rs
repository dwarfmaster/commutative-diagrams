use bevy::ecs::system::Resource;

use crate::ui::GraphDisplay;
use crate::vm::ast::AST;
use crate::vm::parser;

#[derive(Resource)]
pub struct VM {
    pub code: String,
    pub ast: AST,
    pub error_at: Option<(usize, usize)>,
    pub error_msg: String,
    pub parsed_until: usize,
    pub display: GraphDisplay,
}

impl VM {
    pub fn new(gd: GraphDisplay) -> Self {
        Self {
            code: String::new(),
            ast: Vec::new(),
            error_at: None,
            error_msg: String::new(),
            parsed_until: 0,
            display: gd,
        }
    }

    // Compile the code, but do not run it
    pub fn recompile(&mut self) {
        let r = parser::script(&self.code);
        match r {
            Ok((left, ast)) => {
                self.parsed_until = self.code.len() - left.len();
                self.ast = ast;
            }
            Err(err) => {
                let err = match err {
                    nom::Err::Incomplete(_) => panic!("Using complete parsers"),
                    nom::Err::Error(err) => err,
                    nom::Err::Failure(err) => err,
                };
                let start = unsafe { err.input.as_ptr().offset_from(self.code.as_ptr()) as usize };
                self.ast.clear();
                self.error_msg = format!("{}", err);
                self.error_at = Some((start, start + err.input.len()));
            }
        }
    }
}
