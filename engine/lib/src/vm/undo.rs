use super::{Interactive, VM};
use crate::remote::Remote;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub fn undo(&mut self) {
        if self.code.ast.is_empty() {
            self.code.error_msg = "Nothing to undo".to_string();
            return;
        }
        self.undo_until(self.code.ast.len() - 1);
    }

    pub fn redo(&mut self) {
        if let Some(ast) = self.recompile_one() {
            self.run(ast);
        }
    }
}
