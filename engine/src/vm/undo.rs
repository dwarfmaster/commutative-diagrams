use super::{Interactive, VM};
use crate::remote::Remote;

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn undo(&mut self) {
        if self.ast.is_empty() {
            self.error_msg = "Nothing to undo".to_string();
            return;
        }
        self.undo_until(self.ast.len() - 1);
    }

    pub fn redo(&mut self) {
        todo!()
    }
}
