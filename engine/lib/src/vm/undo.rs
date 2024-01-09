use super::{Interactive, VM};
use crate::remote::Remote;

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    pub async fn undo(&mut self) {
        let len = {
            let code = &mut self.code.lock().await;
            if code.ast.is_empty() {
                code.error_msg = "Nothing to undo".to_string();
                return;
            }
            code.ast.len()
        };
        self.undo_until(len - 1);
    }

    pub async fn redo(&mut self) {
        if let Some(ast) = self.recompile_one().await {
            self.run(ast);
        }
    }
}
