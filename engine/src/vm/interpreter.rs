use crate::vm::asm::Instruction;
use crate::vm::VM;

pub struct InterpreterStatus {
    should_relayout: bool,
}

impl InterpreterStatus {
    pub fn new() -> Self {
        Self {
            should_relayout: false,
        }
    }
}

impl VM {
    // Execute the instruction and register it in the vm
    pub fn register_instruction(&mut self, ins: Instruction) {
        self.execute_instruction(&ins);
        self.instructions.push(ins)
    }

    // Undo last instruction
    pub fn pop_instruction(&mut self) {
        if let Some(ins) = self.instructions.pop() {
            self.undo_instruction(&ins)
        }
    }

    // Setup to do before executing instructions
    pub fn initialize_execution(&mut self) {
        todo!()
    }

    // After executing potentially multiple instruction, finish the evaluation
    // and prepare the vm for display
    pub fn finalize_execution(&mut self) {
        if self.eval_status.should_relayout {
            self.layout()
        }
        self.eval_status.should_relayout = false;
    }

    // Execute one instruction
    fn execute_instruction(&mut self, ins: &Instruction) {
        todo!()
    }

    // Undo one instruction
    fn undo_instruction(&mut self, ins: &Instruction) {
        todo!()
    }
}
