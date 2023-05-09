use crate::vm;

pub enum InteractiveAction {}

impl vm::Interactive for InteractiveAction {
    fn compile(self) -> String {
        match self {}
    }
    fn terminate(self) {}
}

pub type VM = vm::VM<InteractiveAction>;
