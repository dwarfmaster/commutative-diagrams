use crate::vm;

pub enum InteractiveAction {}

impl vm::Interactive for InteractiveAction {
    fn compile(self) -> String {
        match self {}
    }
}

pub type VM = vm::VM<InteractiveAction>;
