use super::graph::graph::Action;
use crate::egui::{Context, Ui};
use crate::graph::GraphId;
use crate::vm;

mod apply;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Modifier {
    // Indicates the item is a part of the action
    pub active: bool,
    // Indicates the item is selected
    pub selected: bool,
    // Indicates the item can be selected
    pub candidate: bool,
}

type Md = crate::ui::graph::graph::Modifier;
pub fn apply_modifier(md: Modifier, color: &mut egui::Color32, modifier: &mut Md) {
    if md.selected {
        *color = egui::Color32::from_rgb_additive(150, 0, 255);
        *modifier = Md::Highlight;
    } else if md.active {
        *color = egui::Color32::from_rgb_additive(255, 165, 0);
    }
    if md.candidate {
        *modifier = Md::Highlight;
    }
}

pub enum InteractiveAction {
    LemmaApplication(apply::LemmaApplicationState),
}

impl vm::Interactive for InteractiveAction {
    fn compile(self) -> String {
        use InteractiveAction::*;
        match self {
            LemmaApplication(_) => "".to_string(), // TODO
        }
    }
    fn terminate(self) {}
}

pub type VM = vm::VM<InteractiveAction>;

impl InteractiveAction {
    pub fn apply(vm: &VM, lemma: usize) -> Self {
        let state = apply::LemmaApplicationState::new(vm, lemma);
        InteractiveAction::LemmaApplication(state)
    }

    pub fn display(&mut self, vm: &mut VM, ui: &Context) -> bool {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.display(vm, ui),
        }
    }

    pub fn context_menu(&mut self, vm: &mut VM, on: GraphId, ui: &mut Ui) -> bool {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.context_menu(vm, on, ui),
        }
    }

    pub fn action(&mut self, vm: &mut VM, act: Action, ui: &mut Ui) -> bool {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.action(vm, act, ui),
        }
    }

    pub fn modifier(&self, vm: &VM, on: GraphId) -> Modifier {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.modifier(vm, on),
        }
    }
}
