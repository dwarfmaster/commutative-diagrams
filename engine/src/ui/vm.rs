use super::graph::graph::Action;
use crate::egui::{Context, Ui};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm;

// todo!
// mod apply;

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
    // LemmaApplication(apply::LemmaApplicationState),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ContextMenuResult {
    Added,
    Closed,
    Nothing,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ActionResult {
    Continue,
    Stop,
    Commit,
}

impl vm::Interactive for InteractiveAction {
    fn compile<R: Remote + Sync + Send>(self, _vm: &VM<R>) -> String {
        // use InteractiveAction::*;
        match self {
            // LemmaApplication(apply) => apply.compile(vm),
        }
    }
    fn terminate(self) {}
}

pub type VM<R> = vm::VM<R, InteractiveAction>;

impl InteractiveAction {
    // pub fn apply<R: Remote + Sync + Send>(vm: &mut VM<R>, lemma: usize) -> Self {
    //     let state = apply::LemmaApplicationState::new(vm, lemma);
    //     InteractiveAction::LemmaApplication(state)
    // }

    pub fn display<R: Remote + Sync + Send>(
        &mut self,
        _vm: &mut VM<R>,
        _ui: &Context,
    ) -> ActionResult {
        // use InteractiveAction::*;
        match self {
            // LemmaApplication(state) => state.display(vm, ui),
            _ => todo!(),
        }
    }

    pub fn context_menu<R: Remote + Sync + Send>(
        &mut self,
        _vm: &mut VM<R>,
        _on: GraphId,
        _ui: &mut Ui,
    ) -> ContextMenuResult {
        // use InteractiveAction::*;
        match self {
            // LemmaApplication(state) => state.context_menu(vm, on, ui),
            _ => todo!(),
        }
    }

    pub fn action<R: Remote + Sync + Send>(
        &mut self,
        _vm: &mut VM<R>,
        _act: Action,
        _ui: &mut Ui,
    ) -> bool {
        // use InteractiveAction::*;
        match self {
            // LemmaApplication(state) => state.action(vm, act, ui),
            _ => todo!(),
        }
    }

    pub fn modifier<R: Remote + Sync + Send>(&self, _vm: &VM<R>, _on: GraphId) -> Modifier {
        // use InteractiveAction::*;
        match self {
            // LemmaApplication(state) => state.modifier(vm, on),
            _ => todo!(),
        }
    }
}
