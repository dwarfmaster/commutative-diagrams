use super::graph::graph::Action;
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm;
use egui::{Context, Ui};
use std::sync::Arc;
use std::cell::RefCell;

pub mod apply;
pub mod compose;
pub mod insert;
pub mod merge;

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
pub fn apply_modifier(colors: Arc<RefCell<vm::SemanticColors>>, md: Modifier, color: &mut egui::Color32, modifier: &mut Md) {
    if md.selected {
        *color = colors.borrow().selected;
        *modifier = Md::Highlight;
    } else if md.active {
        *color = colors.borrow().active;
    }
    if md.candidate {
        *modifier = Md::Highlight;
    }
}

pub enum InteractiveAction {
    LemmaApplication(apply::LemmaApplicationState),
    Merge(merge::MergeState),
    Insert(insert::InsertState),
    Compose(compose::ComposeState),
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
    fn compile<R: Remote>(self, vm: &VM<R>) -> String {
        use InteractiveAction::*;
        match self {
            LemmaApplication(apply) => apply.compile(vm),
            Merge(merge) => merge.compile(vm),
            Insert(insert) => insert.compile(vm),
            Compose(comp) => comp.compile(vm),
        }
    }
    fn terminate(self) {}
}

pub type VM<R> = vm::VM<R, InteractiveAction>;

impl InteractiveAction {
    pub fn apply<R: Remote>(vm: &mut VM<R>, lemma: usize) -> Self {
        let state = apply::LemmaApplicationState::new(vm, lemma);
        InteractiveAction::LemmaApplication(state)
    }

    pub fn merge(id: GraphId) -> Self {
        let state = merge::MergeState::new(id);
        InteractiveAction::Merge(state)
    }

    pub fn insert(kind: insert::InsertKind) -> Self {
        let state = insert::InsertState::new(kind);
        InteractiveAction::Insert(state)
    }

    pub fn compose_id(nd: usize) -> Self {
        let state = compose::ComposeState::new_id(nd);
        InteractiveAction::Compose(state)
    }

    pub fn compose<R: Remote>(vm: &VM<R>, src: usize, mph: usize) -> Self {
        let state = compose::ComposeState::new(vm, src, mph);
        InteractiveAction::Compose(state)
    }

    pub fn display<R: Remote>(&mut self, vm: &mut VM<R>, ui: &Context) -> ActionResult {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.display(vm, ui),
            Merge(state) => state.display(vm, ui),
            Insert(state) => state.display(vm, ui),
            Compose(state) => state.display(vm, ui),
        }
    }

    pub fn context_menu<R: Remote>(
        &mut self,
        vm: &mut VM<R>,
        on: GraphId,
        ui: &mut Ui,
    ) -> ContextMenuResult {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.context_menu(vm, on, ui),
            Merge(state) => state.context_menu(vm, on, ui),
            Insert(state) => state.context_menu(vm, on, ui),
            Compose(state) => state.context_menu(vm, on, ui),
        }
    }

    pub fn action<R: Remote>(&mut self, vm: &mut VM<R>, act: Action, ui: &mut Ui) -> bool {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.action(vm, act, ui),
            Merge(state) => state.action(vm, act, ui),
            Insert(state) => state.action(vm, act, ui),
            Compose(state) => state.action(vm, act, ui),
        }
    }

    pub fn modifier<R: Remote>(&self, vm: &VM<R>, on: GraphId) -> Modifier {
        use InteractiveAction::*;
        match self {
            LemmaApplication(state) => state.modifier(vm, on),
            Merge(state) => state.modifier(vm, on),
            Insert(state) => state.modifier(vm, on),
            Compose(state) => state.modifier(vm, on),
        }
    }
}
