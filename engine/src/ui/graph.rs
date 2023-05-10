mod faces;
pub mod graph;
mod lemma;
mod vm;
pub mod widget;

use crate::ui::VM;
use crate::vm::Lemma;

pub fn graph_vm<'a>(vm: &'a mut VM) -> impl egui::Widget + 'a {
    widget::graph(vm)
}
pub fn graph_lemma<'a>(lemma: &'a mut Lemma) -> impl egui::Widget + 'a {
    widget::graph(lemma)
}
