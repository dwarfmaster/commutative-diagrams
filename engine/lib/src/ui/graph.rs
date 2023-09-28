mod faces;
pub mod graph;
mod lemma;
mod vm;
pub mod widget;

use crate::remote::Remote;
use crate::ui::VM;
use crate::vm::Lemma;

pub fn graph_vm<'a, Rm: Remote>(vm: &'a mut VM<Rm>) -> impl egui::Widget + 'a {
    widget::graph(vm)
}
pub fn graph_lemma<'a>(lemma: &'a mut Lemma) -> impl egui::Widget + 'a {
    widget::graph(lemma)
}
