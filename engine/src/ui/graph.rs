mod graph;
mod lemma;
mod vm;
mod widget;

use crate::vm::{Lemma, VM};

pub fn graph_vm<'a>(vm: &'a mut VM) -> impl egui::Widget + 'a {
    widget::graph(vm)
}
pub fn graph_lemma<'a>(lemma: &'a mut Lemma) -> impl egui::Widget + 'a {
    widget::graph(lemma)
}
