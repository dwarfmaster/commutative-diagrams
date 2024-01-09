mod faces;
pub mod graph;
mod lemma;
mod vm;
pub mod widget;

use crate::ui::VM;
use crate::vm::Lemma;
use crate::runtime::Runtime;

pub fn graph_vm<'a, RT: Runtime>(rt: &'a mut RT, vm: &'a mut VM<RT::Rem>) -> impl egui::Widget + 'a {
    widget::graph(vm, rt)
}
pub fn graph_lemma<'a, RT: Runtime>(rt: &'a mut RT, lemma: &'a mut Lemma) -> impl egui::Widget + 'a {
    widget::graph(lemma, rt)
}
