mod graph;
mod vm;
mod widget;

use crate::vm::VM;

pub fn graph_vm<'a>(vm: &'a mut VM) -> impl egui::Widget + 'a {
    widget::graph(vm)
}
