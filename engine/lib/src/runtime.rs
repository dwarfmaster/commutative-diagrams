use crate::remote::Remote;
use crate::ui::VM;
use std::future::Future;

pub trait Runtime {
    type Rem: Remote;
    // Checks wether the runtime is already running something. If it is already
    // running something, returns it description.
    fn running<'a>(&'a self) -> Option<&'a str>;
    // Run a new task with an associated description.
    fn run<Fut: Future, F>(&mut self, desc: &str, f: F)
    where
        F: FnOnce(&VM<Self::Rem>) -> Fut;
}
