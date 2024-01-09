use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::asm::Instruction;
use crate::vm::{Interactive, VM};

#[derive(Clone)]
pub struct InterpreterStatus {
    // Should the graph be re-layouted after the execution
    should_relayout: bool,
    // Should the graph be re-labeled after the execution
    should_relabel: bool,
}

impl InterpreterStatus {
    pub fn new() -> Self {
        Self {
            should_relayout: false,
            should_relabel: false,
        }
    }
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    // Execute the instruction and register it in the vm
    pub async fn register_instruction(&self, ins: Instruction) {
        let () = self.execute_instruction(&ins).await;
        self.ins.lock().await.instructions.push(ins)
    }

    // Undo last instruction
    pub async fn pop_instruction(&self) {
        let ins = {
            let ins = &mut self.ins.lock().await;
            ins.instructions.pop()
        };
        if let Some(ins) = ins {
            let () = self.undo_instruction(&ins).await;
        }
    }

    // Must be called everytime the proof assistant state is changed by the execution
    pub async fn change_state(&self) {
        let () = self.register_instruction(Instruction::DirtyState).await;
    }

    // Setup to do before executing instructions
    pub async fn initialize_execution(&self) {
        self.ins.lock().await.eval_status = InterpreterStatus::new();
        let sel = {
            let graph = self.graph.lock().await;
            graph.selected_face.clone()
        };
        if let Some(face) = sel {
            self.unshow_face(face).await
        }
    }

    // After executing potentially multiple instruction, finish the evaluation
    // and prepare the vm for display
    pub async fn finalize_execution(&self) {
        let _: u64 = self.ctx.lock().await.save_state();
        let status = {
            let ins = self.ins.lock().await;
            ins.eval_status.clone()
        };
        if status.should_relabel {
            self.relabel().await
        }
        if status.should_relayout {
            let graph = &mut self.graph.lock().await;
            let config = &mut self.config.lock().await;
            let () = graph.particles_for_graph(&config);
        }
        let sel = {
            let graph = &mut self.graph.lock().await;
            graph.selected_face.clone()
        };
        if let Some(face) = sel {
            self.show_face(face).await
        }
        self.recompute_face_statuses();
    }

    // Execute one instruction
    async fn execute_instruction(&self, ins: &Instruction) {
        use Instruction::*;
        match ins {
            InsertNode(obj, cat) => {
                let nid = {
                    let graph = &mut self.graph.lock().await;
                    graph.graph.nodes.push((*obj, *cat, Default::default()));
                    graph.graph.edges.push(vec![]);
                    graph.graph.nodes.len() - 1
                };
                self.autoname_node(nid);
                let ins = &mut self.ins.lock().await;
                ins.eval_status.should_relayout = true;
                ins.eval_status.should_relabel = true;
            }
            UpdateNode(nd, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.nodes[*nd].0, *old);
                graph.graph.nodes[*nd].0 = new.clone();
                ins.eval_status.should_relabel = true;
            }
            UpdateNodeLabel(nd, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.apply(&mut graph.graph.nodes[*nd].2);
                ins.eval_status.should_relayout = true;
            }
            RenameNode(nd, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.nodes[*nd].2.name, prev);
                graph.graph.nodes[*nd].2.name = new.clone();
                graph.names.remove(prev);
                graph.names.insert(new.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, dst, mph, morph) => {
                let nid = {
                    let graph = &mut self.graph.lock().await;
                    graph.graph.edges[*src].push((
                        *dst,
                        Default::default(),
                        mph.clone(),
                        morph.clone(),
                    ));
                    graph.graph.edges[*src].len() - 1
                };
                self.autoname_morphism(*src, nid);
                let ins = &mut self.ins.lock().await;
                ins.eval_status.should_relayout = true;
                ins.eval_status.should_relabel = true;
            }
            UpdateMorphism(src, mph, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.edges[*src][*mph].2, *old);
                graph.graph.edges[*src][*mph].2 = new.clone();
                ins.eval_status.should_relabel = true;
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                let name = graph.graph.edges[*old_src].last().unwrap().1.name.clone();
                graph.names.insert(name, GraphId::Morphism(*old_src, *mph));
                let edge = graph.graph.edges[*old_src].swap_remove(*mph);
                let new_mph = graph.graph.edges[*new_src].len();
                graph
                    .names
                    .insert(edge.1.name.clone(), GraphId::Morphism(*new_src, new_mph));
                graph.graph.edges[*new_src].push(edge);
                ins.eval_status.should_relayout = true;
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.edges[*src][*mph].0, *old_dst);
                graph.graph.edges[*src][*mph].0 = *new_dst;
                ins.eval_status.should_relayout = true;
            }
            UpdateMorphismLabel(src, mph, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.apply(&mut graph.graph.edges[*src][*mph].1);
                ins.eval_status.should_relayout = true;
            }
            RenameMorphism(src, mph, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.edges[*src][*mph].1.name, prev);
                graph.graph.edges[*src][*mph].1.name = new.clone();
                graph.names.remove(prev);
                graph
                    .names
                    .insert(new.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(fce) => {
                let nfce = {
                    let graph = &mut self.graph.lock().await;
                    graph.graph.faces.push(fce.clone());
                    graph.graph.faces.len() - 1
                };
                self.autoname_face(nfce);
                if let Some(parent) = fce.label.parent {
                    let graph = &mut self.graph.lock().await;
                    graph.graph.faces[parent].label.children.push(nfce);
                    if let Some(sel) = graph.selected_face {
                        if sel == parent {
                            graph.selected_face = Some(graph.graph.faces.len() - 1);
                        }
                    }
                }
                let () = self.set_face_status(nfce).await;
                let () = self.graph.lock().await.order_new_face(nfce);
                let ins = &mut self.ins.lock().await;
                ins.eval_status.should_relayout = true;
                ins.eval_status.should_relabel = true;
            }
            UpdateFace(fce, old, new) => {
                {
                    let graph = &mut self.graph.lock().await;
                    let ins = &mut self.ins.lock().await;
                    assert_eq!(graph.graph.faces[*fce].eq, *old);
                    graph.graph.faces[*fce].eq = new.clone();
                    ins.eval_status.should_relabel = true;
                }
                let () = self.set_face_status(*fce).await;
            }
            RelocateFaceSrc(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].start, *old);
                graph.graph.faces[*fce].start = *new;
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceDst(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].end, *old);
                graph.graph.faces[*fce].end = *new;
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceLeft(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].left, *old);
                graph.graph.faces[*fce].left = new.clone();
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceRight(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].right, *old);
                graph.graph.faces[*fce].right = new.clone();
                ins.eval_status.should_relayout = true;
            }
            UpdateFaceLabel(fce, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.apply(&mut graph.graph.faces[*fce].label);
                ins.eval_status.should_relayout = true;
            }
            RenameFace(fce, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.faces[*fce].label.name, prev);
                graph.graph.faces[*fce].label.name = new.clone();
                graph.names.remove(prev);
                graph.names.insert(new.clone(), GraphId::Face(*fce));
            }
            DirtyState => self.ins.lock().await.eval_status.should_relabel = true,
        }
    }

    // Undo one instruction
    pub async fn undo_instruction(&self, ins: &Instruction) {
        use Instruction::*;
        match ins {
            InsertNode(..) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                if let Some(nd) = graph.graph.nodes.pop() {
                    graph.names.remove(&nd.2.name);
                }
                graph.graph.edges.pop();
                ins.eval_status.should_relayout = true;
            }
            UpdateNode(nd, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.nodes[*nd].0, *new);
                graph.graph.nodes[*nd].0 = old.clone();
                ins.eval_status.should_relabel = true;
            }
            UpdateNodeLabel(nd, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.undo(&mut graph.graph.nodes[*nd].2);
                ins.eval_status.should_relayout = true;
            }
            RenameNode(nd, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.nodes[*nd].2.name, new);
                graph.graph.nodes[*nd].2.name = prev.clone();
                graph.names.remove(new);
                graph.names.insert(prev.clone(), GraphId::Node(*nd));
            }
            InsertMorphism(src, _, _, _) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                if let Some(mph) = graph.graph.edges[*src].pop() {
                    graph.names.remove(&mph.1.name);
                }
                ins.eval_status.should_relayout = true;
            }
            UpdateMorphism(src, mph, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.edges[*src][*mph].2, *new);
                graph.graph.edges[*src][*mph].2 = old.clone();
                ins.eval_status.should_relabel = true;
            }
            RelocateMorphismSrc(old_src, new_src, mph) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                let mut edge = graph.graph.edges[*new_src].pop().unwrap();
                graph
                    .names
                    .insert(edge.1.name.clone(), GraphId::Morphism(*old_src, *mph));
                if *mph < graph.graph.edges[*old_src].len() {
                    std::mem::swap(&mut edge, &mut graph.graph.edges[*old_src][*mph]);
                    let mphid = graph.graph.edges[*old_src].len();
                    graph
                        .names
                        .insert(edge.1.name.clone(), GraphId::Morphism(*old_src, mphid));
                }
                graph.graph.edges[*old_src].push(edge);
                ins.eval_status.should_relayout = true;
            }
            RelocateMorphismDst(src, mph, old_dst, new_dst) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.edges[*src][*mph].0, *new_dst);
                graph.graph.edges[*src][*mph].0 = *old_dst;
                ins.eval_status.should_relayout = true;
            }
            UpdateMorphismLabel(src, mph, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.undo(&mut graph.graph.edges[*src][*mph].1);
                ins.eval_status.should_relayout = true;
            }
            RenameMorphism(src, mph, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.edges[*src][*mph].1.name, new);
                graph.graph.edges[*src][*mph].1.name = prev.clone();
                graph.names.remove(new);
                graph
                    .names
                    .insert(prev.clone(), GraphId::Morphism(*src, *mph));
            }
            InsertFace(_) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                if let Some(fce) = graph.graph.faces.pop() {
                    if let Some(focused) = graph.selected_face {
                        if focused == graph.graph.faces.len() {
                            graph.selected_face = fce.label.parent;
                        }
                    }
                    graph.names.remove(&fce.label.name);
                    if let Some(parent) = fce.label.parent {
                        if graph.graph.faces[parent].label.name.is_empty()
                            && graph.graph.faces[parent].label.hidden
                        {
                            let () = graph.set_name(GraphId::Face(parent), fce.label.name);
                        }
                        graph.graph.faces[parent].label.children.pop();
                    }
                    let fid = graph.graph.faces.len();
                    let () = graph.order_rm_face(fid);
                }
                ins.eval_status.should_relayout = true;
            }
            UpdateFace(fce, old, new) => {
                {
                    let graph = &mut self.graph.lock().await;
                    let ins = &mut self.ins.lock().await;
                    assert_eq!(graph.graph.faces[*fce].eq, *new);
                    graph.graph.faces[*fce].eq = old.clone();
                    ins.eval_status.should_relabel = true;
                }
                let () = self.set_face_status(*fce).await;
            }
            RelocateFaceSrc(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].start, *new);
                graph.graph.faces[*fce].start = *old;
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceDst(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].end, *new);
                graph.graph.faces[*fce].end = *old;
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceLeft(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].left, *new);
                graph.graph.faces[*fce].left = old.clone();
                ins.eval_status.should_relayout = true;
            }
            RelocateFaceRight(fce, old, new) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                assert_eq!(graph.graph.faces[*fce].right, *new);
                graph.graph.faces[*fce].right = old.clone();
                ins.eval_status.should_relayout = true;
            }
            UpdateFaceLabel(fce, upd) => {
                let graph = &mut self.graph.lock().await;
                let ins = &mut self.ins.lock().await;
                upd.undo(&mut graph.graph.faces[*fce].label);
                ins.eval_status.should_relayout = true;
            }
            RenameFace(fce, prev, new) => {
                let graph = &mut self.graph.lock().await;
                assert_eq!(&graph.graph.faces[*fce].label.name, new);
                graph.graph.faces[*fce].label.name = prev.clone();
                graph.names.remove(new);
                graph.names.insert(prev.clone(), GraphId::Face(*fce));
            }
            DirtyState => self.ins.lock().await.eval_status.should_relabel = true,
        }
    }
}
