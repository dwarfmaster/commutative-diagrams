use crate::data::{Feature, Tag};
use crate::graph::GraphId;
use crate::remote::Remote;
use crate::vm::actions::decompose;
use crate::vm::ast;
use crate::vm::ast::Action;
use crate::vm::vm;
use crate::vm::{CodeStyle, EndStatus, Interactive, VM};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ExecutionResult {
    Success,
    Failure,
    ExecutionError,
    Unfinished,
}

impl<Rm: Remote, I: Interactive> VM<Rm, I> {
    async fn execute(&self, act: ast::Annot<ast::Action>) -> ExecutionResult {
        use Action::*;
        use ExecutionResult::*;
        let mut result = Unfinished;
        let start = self.ins.lock().await.instructions.len();
        match act.value.clone() {
            InsertNode(node) => {
                let node = {
                    let ctx = &mut self.ctx.lock().await;
                    ctx.remote.parse(node.value.clone()).unwrap()
                };
                match node {
                    Ok(node) => {
                        let tps = {
                            let ctx = &mut self.ctx.lock().await;
                            ctx.get_stored_query(node, Tag::Object)
                        };
                        for tp in tps {
                            if let Feature::Object { cat } = tp {
                                self.insert_node(node, cat).await;
                            }
                        }
                    }
                    Err(err) => {
                        self.code.lock().await.error_msg =
                            format!("Couldn't parse object: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphism(mph) => {
                let mph = {
                    let ctx = &mut self.ctx.lock().await;
                    ctx.remote.parse(mph.value.clone()).unwrap()
                };
                match mph {
                    Ok(mph) => {
                        let tps = {
                            let ctx = &mut self.ctx.lock().await;
                            ctx.get_stored_query(mph, Tag::Morphism)
                        };
                        for tp in tps {
                            if let Feature::Morphism { cat, .. } = tp {
                                self.insert_mph(mph, cat).await;
                            }
                        }
                    }
                    Err(err) => {
                        self.code.lock().await.error_msg =
                            format!("Couldn't parse morphism: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            InsertFace(eq) => {
                let eq = {
                    let ctx = &mut self.ctx.lock().await;
                    ctx.remote.parse(eq.value.clone()).unwrap()
                };
                match eq {
                    Ok(eq) => {
                        let tps = {
                            let ctx = &mut self.ctx.lock().await;
                            ctx.get_stored_query(eq, Tag::Equality)
                        };
                        for tp in tps {
                            if let Feature::Equality { cat, .. } = tp {
                                self.insert_eq(eq, cat).await;
                            }
                        }
                    }
                    Err(err) => {
                        self.code.lock().await.error_msg =
                            format!("Couldn't parse equality: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            InsertMorphismAt(node, mph) => {
                let mph = {
                    let ctx = &mut self.ctx.lock().await;
                    ctx.remote.parse(mph.value.clone()).unwrap()
                };
                match mph {
                    Ok(mph) => {
                        let nid = {
                            let graph = self.graph.lock().await;
                            graph.names.get(&node.value).cloned()
                        };
                        if let Some(GraphId::Node(id)) = nid {
                            self.insert_mph_at(id, mph).await;
                        } else {
                            self.code.lock().await.error_msg =
                                format!("{} is not a valid node name", node.value);
                            result = ExecutionError;
                        }
                    }
                    Err(err) => {
                        self.code.lock().await.error_msg =
                            format!("Couldn't parse morphism: {:#?}", err);
                        result = ExecutionError;
                    }
                }
            }
            Split(mph) => {
                let mid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&mph.value).cloned()
                };
                if let Some(GraphId::Morphism(src, mph)) = mid {
                    self.split(src, mph).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid morphism name", mph.value);
                    result = ExecutionError;
                }
            }
            HideNode(n) => {
                let nid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&n.value).cloned()
                };
                if let Some(GraphId::Node(n)) = nid {
                    self.hide(GraphId::Node(n)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid node name", n.value);
                    result = ExecutionError;
                }
            }
            RevealNode(n) => {
                let nid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&n.value).cloned()
                };
                if let Some(GraphId::Node(n)) = nid {
                    self.reveal(GraphId::Node(n)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid node name", n.value);
                    result = ExecutionError;
                }
            }
            HideMorphism(m) => {
                let mid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&m.value).cloned()
                };
                if let Some(GraphId::Morphism(s, m)) = mid {
                    self.hide(GraphId::Morphism(s, m)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid morphism name", m.value);
                    result = ExecutionError;
                }
            }
            RevealMorphism(m) => {
                let mid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&m.value).cloned()
                };
                if let Some(GraphId::Morphism(s, m)) = mid {
                    self.reveal(GraphId::Morphism(s, m)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid morphism name", m.value);
                    result = ExecutionError;
                }
            }
            HideFace(f) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    self.hide(GraphId::Face(f)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            RevealFace(f) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    self.reveal(GraphId::Face(f)).await
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            Solve(size, f) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    if !self.solve_face(f, size.map(|a| a.value).unwrap_or(5)).await {
                        self.code.lock().await.error_msg = format!("Couldn't solve face {}", f);
                        result = ExecutionError;
                    }
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            PullFace(f, size) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    if !self.shrink(f, Some(0), size).await {
                        self.code.lock().await.error_msg =
                            "Couldn't pull previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            PushFace(f, size) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    if !self.shrink(f, size, Some(0)).await {
                        self.code.lock().await.error_msg =
                            "Couldn't push previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            ShrinkFace(f) => {
                let fid = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&f.value).cloned()
                };
                if let Some(GraphId::Face(f)) = fid {
                    if !self.shrink(f, None, None).await {
                        self.code.lock().await.error_msg =
                            "Couldn't shrink previous face".to_string();
                        result = ExecutionError;
                    }
                } else {
                    self.code.lock().await.error_msg =
                        format!("{} is not a valid face name", f.value);
                    result = ExecutionError;
                }
            }
            Lemma(lem, matching) => {
                let lemma = self.find_lemma(&lem.value).await;
                if let Some(lemma) = lemma {
                    let matching = {
                        let ctx = &mut self.ctx.lock().await;
                        let config = self.config.lock().await;
                        let graph = self.graph.lock().await;
                        let lemmas = &mut self.lemmas.lock().await;
                        let () = lemmas.lemmas[lemma].get_pattern(ctx, &config).await;

                        let pattern = lemmas.lemmas[lemma].pattern.lock().await;
                        let matching = matching
                            .iter()
                            .map(|(lem, goal)| {
                                let lemid = pattern
                                    .as_ref()
                                    .and_then(|pat| pat.names.get(&lem.value).cloned());
                                let goalid = graph.names.get(&goal.value);
                                match (lemid, goalid) {
                                    (Some(lem), Some(goal)) => Ok((lem.clone(), goal.clone())),
                                    (None, _) => {
                                        Err(format!("Couldn't find {:#?} in lemma", lem.value))
                                    }
                                    _ => Err(format!("Couldn't find {:#?} in goal", goal.value)),
                                }
                            })
                            .collect::<Result<Vec<_>, String>>();
                        matching
                    };
                    match matching {
                        Ok(matching) => {
                            if !self.apply_lemma(lemma, &matching[..]).await {
                                // error_msg is set by apply_lemma
                                result = ExecutionError;
                            }
                        }
                        Err(msg) => {
                            self.code.lock().await.error_msg = msg;
                            result = ExecutionError;
                        }
                    }
                } else {
                    self.code.lock().await.error_msg = format!("Couldn't find lemma {}", lem.value);
                    result = ExecutionError;
                }
            }
            Merge(name1, name2) => {
                let (id1, id2) = {
                    let graph = self.graph.lock().await;
                    let id1 = graph.names.get(&name1.value).cloned();
                    let id2 = graph.names.get(&name2.value).cloned();
                    (id1, id2)
                };
                if let Some(id1) = id1 {
                    if let Some(id2) = id2 {
                        if !self.merge_dwim(id1, id2).await {
                            self.code.lock().await.error_msg =
                                format!("Couldn't merge {} with {}", name1.value, name2.value);
                            result = ExecutionError;
                        }
                    } else {
                        self.code.lock().await.error_msg = format!("Couldn't find {}", name2.value);
                        result = ExecutionError;
                    }
                } else {
                    self.code.lock().await.error_msg = format!("Couldn't find {}", name1.value);
                    result = ExecutionError;
                }
            }
            Decompose(fce, steps) => {
                let id = {
                    let graph = self.graph.lock().await;
                    graph.names.get(&fce.value).cloned()
                };
                if let Some(GraphId::Face(face)) = id {
                    let steps = {
                        let graph = self.graph.lock().await;
                        let deref_names =
                            |v: Vec<ast::Annot<String>>| -> Result<Vec<(usize, usize)>, String> {
                                v.into_iter()
                                    .map(|name| {
                                        if let Some(GraphId::Morphism(src, mph)) =
                                            graph.names.get(&name.value)
                                        {
                                            Ok((*src, *mph))
                                        } else {
                                            Err(format!("Couldn't find {}", name.value))
                                        }
                                    })
                                    .collect()
                            };
                        let steps: Result<Vec<decompose::Step>, String> = steps
                            .into_iter()
                            .map(|step| {
                                let r = decompose::Step {
                                    start: deref_names(step.start)?,
                                    middle_left: deref_names(step.middle_left)?,
                                    middle_right: deref_names(step.middle_right)?,
                                    end: deref_names(step.end)?,
                                };
                                Ok(r)
                            })
                            .collect();
                        steps
                    };
                    match steps {
                        Ok(steps) => {
                            if !self.decompose_face(face, steps).await {
                                self.code.lock().await.error_msg =
                                    format!("Couldn't decompose face {}", fce.value);
                                result = ExecutionError;
                            }
                        }
                        Err(msg) => {
                            self.code.lock().await.error_msg = msg;
                            result = ExecutionError;
                        }
                    }
                } else {
                    self.code.lock().await.error_msg = format!("Coudn't find face {}", fce.value);
                    result = ExecutionError;
                }
            }
            Succeed => result = Success,
            Fail => result = Failure,
        }
        if result == ExecutionError {
            // Undo all new instructions
            let tail = self.ins.lock().await.instructions.split_off(start);
            for ins in tail.into_iter().rev() {
                self.undo_instruction(&ins).await
            }
            self.ctx
                .lock()
                .await
                .restore_state(*self.code.lock().await.states.last().unwrap());
        } else {
            // Register the action as having been executed
            self.store_action(act, start);
        }
        result
    }

    pub async fn store_action(&self, act: ast::Annot<Action>, from: usize) {
        let code = &mut self.code.lock().await;
        let ins = self.ins.lock().await;
        code.run_until = act.range.end;
        let () = code.reset_style();
        let run_until = code.run_until;
        let () = code.style_range(0..run_until, CodeStyle::Run);
        code.ast.push(vm::Action {
            act: act.value,
            text: act.range,
            asm: from..ins.instructions.len(),
        });
        let status = self.ctx.lock().await.save_state();
        code.states.push(status);
    }

    async fn clear_interactive(&self) {
        let current = {
            let caction = &mut self.current_action.lock().await;
            caction.take()
        };
        if let Some((last_act, act)) = current {
            act.terminate().await;
            // Undo partial execution of the action
            while self.ins.lock().await.instructions.len() > last_act {
                self.pop_instruction();
            }
            {
                let code = self.code.lock().await;
                let state = *code.states.last().unwrap();
                log::trace!("Restoring to {}", state);
                self.ctx
                    .lock()
                    .await
                    .restore_state(*code.states.last().unwrap());
            }
            let () = self.change_state().await;
        }
    }

    // Cancel the current interactive action
    pub async fn stop_interactive(&self) {
        let () = self.initialize_execution().await;
        let () = self.clear_interactive().await;
        let () = self.finalize_execution().await;
    }

    pub async fn run(&self, ast: ast::AST) {
        use ExecutionResult::*;
        let () = self.initialize_execution().await;
        let () = self.clear_interactive().await;
        for a in ast {
            match self.execute(a).await {
                Success => *self.end_status.lock().await = EndStatus::Success,
                Failure => *self.end_status.lock().await = EndStatus::Failure,
                _ => (),
            }
        }
        let () = self.finalize_execution().await;
        let code = &mut self.code.lock().await;
        code.prev_code = code.code.clone();
    }

    // On code change, undo all actions that were downstream the edit
    pub async fn sync_code(&self) {
        let first_modified = {
            let code = &mut self.code.lock().await;
            // Find first change
            let prevlen = code.prev_code.len();
            let mut first_change_id: usize = code.code.len().min(prevlen);
            for i in 0..code.code.len().min(prevlen) {
                if code.code.as_bytes()[i] != code.prev_code.as_bytes()[i] {
                    first_change_id = i;
                    break;
                }
            }

            // If nothing has changed in the part that has been run, there is
            // nothing to do
            if first_change_id >= code.run_until {
                return;
            }

            // Find first modified action
            let first_modified = code
                .ast
                .binary_search_by(|act: &vm::Action| {
                    use std::cmp::Ordering::*;
                    if first_change_id < act.text.start {
                        Greater
                    } else if first_change_id >= act.text.end {
                        Less
                    } else {
                        Equal
                    }
                })
                .unwrap_or_else(|i| i);
            if first_modified >= code.ast.len() {
                return;
            }
            first_modified
        };

        let () = self.undo_until(first_modified).await;
    }

    // Keep the first keep actions, undoing all the others
    pub async fn undo_until(&self, keep: usize) {
        let (status, tail) = {
            let code = &mut self.code.lock().await;
            // Undo all these actions and remove them from the ast
            let tail = code.ast.split_off(keep);
            let status = code.states[keep];
            code.states.truncate(keep + 1);
            (status, tail)
        };
        let () = self.initialize_execution().await;
        let () = self.clear_interactive().await;
        for act in tail.iter().rev() {
            for _ in act.asm.clone() {
                let () = self.pop_instruction().await;
            }
        }
        let () = self.ctx.lock().await.restore_state(status);
        let () = self.finalize_execution().await;

        // Update run_until
        let code = &mut self.code.lock().await;
        code.reset_style();
        let end = code.ast.last().map(|lst| lst.text.end);
        if let Some(end) = end {
            code.run_until = end;
            code.style_range(0..end, CodeStyle::Run);
        } else {
            code.run_until = 0;
        }
    }
}
