use crate::remote::Remote;
use crate::vm::FaceStatus;
use crate::vm::{Interactive, VM};
use std::cmp::Ordering;

fn cmp_status(st1: FaceStatus, st2: FaceStatus) -> Ordering {
    if st1 != FaceStatus::Hypothesis && st2 == FaceStatus::Hypothesis {
        Ordering::Less
    } else if st1 == FaceStatus::Hypothesis && st2 != FaceStatus::Hypothesis {
        Ordering::Greater
    } else {
        Ordering::Equal
    }
}

impl<Rm: Remote + Sync + Send, I: Interactive + Sync + Send> VM<Rm, I> {
    pub fn init_face_order(&mut self) {
        let mut faces_ids = (0..self.graph.faces.len()).collect::<Vec<_>>();
        faces_ids.sort_by(|id1, id2| {
            let st = cmp_status(
                self.graph.faces[*id1].label.status,
                self.graph.faces[*id2].label.status,
            );
            if st == Ordering::Equal {
                self.graph.faces[*id1]
                    .label
                    .name
                    .cmp(&self.graph.faces[*id2].label.name)
            } else {
                st
            }
        });

        let split = faces_ids
            .binary_search_by_key(&1, |id| {
                if self.graph.faces[*id].label.status == FaceStatus::Hypothesis {
                    1
                } else {
                    0
                }
            })
            .unwrap_or_else(|x| x);
        self.face_hyps_order = faces_ids.split_off(split);
        self.face_goal_order = faces_ids;
    }

    // Return Ok(id) if it is in goals, and Err(id) otherwise
    fn locate_face_order(&self, fce: usize) -> Result<usize, usize> {
        for id in 0..self.face_goal_order.len() {
            if self.face_goal_order[id] == fce {
                return Ok(id);
            }
        }
        for id in 0..self.face_hyps_order.len() {
            if self.face_hyps_order[id] == fce {
                return Err(id);
            }
        }
        log::warn!("Failed to find face {}", fce);
        panic!("All faces should be in order");
    }

    pub fn order_new_face(&mut self, fce: usize) {
        // If it has a parent, we add it after the last child of the parent, or
        // after the parent if it is the first child
        if let Some(parent) = self.graph.faces[fce].label.parent {
            let after = self.graph.faces[parent]
                .label
                .children
                .iter()
                .copied()
                .filter(|f| *f != fce)
                .map(|fce| self.locate_face_order(fce).unwrap_or_else(|x| x))
                .max();
            let parent_pos = self.locate_face_order(parent);
            match parent_pos {
                Ok(pos) => {
                    let pos = after.unwrap_or(pos) + 1;
                    self.face_goal_order.insert(pos, fce);
                }
                Err(pos) => {
                    let pos = after.unwrap_or(pos) + 1;
                    self.face_hyps_order.insert(pos, fce);
                }
            }
            return;
        }

        // Otherwise, we add it at the end of either hyps or goals depending on
        // wether it contains existentials
        if self.graph.faces[fce].label.status == FaceStatus::Hypothesis {
            self.face_hyps_order.push(fce);
            return;
        } else {
            self.face_goal_order.push(fce);
            return;
        }
    }

    pub fn order_rm_face(&mut self, fce: usize) {
        log::debug!("Removing face {}", fce);
        let pos = self.locate_face_order(fce);
        match pos {
            Ok(pos) => self.face_goal_order.remove(pos),
            Err(pos) => self.face_hyps_order.remove(pos),
        };
    }
}
