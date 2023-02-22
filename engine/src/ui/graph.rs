use crate::vm::{GraphId, VM};
use egui::{Pos2, Vec2};
use std::ops::Add;

pub fn graph_widget(ui: &mut egui::Ui, vm: &mut VM) -> egui::Response {
    // We fill all the available space
    let desired_size = ui.available_size();
    // For now it is non interactive, but we may want to react to click and drag
    // on nodes and edges to allow manual placement of the graph. That is why we
    // take the graph as mutable.
    let response = ui.allocate_response(
        desired_size,
        egui::Sense {
            click: true,
            drag: true,
            focusable: true,
        },
    );
    let rect = response.rect;

    // When drawing we compute the closest element to the hover cursor and its distance,
    // and print information in a tooltip
    let mut closest_distance = f32::INFINITY;
    let mut closest_object = GraphId::Node(0);
    let hover_pos = response.hover_pos();

    if response.clicked() {
        response.request_focus();
    }
    if response.dragged() {
        vm.offset += response.drag_delta();
        response.request_focus();
    }
    if response.has_focus() {
        // zoom
        // vm.zoom += ui.input().zoom_delta();
        if ui.input().key_pressed(egui::Key::K) {
            vm.zoom *= 1.1;
        }
        if ui.input().key_pressed(egui::Key::J) {
            vm.zoom /= 1.1;
        }
        if ui.input().key_pressed(egui::Key::Num0) {
            vm.zoom = 1.0;
        }

        // offset
        vm.offset += ui.input().scroll_delta;
        let offset_delta = 50.0;
        if ui.input().key_pressed(egui::Key::ArrowUp) {
            vm.offset.y -= offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowDown) {
            vm.offset.y += offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowLeft) {
            vm.offset.x -= offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowRight) {
            vm.offset.x += offset_delta;
        }

        // Reset
        if ui.input().modifiers.ctrl && ui.input().key_pressed(egui::Key::Num0) {
            vm.zoom = 1.0;
            vm.offset = Vec2::ZERO;
        }
    }

    if ui.is_rect_visible(rect) {
        let offset = rect.left_top().add(vm.offset).to_vec2();
        let visuals = ui.style().noninteractive();
        let rect = rect.expand(visuals.expansion);
        let stroke = if response.has_focus() {
            visuals.fg_stroke
        } else {
            visuals.bg_stroke
        };
        ui.painter().rect(rect, 3.0, visuals.bg_fill, stroke);

        let painter = ui.painter().with_clip_rect(rect);
        let mut bg_stroke = visuals.bg_stroke;
        bg_stroke.width *= vm.zoom;
        let mut fg_stroke = visuals.fg_stroke;
        fg_stroke.width *= vm.zoom;

        let setup_pos = |p: Pos2| -> Pos2 { (vm.zoom * p.to_vec2()).to_pos2().add(offset) };

        // Paint nodes
        for (n, (_, label)) in vm.graph.nodes.iter().enumerate() {
            let pos = setup_pos(label.pos.clone());
            let name = &label.label;
            let size = 14.0 * vm.zoom;
            painter.text(
                pos,
                egui::Align2::CENTER_CENTER,
                name,
                egui::FontId::proportional(size),
                fg_stroke.color,
            );

            // Compute distance to hover_pos
            if let Some(hpos) = hover_pos {
                let dist = pos.distance(hpos);
                if dist < closest_distance {
                    closest_distance = dist;
                    closest_object = GraphId::Node(n);
                }
            }
        }

        // Paint edges
        for src in 0..vm.graph.nodes.len() {
            for (mph, (_, label, _)) in vm.graph.edges[src].iter().enumerate() {
                // Style
                let mut stroke = fg_stroke;
                if label.style.highlight {
                    stroke.width *= 2.0;
                }
                if label.style.left && label.style.right {
                    stroke.color = egui::Color32::GOLD;
                } else if label.style.left {
                    stroke.color = egui::Color32::RED;
                } else if label.style.right {
                    stroke.color = egui::Color32::GREEN;
                }

                // paint label
                let lbl_rect = painter.text(
                    setup_pos(label.label_pos),
                    egui::Align2::CENTER_CENTER,
                    label.label.clone(),
                    egui::FontId::proportional(14.0 * vm.zoom),
                    fg_stroke.color,
                );
                // If the label is hovered, change style
                if let Some(hpos) = hover_pos {
                    if lbl_rect.contains(hpos) {
                        closest_distance = 0.0;
                        closest_object = GraphId::Morphism(src, mph);
                        stroke.width *= 2.0;
                    }
                }

                let curves = &label.shape;
                let curves = curves
                    .iter()
                    .map(|curve| egui::epaint::CubicBezierShape {
                        points: curve.clone().map(setup_pos),
                        closed: false,
                        fill: egui::Color32::TRANSPARENT,
                        stroke,
                    })
                    .collect::<Vec<_>>();
                // Paint the curve
                for curve in &curves {
                    painter.add(*curve);

                    // Compute distance
                    // Since it is expensive we first check if hover_pos is in
                    // the bounding rect of the curve.
                    if let Some(hpos) = hover_pos {
                        if curve.visual_bounding_rect().contains(hpos) {
                            let mut dist = f32::INFINITY;
                            curve.for_each_flattened_with_t(1.0, &mut |p: Pos2, _: f32| {
                                let d = p.distance(hpos);
                                if d < dist {
                                    dist = d;
                                }
                            });
                            if dist < closest_distance {
                                closest_distance = dist;
                                closest_object = GraphId::Morphism(src, mph);
                            }
                        }
                    }
                }

                // Paint the arrow
                if let Some(lcurve) = &curves.last() {
                    use egui::emath::*;
                    let tip = lcurve.points[3];
                    let dir = (lcurve.points[3] - lcurve.points[2]).normalized();
                    let rot = if label.style.highlight {
                        Rot2::from_angle(std::f32::consts::TAU / 6.0)
                    } else {
                        Rot2::from_angle(std::f32::consts::TAU / 12.0)
                    };
                    let length = 5.0 * vm.zoom;
                    painter.line_segment([tip, tip - length * (rot * dir)], stroke);
                    painter.line_segment([tip, tip - length * (rot.inverse() * dir)], stroke);
                }
            }
        }

        // Paint tooltip
        if hover_pos.is_some() && closest_distance < 20.0 * vm.zoom {
            egui::show_tooltip_at_pointer(ui.ctx(), egui::Id::new("id tooltip"), |ui| {
                let label = match closest_object {
                    GraphId::Node(n) => {
                        let node = &vm.graph.nodes[n].1;
                        format!(
                            "node {}[{}] : {}",
                            node.name.as_ref().unwrap_or(&"".to_string()),
                            n,
                            node.label
                        )
                    }
                    GraphId::Morphism(src, dst) => {
                        let edge = &vm.graph.edges[src][dst].1;
                        format!(
                            "morphism {}[{}] : {}",
                            edge.name.as_ref().unwrap_or(&"".to_string()),
                            edge.id,
                            edge.label
                        )
                    }
                    _ => panic!(),
                };
                ui.label(label)
            });
        }
    }

    response
}

// For more idiomatic usage
pub fn graph<'a>(vm: &'a mut VM) -> impl egui::Widget + 'a {
    move |ui: &mut egui::Ui| graph_widget(ui, vm)
}
