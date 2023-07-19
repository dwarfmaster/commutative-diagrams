use super::faces::faces_in_rect;
use super::graph::{Action, ArrowStyle, Drawable, Modifier, UiGraph};
use crate::graph::GraphId;
use egui::{Pos2, Rect, Vec2};
use std::ops::Add;

// Keep max
fn set_width_right(mut r: Rect, w: f32) -> Rect {
    r.set_left(r.right() - w);
    r
}

fn graph_widget<G: UiGraph>(ui: &mut egui::Ui, gr: &mut G) -> egui::Response {
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
    let pointer_pos = if let Some(hover) = response.hover_pos() {
        Some(hover)
    } else {
        response.interact_pointer_pos()
    };

    if response.clicked() {
        *gr.focused() = None;
        response.request_focus();
    }
    if response.middle_clicked() {
        *gr.focused() = None;
        response.request_focus();
    }
    if response.dragged() {
        *gr.focused() = None;
        *gr.offset() += response.drag_delta();
        response.request_focus();
    }
    if response.lost_focus() {
        *gr.focused() = None;
    }
    if response.has_focus() {
        // zoom
        // vm.zoom += ui.input().zoom_delta();
        if ui.input(|i| i.key_pressed(egui::Key::K)) {
            *gr.zoom() *= 1.1;
        }
        if ui.input(|i| i.key_pressed(egui::Key::J)) {
            *gr.zoom() /= 1.1;
        }
        if ui.input(|i| i.key_pressed(egui::Key::Num0)) {
            *gr.zoom() = 1.0;
        }

        // offset
        *gr.offset() += ui.input(|i| i.scroll_delta);
        let offset_delta = 50.0;
        if ui.input(|i| i.key_pressed(egui::Key::ArrowUp)) {
            gr.offset().y -= offset_delta;
        }
        if ui.input(|i| i.key_pressed(egui::Key::ArrowDown)) {
            gr.offset().y += offset_delta;
        }
        if ui.input(|i| i.key_pressed(egui::Key::ArrowLeft)) {
            gr.offset().x -= offset_delta;
        }
        if ui.input(|i| i.key_pressed(egui::Key::ArrowRight)) {
            gr.offset().x += offset_delta;
        }

        // Reset
        if ui.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num0)) {
            *gr.zoom() = 1.0;
            *gr.offset() = Vec2::ZERO;
        }
    }

    if ui.is_rect_visible(rect) {
        let offset = rect.left_top().add(*gr.offset()).to_vec2();
        let visuals = ui.style().noninteractive();
        let rect = rect.expand(visuals.expansion);
        let stroke = if response.has_focus() {
            visuals.fg_stroke
        } else {
            visuals.bg_stroke
        };
        ui.painter().rect(rect, 3.0, visuals.bg_fill, stroke);

        let painter = ui.painter().with_clip_rect(rect);
        let zoom = *gr.zoom();
        let setup_pos = |p: Pos2| -> Pos2 { (zoom * p.to_vec2()).to_pos2().add(offset) };

        // Paint graph
        gr.draw(ui.style(), |dr, mut stroke, md, id| {
            stroke.width *= match md {
                Modifier::None => zoom,
                Modifier::Highlight => zoom * 2.0,
            };

            match dr {
                Drawable::Text(p, label) => {
                    let rect = painter.text(
                        setup_pos(p),
                        egui::Align2::CENTER_CENTER,
                        label,
                        egui::FontId::proportional(14.0),
                        stroke.color,
                    );

                    if let Some(ppos) = pointer_pos {
                        if rect.contains(ppos) {
                            closest_distance = 0.0;
                            closest_object = id;
                        }
                    }
                }

                Drawable::Curve(curve, _, arrow) => {
                    let curve = egui::epaint::CubicBezierShape {
                        points: curve.clone().map(setup_pos),
                        closed: false,
                        fill: egui::Color32::TRANSPARENT,
                        stroke,
                    };
                    painter.add(curve);

                    if arrow != ArrowStyle::None {
                        use egui::emath::*;
                        let tip = curve.points[3];
                        let dir = (curve.points[3] - curve.points[2]).normalized();
                        let rot = Rot2::from_angle(std::f32::consts::TAU / 12.0);
                        let length = 5.0 * zoom;
                        painter.line_segment([tip, tip - length * (rot * dir)], stroke);
                        painter.line_segment([tip, tip - length * (rot.inverse() * dir)], stroke);
                    }

                    if let Some(ppos) = pointer_pos {
                        if curve.visual_bounding_rect().expand(10.0).contains(ppos) {
                            let mut dist = f32::INFINITY;
                            curve.for_each_flattened_with_t(5.0, &mut |p: Pos2, _: f32| {
                                let d = p.distance(ppos);
                                if d < dist {
                                    dist = d
                                }
                            });
                            if dist < closest_distance {
                                closest_distance = dist;
                                closest_object = id;
                            }
                        }
                    }
                }
            }
        });

        // Faces
        let faces_rect = set_width_right(rect.shrink(10.0), 210.0_f32.min(rect.width() / 2.0));
        painter.rect(faces_rect, 3.0, visuals.bg_fill, visuals.bg_stroke);
        let faces_rect = faces_rect.shrink(5.0);
        faces_in_rect(
            ui,
            painter.with_clip_rect(faces_rect),
            faces_rect,
            gr,
            pointer_pos,
            &mut closest_object,
            &mut closest_distance,
        );

        // Notify graph of actions
        if closest_distance < 20.0 * zoom {
            if response.double_clicked() {
                gr.action(Action::DoubleClick(closest_object), ui);
            } else if response.clicked() {
                gr.action(Action::Click(closest_object), ui);
            } else if response.hovered() {
                gr.action(Action::Hover(closest_object), ui);
            } else {
                gr.action(Action::None, ui);
            }
        } else {
            gr.action(Action::None, ui);
        }

        // Setup context (right-click) menu
        response.clone().context_menu(|ui| {
            if gr.focused().is_none() {
                if closest_distance < 20.0 * zoom {
                    *gr.focused() = Some(closest_object);
                } else {
                    ui.close_menu();
                }
            }
            if let Some(id) = *gr.focused() {
                if !gr.context_menu(id, ui) {
                    *gr.focused() = None;
                }
            } else {
                ui.close_menu();
            }
        });
    }

    response
}

// For more idiomatic usage
pub fn graph<'a, G: UiGraph>(gr: &'a mut G) -> impl egui::Widget + 'a {
    move |ui: &mut egui::Ui| graph_widget(ui, gr)
}
