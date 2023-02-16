use crate::ui::GraphDisplay;
use egui::{Pos2, Vec2};
use lens_rs::{Lens, LensMut, TraversalMut};
use std::ops::Add;

pub fn graph_widget<
    NodeLabel,
    EdgeLabel,
    FaceLabel,
    PosGetter: Copy,
    NodeNameGetter: Copy,
    PathGetter: Copy,
    PathNameGetter: Copy,
    FaceNameGetter,
>(
    ui: &mut egui::Ui,
    gd: &mut GraphDisplay<
        NodeLabel,
        EdgeLabel,
        FaceLabel,
        PosGetter,
        NodeNameGetter,
        PathGetter,
        PathNameGetter,
        FaceNameGetter,
    >,
) -> egui::Response
where
    NodeLabel: LensMut<PosGetter, Pos2> + Lens<NodeNameGetter, String>,
    // An edge is drawn as a succession of bezier curves
    EdgeLabel: TraversalMut<PathGetter, [Pos2; 4]> + Lens<PathNameGetter, String>,
{
    // We fill all the available space
    let desired_size = ui.available_size();
    // For now it is non interactive, but we may want to react to click and drag
    // on nodes and edges to allow manual placement of the graph. That is why we
    // take the graph as mutable, and the lenses as mutable.
    let response = ui.allocate_response(
        desired_size,
        egui::Sense {
            click: true,
            drag: true,
            focusable: true,
        },
    );
    let rect = response.rect;

    if response.clicked() {
        response.request_focus();
    }
    if response.dragged() {
        gd.offset += response.drag_delta();
        response.request_focus();
    }
    if response.has_focus() {
        // zoom
        // gd.zoom += ui.input().zoom_delta();
        if ui.input().key_pressed(egui::Key::K) {
            gd.zoom *= 1.1;
        }
        if ui.input().key_pressed(egui::Key::J) {
            gd.zoom /= 1.1;
        }
        if ui.input().key_pressed(egui::Key::Num0) {
            gd.zoom = 1.0;
        }

        // offset
        gd.offset += ui.input().scroll_delta;
        let offset_delta = 50.0;
        if ui.input().key_pressed(egui::Key::ArrowUp) {
            gd.offset.y -= offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowDown) {
            gd.offset.y += offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowLeft) {
            gd.offset.x -= offset_delta;
        }
        if ui.input().key_pressed(egui::Key::ArrowRight) {
            gd.offset.x += offset_delta;
        }

        // Reset
        if ui.input().modifiers.ctrl && ui.input().key_pressed(egui::Key::Num0) {
            gd.zoom = 1.0;
            gd.offset = Vec2::ZERO;
        }
    }

    if ui.is_rect_visible(rect) {
        let offset = rect.left_top().add(gd.offset).to_vec2();
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
        bg_stroke.width *= gd.zoom;
        let mut fg_stroke = visuals.fg_stroke;
        fg_stroke.width *= gd.zoom;

        let setup_pos = |p: Pos2| -> Pos2 { (gd.zoom * p.to_vec2()).to_pos2().add(offset) };

        // Paint nodes
        for (_, label) in &gd.graph.nodes {
            let pos = setup_pos(label.view_ref(gd.node_pos).clone());
            let name = label.view_ref(gd.node_name);
            let size = 14.0 * gd.zoom;
            painter.text(
                pos,
                egui::Align2::CENTER_CENTER,
                name,
                egui::FontId::proportional(size),
                fg_stroke.color,
            );
        }

        // Paint edges
        for src in 0..gd.graph.nodes.len() {
            for (_, label, _) in &gd.graph.edges[src] {
                let curves = label.traverse_ref(gd.path_curve);
                let curves = curves
                    .iter()
                    .map(|curve| egui::epaint::CubicBezierShape {
                        points: curve.clone().map(setup_pos),
                        closed: false,
                        fill: egui::Color32::TRANSPARENT,
                        stroke: fg_stroke,
                    })
                    .collect::<Vec<_>>();
                // Paint the curve
                for curve in &curves {
                    painter.add(*curve);
                }

                // Paint the arrow
                if let Some(lcurve) = &curves.last() {
                    use egui::emath::*;
                    let tip = lcurve.points[3];
                    let dir = (lcurve.points[3] - lcurve.points[2]).normalized();
                    let rot = Rot2::from_angle(std::f32::consts::TAU / 12.0);
                    let length = 5.0 * gd.zoom;
                    painter.line_segment([tip, tip - length * (rot * dir)], fg_stroke);
                    painter.line_segment([tip, tip - length * (rot.inverse() * dir)], fg_stroke);
                }

                // paint label
                let lengths = curves
                    .iter()
                    .map(|cbc| cubic_length(&cbc))
                    .scan(0.0, |acc, x| {
                        *acc = *acc + x;
                        Some(*acc)
                    })
                    .collect::<Vec<_>>();
                if let Some(total) = lengths.last() {
                    let half = total / 2.0;
                    let middle = lengths
                        .binary_search_by(|v| {
                            v.partial_cmp(&half).expect("Couldn't compare floats")
                        })
                        .unwrap_or_else(|pos| pos);
                    let dir = -cubic_derivative(&curves[middle], 0.5).normalized().rot90();
                    let pos = curves[middle].sample(0.5) + gd.zoom * 8.0 * dir;
                    let name = label.view_ref(gd.path_name);
                    let layout = painter.layout_no_wrap(
                        name.to_string(),
                        egui::FontId::proportional(14.0 * gd.zoom),
                        fg_stroke.color,
                    );
                    let rvec = layout.rect.size() * 0.5;
                    let alpha = if dir.x.abs() <= 1e-6 {
                        rvec.y / dir.y.abs()
                    } else if dir.y.abs() <= 1e-6 {
                        rvec.x / dir.x.abs()
                    } else {
                        (rvec.x / dir.x.abs()).min(rvec.y / dir.y.abs())
                    };
                    painter.circle(pos, 5.0, bg_stroke.color, fg_stroke);
                    painter.galley(pos - rvec + alpha * dir, layout);
                }
            }
        }
    }

    response
}

// Helpers
fn cubic_length(curve: &egui::epaint::CubicBezierShape) -> f32 {
    let approx = curve.points[0].distance(curve.points[3]) / 100.0;
    curve
        .flatten(Some(approx))
        .windows(2)
        .map(|win| win[0].distance(win[1]))
        .sum()
}

// Taken from:
//   https://stackoverflow.com/questions/4089443/find-the-tangent-of-a-point-on-a-cubic-bezier-curve
fn cubic_derivative(curve: &egui::epaint::CubicBezierShape, t: f32) -> Vec2 {
    let v0 = curve.points[0].to_vec2();
    let v1 = curve.points[1].to_vec2();
    let v2 = curve.points[2].to_vec2();
    let v3 = curve.points[3].to_vec2();
    let f0 = -3.0 * (1.0 - t) * (1.0 - t);
    let f1 = 3.0 * (1.0 - t) * (1.0 - t) - 6.0 * t * (1.0 - t);
    let f2 = -3.0 * t * t + 6.0 * t * (1.0 - t);
    let f3 = 3.0 * t * t;
    f0 * v0 + f1 * v1 + f2 * v2 + f3 * v3
}

// For more idiomatic usage
pub fn graph<
    'a,
    NodeLabel,
    EdgeLabel,
    FaceLabel,
    PosGetter: Copy + 'a,
    NodeNameGetter: Copy + 'a,
    PathGetter: Copy + 'a,
    PathNameGetter: Copy + 'a,
    FaceNameGetter,
>(
    gd: &'a mut GraphDisplay<
        NodeLabel,
        EdgeLabel,
        FaceLabel,
        PosGetter,
        NodeNameGetter,
        PathGetter,
        PathNameGetter,
        FaceNameGetter,
    >,
) -> impl egui::Widget + 'a
where
    NodeLabel: LensMut<PosGetter, Pos2> + Lens<NodeNameGetter, String>,
    // An edge is drawn as a succession of bezier curves
    EdgeLabel: TraversalMut<PathGetter, [Pos2; 4]> + Lens<PathNameGetter, String>,
{
    move |ui: &mut egui::Ui| graph_widget(ui, gd)
}
