use crate::graph::Graph;
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
>(
    ui: &mut egui::Ui,
    graph: &mut Graph<NodeLabel, EdgeLabel, FaceLabel>,
    offset: &mut Vec2,
    node_pos: PosGetter,
    node_name: NodeNameGetter,
    path_curve: PathGetter,
    path_name: PathNameGetter,
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
    let mut response = ui.allocate_response(desired_size, egui::Sense::focusable_noninteractive());
    let rect = response.rect;

    // We ignore response for now

    if ui.is_rect_visible(rect) {
        let offset = rect.left_top().add(*offset).to_vec2();
        let visuals = ui.style().noninteractive();
        let rect = rect.expand(visuals.expansion);
        ui.painter()
            .rect(rect, 3.0, visuals.bg_fill, visuals.bg_stroke);
        let painter = ui.painter().with_clip_rect(rect);

        // Paint nodes
        for (_, label) in &graph.nodes {
            let radius = 2.5;
            let pos = label.view_ref(node_pos).clone().add(offset);
            painter.circle(pos, radius, visuals.fg_stroke.color, visuals.fg_stroke);
            // TODO paint label
        }

        // Paint edges
        for src in 0..graph.nodes.len() {
            for (_, label, _) in &graph.edges[src] {
                let curves = label.traverse_ref(path_curve);
                // Paint the curve
                for curve in &curves {
                    let curve: [Pos2; 4] = curve.map(|p| p.add(offset));
                    let shape = egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
                        points: curve,
                        closed: false,
                        fill: egui::Color32::TRANSPARENT,
                        stroke: visuals.fg_stroke,
                    });
                    painter.add(shape);
                }

                // Paint the arrow
                if let Some(lcurve) = &curves.last() {
                    use egui::emath::*;
                    let tip = lcurve[3].add(offset);
                    let dir = (lcurve[3] - lcurve[2]).normalized();
                    let rot = Rot2::from_angle(std::f32::consts::TAU / 12.0);
                    let length = 5.0;
                    painter.line_segment([tip, tip - length * (rot * dir)], visuals.fg_stroke);
                    painter.line_segment(
                        [tip, tip - length * (rot.inverse() * dir)],
                        visuals.fg_stroke,
                    );
                }

                // TODO paint label
            }
        }
    }

    response
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
>(
    graph: &'a mut Graph<NodeLabel, EdgeLabel, FaceLabel>,
    offset: &'a mut Vec2,
    node_pos: PosGetter,
    node_name: NodeNameGetter,
    path_curve: PathGetter,
    path_name: PathNameGetter,
) -> impl egui::Widget + 'a
where
    NodeLabel: LensMut<PosGetter, Pos2> + Lens<NodeNameGetter, String>,
    // An edge is drawn as a succession of bezier curves
    EdgeLabel: TraversalMut<PathGetter, [Pos2; 4]> + Lens<PathNameGetter, String>,
{
    move |ui: &mut egui::Ui| {
        graph_widget(
            ui, graph, offset, node_pos, node_name, path_curve, path_name,
        )
    }
}
