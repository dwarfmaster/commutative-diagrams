use crate::graph::Graph;
use bevy::ecs::system::Resource;
use egui::{Pos2, Vec2};
use lens_rs::{Lens, LensMut, TraversalMut};
use std::ops::Add;
use std::ops::Deref;

#[derive(Resource)]
pub struct GraphDisplay<
    NodeLabel,
    EdgeLabel,
    FaceLabel,
    PosGetter,
    NodeNameGetter,
    PathGetter,
    PathNameGetter,
> {
    graph: Graph<NodeLabel, EdgeLabel, FaceLabel>,
    node_pos: PosGetter,
    node_name: NodeNameGetter,
    path_curve: PathGetter,
    path_name: PathNameGetter,
    offset: Vec2,
    zoom: f32,
}

impl<NL, EL, FL, PG, NNG, PtG, PtNG> GraphDisplay<NL, EL, FL, PG, NNG, PtG, PtNG> {
    pub fn new(graph: Graph<NL, EL, FL>, np: PG, nn: NNG, ptc: PtG, ptn: PtNG) -> Self
    where
        NL: LensMut<PG, Pos2> + Lens<NNG, String>,
        EL: TraversalMut<PtG, [Pos2; 4]> + Lens<PtNG, String>,
        PG: Copy,
        NNG: Copy,
        PtG: Copy,
        PtNG: Copy,
    {
        GraphDisplay {
            graph,
            node_pos: np,
            node_name: nn,
            path_curve: ptc,
            path_name: ptn,
            offset: Vec2::ZERO,
            zoom: 1.0,
        }
    }
}

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
    gd: &mut GraphDisplay<
        NodeLabel,
        EdgeLabel,
        FaceLabel,
        PosGetter,
        NodeNameGetter,
        PathGetter,
        PathNameGetter,
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
        }
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

        let setup_pos = |p: Pos2| -> Pos2 {
            (gd.zoom * p.to_vec2()).to_pos2().add(offset)
        };

        // Paint nodes
        for (_, label) in &gd.graph.nodes {
            let radius = 2.5 * gd.zoom;
            let pos = setup_pos(label.view_ref(gd.node_pos).clone());
            painter.circle(pos, radius, fg_stroke.color, fg_stroke);
            // TODO paint label
        }

        // Paint edges
        for src in 0..gd.graph.nodes.len() {
            for (_, label, _) in &gd.graph.edges[src] {
                let curves = label.traverse_ref(gd.path_curve);
                // Paint the curve
                for curve in &curves {
                    let curve: [Pos2; 4] = curve.map(setup_pos);
                    let shape = egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
                        points: curve,
                        closed: false,
                        fill: egui::Color32::TRANSPARENT,
                        stroke: fg_stroke,
                    });
                    painter.add(shape);
                }

                // Paint the arrow
                if let Some(lcurve) = &curves.last() {
                    use egui::emath::*;
                    let tip = setup_pos(lcurve[3]);
                    let dir = (lcurve[3] - lcurve[2]).normalized();
                    let rot = Rot2::from_angle(std::f32::consts::TAU / 12.0);
                    let length = 5.0 * gd.zoom;
                    painter.line_segment([tip, tip - length * (rot * dir)], fg_stroke);
                    painter.line_segment(
                        [tip, tip - length * (rot.inverse() * dir)],
                        fg_stroke,
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
    gd: &'a mut GraphDisplay<
        NodeLabel,
        EdgeLabel,
        FaceLabel,
        PosGetter,
        NodeNameGetter,
        PathGetter,
        PathNameGetter,
    >,
) -> impl egui::Widget + 'a
where
    NodeLabel: LensMut<PosGetter, Pos2> + Lens<NodeNameGetter, String>,
    // An edge is drawn as a succession of bezier curves
    EdgeLabel: TraversalMut<PathGetter, [Pos2; 4]> + Lens<PathNameGetter, String>,
{
    move |ui: &mut egui::Ui| graph_widget(ui, gd)
}
