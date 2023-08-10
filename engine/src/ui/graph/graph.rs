use crate::graph::GraphId;
use egui::{Color32, Pos2, Rect, Stroke, Style, Ui, Vec2};
use std::sync::Arc;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum CurveStyle {
    Simple,
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum ArrowStyle {
    None,
    Simple,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Drawable<'a> {
    Text(Pos2, &'a str),
    // Quadratic bezier curve
    Curve([Pos2; 3], CurveStyle, ArrowStyle),
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum Modifier {
    None,
    Highlight,
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct FaceContent<'a> {
    pub name: &'a str,
    pub content: &'a str,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct FaceStyle {
    pub fill: Color32,
    pub border: Stroke,
    pub sep: Stroke,
    pub text: Color32,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Action {
    None,
    Hover(GraphId),
    Click(GraphId),
    DoubleClick(GraphId),
    Drag(GraphId, Pos2, Vec2),
    DragRelease(GraphId),
}

pub trait UiGraph {
    // Draw the graph
    fn draw<'a, F>(&'a self, style: &Arc<Style>, f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId) -> Rect;
    // Draw the faces
    fn faces<'a, F>(&'a self, style: &Arc<Style>, f: F)
    where
        F: FnMut(GraphId, FaceContent<'a>, bool, FaceStyle);

    fn zoom<'a>(&'a mut self) -> &'a mut f32;
    fn offset<'a>(&'a mut self) -> &'a mut Vec2;
    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId>;
    fn dragged<'a>(&'a mut self) -> &'a mut Option<GraphId>;
    fn face_folded<'a>(&'a mut self, fce: usize) -> &'a mut bool;
    // Called at every frame
    fn action(&mut self, act: Action, ui: &mut Ui);
    // Setup right-click menu. Must returns false when the menu is closed
    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool;
}

// Helpers
pub fn compute_quadratic_at(q0: Pos2, q1: Pos2, q2: Pos2, t: f32) -> Pos2 {
    let v = (1f32 - t) * (1f32 - t) * q0.to_vec2()
        + 2f32 * (1f32 - t) * t * q1.to_vec2()
        + t * t * q2.to_vec2();
    v.to_pos2()
}

pub fn edge_label_pos(src: Pos2, dst: Pos2, control: Pos2) -> Pos2 {
    let center = src + 0.5f32 * (dst - src);
    let v = compute_quadratic_at(src, control, dst, 0.5f32) - center;
    let dist_to_edge = 30.0f32;
    if v.length_sq() >= 25.0f32 {
        center + v + dist_to_edge * v.normalized()
    } else {
        center + dist_to_edge * (dst - src).normalized().rot90()
    }
}

// Given a rectangle and a vector, give the intersection of the ray starting
// from the center of the rectangle in the direction of the vector with the
// rectangle. Inspired by:
//   https://gamedev.stackexchange.com/questions/203608/find-where-two-mirrored-2d-vectors-intersect-a-rectangle-bounds
pub fn ray_rect(ray: Vec2, rect: Rect) -> Pos2 {
    let w = rect.width() / 2f32;
    let h = rect.height() / 2f32;
    let v1 = (w / ray.x).abs();
    let v2 = (h / ray.y).abs();
    let v = v1.min(v2);
    rect.center() + v * ray
}
