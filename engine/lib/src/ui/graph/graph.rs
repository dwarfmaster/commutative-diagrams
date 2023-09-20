use crate::graph::GraphId;
use egui::{Color32, Pos2, Rect, Rounding, Stroke, Style, Ui, Vec2};
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
pub struct TextStyle {
    pub color: Option<Color32>,
    pub underline: bool,
}

impl TextStyle {
    pub fn new() -> Self {
        Self {
            color: None,
            underline: false,
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Drawable<'a> {
    Text(Pos2, &'a str, TextStyle),
    // Quadratic bezier curve
    Curve([Pos2; 4], CurveStyle, ArrowStyle),
    // The color is the background color
    Rect(Rect, Rounding, Color32),
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
pub fn bezier_quadratic_to_cubic(q0: Pos2, q1: Pos2, q2: Pos2) -> [Pos2; 4] {
    // Taken from
    // https://stackoverflow.com/questions/3162645/convert-a-quadratic-bezier-to-a-cubic-one
    let c0 = q0;
    let c1 = q0 + (2f32 / 3f32) * (q1 - q0);
    let c2 = q2 + (2f32 / 3f32) * (q1 - q2);
    let c3 = q2;
    [c0, c1, c2, c3]
}

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

// Given a quadratic bezier and rectangle at the start and end, create an
// appropriate cubic that is the same at the bezier, but not cropping on the
// edge rects. It handle the src == dst by making a loop instead of a linear
// cubic.
pub fn prepare_edge(
    src: Pos2,
    src_rect: Rect,
    control: Pos2,
    dst: Pos2,
    dst_rect: Rect,
) -> [Pos2; 4] {
    let [q0, q1, q2, q3] = bezier_quadratic_to_cubic(src, control, dst);
    let (q1, q2) = if q1.distance_sq(q2) <= 1f32 && q1.distance_sq(q0) >= 1f32 {
        let v = 0.5f32 * (control - src).rot90();
        (q1 + v, q2 - v)
    } else {
        (q1, q2)
    };
    let q0 = ray_rect(q1 - q0, src_rect);
    let q3 = ray_rect(q2 - q3, dst_rect);
    [q0, q1, q2, q3]
}
