use crate::graph::GraphId;
use egui::{Pos2, Stroke, Style, Ui, Vec2};
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
    Curve([Pos2; 4], CurveStyle, ArrowStyle),
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum Modifier {
    None,
    Highlight,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Action {
    None,
    Hover(GraphId),
    Click(GraphId),
    DoubleClick(GraphId),
}

pub trait UiGraph {
    fn draw<'a, F>(&'a self, style: &Arc<Style>, f: F)
    where
        F: FnMut(Drawable<'a>, Stroke, Modifier, GraphId);
    fn zoom<'a>(&'a mut self) -> &'a mut f32;
    fn offset<'a>(&'a mut self) -> &'a mut Vec2;
    fn focused<'a>(&'a mut self) -> &'a mut Option<GraphId>;
    // Called at every frame
    fn action(&mut self, act: Action, ui: &mut Ui);
    // Setup right-click menu. Must returns false when the menu is closed
    fn context_menu(&mut self, on: GraphId, ui: &mut Ui) -> bool;
}
