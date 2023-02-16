use crate::graph::Graph;
use bevy::ecs::system::Resource;
use egui::{Pos2, Vec2};
use lens_rs::*;

#[derive(Resource)]
pub struct GraphDisplay<
    NodeLabel,
    EdgeLabel,
    FaceLabel,
    PosGetter,
    NodeNameGetter,
    PathGetter,
    PathNameGetter,
    FaceNameGetter,
> {
    pub graph: Graph<NodeLabel, EdgeLabel, FaceLabel>,
    pub node_pos: PosGetter,
    pub node_name: NodeNameGetter,
    pub path_curve: PathGetter,
    pub path_name: PathNameGetter,
    pub face_name: FaceNameGetter,
    pub offset: Vec2,
    pub zoom: f32,
}

impl<NL, EL, FL, PG, NNG, PtG, PtNG, FcNG> GraphDisplay<NL, EL, FL, PG, NNG, PtG, PtNG, FcNG> {
    pub fn new(graph: Graph<NL, EL, FL>, np: PG, nn: NNG, ptc: PtG, ptn: PtNG, fcn: FcNG) -> Self
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
            face_name: fcn,
            offset: Vec2::ZERO,
            zoom: 1.0,
        }
    }
}
