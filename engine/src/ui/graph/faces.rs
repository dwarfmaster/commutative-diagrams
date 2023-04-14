use super::graph::UiGraph;
use crate::graph::GraphId;
use egui::{Align2, FontId, Painter, Pos2, Rect, Ui, Vec2};

pub fn faces_in_rect<G: UiGraph>(
    ui: &mut Ui,
    mut painter: Painter,
    mut rect: Rect,
    gr: &mut G,
    ppos: Option<Pos2>,
    closest: &mut GraphId,
    dist: &mut f32,
) {
    let width = rect.width();
    let fg_stroke = ui.visuals().noninteractive().fg_stroke;
    let bg_stroke = ui.visuals().noninteractive().bg_stroke;
    let font = FontId::monospace(14.0);
    let font_content = FontId::monospace(12.0);

    // Draw title
    let title = painter.text(
        rect.center_top(),
        Align2::CENTER_TOP,
        "Faces",
        font.clone(),
        fg_stroke.color,
    );
    painter.line_segment(
        [
            Pos2::new(rect.left(), title.bottom() + 5.0),
            Pos2::new(rect.right(), title.bottom() + 5.0),
        ],
        bg_stroke,
    );

    // Draw faces
    rect.set_top(title.bottom() + 10.0);
    painter.set_clip_rect(rect);
    // Position to draw the next face
    let mut face_pos = rect.center_top();
    gr.faces(ui.style(), |id, content, folded, style| {
        let name_galley =
            painter.layout_no_wrap(content.name.to_string(), font.clone(), style.text);
        let content_galley = if folded {
            None
        } else {
            Some(painter.layout(
                content.content.to_string(),
                font_content.clone(),
                style.text,
                rect.width() - 4.0,
            ))
        };

        // Compute size
        let face_height = 4.0
            + name_galley.rect.height()
            + content_galley
                .as_ref()
                .map(|g| g.rect.height() + 4.0 + style.border.width)
                .unwrap_or(0.0);
        let mut face_rect = Rect::from_center_size(face_pos, Vec2::new(width, face_height));
        face_rect.set_center(face_rect.center() + (face_rect.height() / 2.0) * Vec2::Y);
        painter.rect(face_rect, 5.0, style.fill, style.border);

        // Draw name
        let name_pos = face_pos - (name_galley.rect.width() / 2.0) * Vec2::X + 2.0 * Vec2::Y;
        let name_rect = name_galley.rect;
        painter.galley(name_pos, name_galley);

        // Draw content
        if let Some(content_galley) = content_galley {
            painter.line_segment(
                [
                    Pos2::new(rect.left() + 5.0, face_pos.y + name_rect.height() + 2.0),
                    Pos2::new(rect.right() - 5.0, face_pos.y + name_rect.height() + 2.0),
                ],
                style.sep,
            );
            let content_pos = Pos2::new(
                face_pos.x - content_galley.rect.width() / 2.0,
                face_pos.y + name_rect.height() + style.border.width + 4.0,
            );
            painter.galley(content_pos, content_galley);
        }

        if let Some(ppos) = ppos {
            if face_rect.contains(ppos) {
                *dist = 0.0;
                *closest = id;
            }
        }
        face_pos += (face_rect.height() + 5.0) * Vec2::Y;
    });
}
