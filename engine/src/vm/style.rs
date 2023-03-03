use crate::vm::vm::CodeStyle;
use crate::vm::VM;
use std::ops::Range;

impl VM {
    pub fn reset_style(&mut self) {
        assert!(self.code_style.len() >= 1);
        self.code_style[0].1 = CodeStyle::None;
        self.code_style.truncate(1);
    }

    pub fn style_range(&mut self, rng: Range<usize>, style: CodeStyle) {
        // Nothing to to if the range is empty
        if rng.end <= rng.start {
            return;
        }

        let start = self
            .code_style
            .binary_search_by_key(&rng.start, |(i, _)| *i);
        let end = self.code_style.binary_search_by_key(&rng.end, |(i, _)| *i);
        let start_id = start.clone().unwrap_or_else(|i| i);
        let end_id = end.clone().unwrap_or_else(|i| i) + if start.is_ok() { 0 } else { 1 };

        if let Err(id) = end {
            // Special case if we reach the end
            if rng.end != self.code.len() {
                let style = self.code_style[id - 1].1;
                self.code_style.insert(id, (rng.end, style));
            }
        }
        match start {
            Ok(id) => self.code_style[id].1 = style,
            Err(id) => self.code_style.insert(id, (rng.start, style)),
        }

        if end_id > start_id + 1 {
            self.code_style
                .splice(start_id + 1..end_id, std::iter::empty());
        }
    }
}
