use crate::data::{ActualEquality, Context, Equality, Morphism};

pub fn hook(mph: Morphism, ctx: &mut Context, eq: Equality, opts: &mut Vec<Equality>) {
    if mph.dst(ctx) == eq.src(ctx) {
        let eq = ctx.mk(ActualEquality::LAp(mph.clone(), eq));
        opts.push(eq)
    }
}
