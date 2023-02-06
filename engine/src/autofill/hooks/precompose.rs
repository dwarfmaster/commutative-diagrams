use crate::data::{ActualEquality, Context, Equality, Morphism, ActualMorphism};
use crate::normalize;
use crate::dsl::{eq,mph};

pub fn hook(mph: Morphism, ctx: &mut Context, eq: Equality, opts: &mut Vec<Equality>) {
    if mph.dst(ctx) == eq.src(ctx) {
        let (_, eql) = normalize::morphism(ctx, mph!(ctx, mph >> (eq.left(ctx))));
        let (_, eqr) = normalize::morphism(ctx, mph!(ctx, mph >> (eq.right(ctx))));
        let eq = ctx.mk(ActualEquality::LAp(mph.clone(), eq));
        let eq = eq!(ctx, (~eql) . (eq . eqr));
        assert!(eq.check(ctx), "Invalid equality produced");
        opts.push(eq)
    }
}
