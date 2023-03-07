use crate::data::{ActualEquality, Context, Equality, Morphism};
use crate::dsl::{eq, mph};
use crate::normalize;

pub fn hook(mph: Morphism, ctx: &mut Context, eq: Equality, opts: &mut Vec<Equality>) {
    if mph.src(ctx) == eq.dst(ctx) {
        let left = eq.left(ctx);
        let right = eq.right(ctx);
        let (_, eql) = normalize::morphism(ctx, mph!(ctx, left >> mph));
        let (_, eqr) = normalize::morphism(ctx, mph!(ctx, right >> mph));
        let eq = ctx.mk(ActualEquality::RAp(eq, mph.clone()));
        let eq = eq!(ctx, (~eql) . (eq . eqr));
        assert!(eq.check(ctx), "Invalid equality produced");
        opts.push(eq)
    }
}
