use crate::data::{ActualCategory, ActualEquality, ActualFunctor, ActualMorphism, ActualObject};
use crate::data::{Context, ProofObject};
use pretty::RcDoc;
use std::iter::IntoIterator;

fn paren(d: RcDoc<()>) -> RcDoc<()> {
    RcDoc::text("(").append(d).append(RcDoc::text(")"))
}
fn sub(d: RcDoc<()>) -> RcDoc<()> {
    RcDoc::line().append(paren(d)).nest(2).group()
}
fn app<'a, I>(f: RcDoc<'a, ()>, args: I) -> RcDoc<'a, ()>
where
    I: IntoIterator<Item = RcDoc<'a, ()>>,
{
    let args = args.into_iter().map(|d| {
        RcDoc::line()
            .append(d)
            .append(RcDoc::text(","))
            .nest(2)
            .group()
    });
    f.append(RcDoc::text("("))
        .append(RcDoc::concat(args))
        .append(RcDoc::line())
        .append(RcDoc::text(")"))
}
fn render(d: RcDoc<()>, width: usize) -> String {
    let mut buf = Vec::new();
    d.render(width, &mut buf).unwrap();
    String::from_utf8(buf).unwrap()
}

impl ProofObject {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ProofObject::*;
        match *self {
            Term(t) => match ctx.term(t) {
                Some(str) => RcDoc::text(str),
                None => RcDoc::text(format!(":{}", t)),
            },
            Existential(e) => RcDoc::text(format!("?{}", e)),
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}

impl ActualCategory {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ActualCategory::*;
        match self {
            Atomic(pobj) => pobj.pobj.to_pretty(ctx),
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}

impl ActualFunctor {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ActualFunctor::*;
        match self {
            Atomic(pobj) => pobj.pobj.to_pretty(ctx),
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}

impl ActualObject {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ActualObject::*;
        match self {
            Atomic(pobj) => pobj.pobj.to_pretty(ctx),
            Funct(f, o) => {
                let f = f.to_pretty(ctx);
                let o = o.to_pretty(ctx);
                f.append(sub(o))
            }
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}

impl ActualMorphism {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ActualMorphism::*;
        match self {
            Atomic(pobj) => pobj.pobj.to_pretty(ctx),
            Identity(o) => RcDoc::text("id_").append(paren(o.to_pretty(ctx))),
            Comp(m1, m2) => {
                let m1 = m1.to_pretty(ctx);
                let m2 = m2.to_pretty(ctx);
                paren(m2)
                    .group()
                    .append(RcDoc::line().append(RcDoc::text("o")).nest(2).group())
                    .append(sub(m1))
            }
            Funct(f, m) => {
                let f = f.to_pretty(ctx);
                let m = m.to_pretty(ctx);
                f.append(sub(m))
            }
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}

impl ActualEquality {
    pub fn to_pretty(&self, ctx: &mut Context) -> RcDoc<()> {
        use ActualEquality::*;
        match self {
            Atomic(pobj) => pobj.pobj.to_pretty(ctx),
            Refl(m) => RcDoc::text("1_").append(paren(m.to_pretty(ctx))),
            Concat(p1, p2) => {
                let p1 = p1.to_pretty(ctx);
                let p2 = p2.to_pretty(ctx);
                paren(p1)
                    .group()
                    .append(RcDoc::line().append(RcDoc::text("<>")).nest(2).group())
                    .append(sub(p2))
            }
            Inv(p) => paren(p.to_pretty(ctx)).append(RcDoc::text("^-1")),
            Compose(p1, p2) => {
                let p1 = p1.to_pretty(ctx);
                let p2 = p2.to_pretty(ctx);
                paren(p1)
                    .group()
                    .append(RcDoc::line().append(RcDoc::text("o")).nest(2).group())
                    .append(sub(p2))
            }
            Assoc(m1, m2, m3) => {
                let m1 = m1.to_pretty(ctx);
                let m2 = m2.to_pretty(ctx);
                let m3 = m3.to_pretty(ctx);
                app(RcDoc::text("assoc"), vec![m1, m2, m3].into_iter())
            }
            LeftId(m) => app(RcDoc::text("lid"), std::iter::once(m.to_pretty(ctx))),
            RightId(m) => app(RcDoc::text("rid"), std::iter::once(m.to_pretty(ctx))),
            RAp(p, m) => {
                let p = p.to_pretty(ctx);
                let m = m.to_pretty(ctx);
                paren(p)
                    .group()
                    .append(RcDoc::line().append(RcDoc::text("o")).nest(2).group())
                    .append(sub(m))
            }
            LAp(m, p) => {
                let m = m.to_pretty(ctx);
                let p = p.to_pretty(ctx);
                paren(m)
                    .group()
                    .append(RcDoc::line().append(RcDoc::text("o")).nest(2).group())
                    .append(sub(p))
            }
            FunctId(f, o) => {
                let f = f.to_pretty(ctx);
                let o = o.to_pretty(ctx);
                app(RcDoc::text("fid"), vec![f, o].into_iter())
            }
            FunctComp(f, m1, m2) => {
                let f = f.to_pretty(ctx);
                let m1 = m1.to_pretty(ctx);
                let m2 = m2.to_pretty(ctx);
                app(RcDoc::text("fcomp"), vec![f, m1, m2].into_iter())
            }
            FunctCtx(f, p) => {
                let f = f.to_pretty(ctx);
                let p = p.to_pretty(ctx);
                f.append(sub(p))
            }
        }
    }

    pub fn render(&self, ctx: &mut Context, width: usize) -> String {
        render(self.to_pretty(ctx), width)
    }
}
