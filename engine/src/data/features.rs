use itertools::Itertools;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tag {
    // Types
    Category,
    Object,
    Morphism,
    Functor,
    Equality,
    // Objects
    AppliedFunctObj,
    // Morphisms
    Identity,
    ComposeMph,
    AppliedFunctMph,
    // Equality
    Reflexivity,
    Concat,
    InverseEq,
    ComposeEq,
    Associativity,
    LeftUnitality,
    RightUnitality,
    LeftApplication,
    RightApplication,
    FunctIdentity,
    FunctComposition,
    AppliedFunctEq,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Feature {
    // Types
    Category,
    Object {
        cat: u64,
    },
    Morphism {
        cat: u64,
        src: u64,
        dst: u64,
    },
    Functor {
        scat: u64,
        dcat: u64,
    },
    Equality {
        cat: u64,
        src: u64,
        dst: u64,
        left: u64,
        right: u64,
    },
    // Objects
    AppliedFunctObj {
        scat: u64,
        dcat: u64,
        funct: u64,
        obj: u64,
    },
    // Morphisms
    Identity {
        cat: u64,
        obj: u64,
    },
    ComposeMph {
        cat: u64,
        src: u64,
        mid: u64,
        dst: u64,
        m1: u64,
        m2: u64,
    },
    AppliedFunctMph {
        scat: u64,
        dcat: u64,
        funct: u64,
        src: u64,
        dst: u64,
        mph: u64,
    },
    // Equality
    Reflexivity {
        cat: u64,
        src: u64,
        dst: u64,
        mph: u64,
    },
    Concat {
        cat: u64,
        src: u64,
        dst: u64,
        left: u64,
        mid: u64,
        right: u64,
        eq1: u64,
        eq2: u64,
    },
    InverseEq {
        cat: u64,
        src: u64,
        dst: u64,
        left: u64,
        right: u64,
        eq: u64,
    },
    ComposeEq {
        cat: u64,
        src: u64,
        mid: u64,
        dst: u64,
        left1: u64,
        right1: u64,
        eq1: u64,
        left2: u64,
        right2: u64,
        eq2: u64,
    },
    Associativity {
        cat: u64,
        src: u64,
        mid1: u64,
        mid2: u64,
        dst: u64,
        m1: u64,
        m2: u64,
        m3: u64,
    },
    LeftUnitality {
        cat: u64,
        src: u64,
        dst: u64,
        mph: u64,
    },
    RightUnitality {
        cat: u64,
        src: u64,
        dst: u64,
        mph: u64,
    },
    LeftApplication {
        cat: u64,
        src: u64,
        mid: u64,
        dst: u64,
        mph: u64,
        left: u64,
        right: u64,
        eq: u64,
    },
    RightApplication {
        cat: u64,
        src: u64,
        mid: u64,
        dst: u64,
        left: u64,
        right: u64,
        eq: u64,
        mph: u64,
    },
    FunctIdentity {
        scat: u64,
        dcat: u64,
        funct: u64,
        obj: u64,
    },
    FunctComposition {
        scat: u64,
        dcat: u64,
        funct: u64,
        src: u64,
        mid: u64,
        dst: u64,
        m1: u64,
        m2: u64,
    },
    AppliedFunctEq {
        scat: u64,
        dcat: u64,
        funct: u64,
        src: u64,
        dst: u64,
        left: u64,
        right: u64,
        eq: u64,
    },
}

pub struct FeatureIterator<'a> {
    feat: &'a Feature,
    pos: usize,
}

impl<'a> Iterator for FeatureIterator<'a> {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        use Feature::*;
        let r = match (self.feat, self.pos) {
            (Category, _) => None,
            (Object { cat }, 0) => Some(cat),
            (Object { .. }, _) => None,
            (Morphism { cat, .. }, 0) => Some(cat),
            (Morphism { src, .. }, 1) => Some(src),
            (Morphism { dst, .. }, 2) => Some(dst),
            (Morphism { .. }, _) => None,
            (Functor { scat, .. }, 0) => Some(scat),
            (Functor { dcat, .. }, 1) => Some(dcat),
            (Functor { .. }, _) => None,
            (Equality { cat, .. }, 0) => Some(cat),
            (Equality { src, .. }, 1) => Some(src),
            (Equality { dst, .. }, 2) => Some(dst),
            (Equality { left, .. }, 3) => Some(left),
            (Equality { right, .. }, 4) => Some(right),
            (Equality { .. }, _) => None,
            (AppliedFunctObj { scat, .. }, 0) => Some(scat),
            (AppliedFunctObj { dcat, .. }, 1) => Some(dcat),
            (AppliedFunctObj { funct, .. }, 2) => Some(funct),
            (AppliedFunctObj { obj, .. }, 3) => Some(obj),
            (AppliedFunctObj { .. }, _) => None,
            (Identity { cat, .. }, 0) => Some(cat),
            (Identity { obj, .. }, 1) => Some(obj),
            (Identity { .. }, _) => None,
            (ComposeMph { cat, .. }, 0) => Some(cat),
            (ComposeMph { src, .. }, 1) => Some(src),
            (ComposeMph { mid, .. }, 2) => Some(mid),
            (ComposeMph { dst, .. }, 3) => Some(dst),
            (ComposeMph { m1, .. }, 4) => Some(m1),
            (ComposeMph { m2, .. }, 5) => Some(m2),
            (ComposeMph { .. }, _) => None,
            (AppliedFunctMph { scat, .. }, 0) => Some(scat),
            (AppliedFunctMph { dcat, .. }, 1) => Some(dcat),
            (AppliedFunctMph { funct, .. }, 2) => Some(funct),
            (AppliedFunctMph { src, .. }, 3) => Some(src),
            (AppliedFunctMph { dst, .. }, 4) => Some(dst),
            (AppliedFunctMph { mph, .. }, 5) => Some(mph),
            (AppliedFunctMph { .. }, _) => None,
            (Reflexivity { cat, .. }, 0) => Some(cat),
            (Reflexivity { src, .. }, 1) => Some(src),
            (Reflexivity { dst, .. }, 2) => Some(dst),
            (Reflexivity { mph, .. }, 3) => Some(mph),
            (Reflexivity { .. }, _) => None,
            (Concat { cat, .. }, 0) => Some(cat),
            (Concat { src, .. }, 1) => Some(src),
            (Concat { dst, .. }, 2) => Some(dst),
            (Concat { left, .. }, 3) => Some(left),
            (Concat { mid, .. }, 4) => Some(mid),
            (Concat { right, .. }, 5) => Some(right),
            (Concat { eq1, .. }, 6) => Some(eq1),
            (Concat { eq2, .. }, 7) => Some(eq2),
            (Concat { .. }, _) => None,
            (InverseEq { cat, .. }, 0) => Some(cat),
            (InverseEq { src, .. }, 1) => Some(src),
            (InverseEq { dst, .. }, 2) => Some(dst),
            (InverseEq { left, .. }, 3) => Some(left),
            (InverseEq { right, .. }, 4) => Some(right),
            (InverseEq { eq, .. }, 5) => Some(eq),
            (InverseEq { .. }, _) => None,
            (ComposeEq { cat, .. }, 0) => Some(cat),
            (ComposeEq { src, .. }, 1) => Some(src),
            (ComposeEq { mid, .. }, 2) => Some(mid),
            (ComposeEq { dst, .. }, 3) => Some(dst),
            (ComposeEq { left1, .. }, 4) => Some(left1),
            (ComposeEq { right1, .. }, 5) => Some(right1),
            (ComposeEq { eq1, .. }, 6) => Some(eq1),
            (ComposeEq { left2, .. }, 7) => Some(left2),
            (ComposeEq { right2, .. }, 8) => Some(right2),
            (ComposeEq { eq2, .. }, 9) => Some(eq2),
            (ComposeEq { .. }, _) => None,
            (Associativity { cat, .. }, 0) => Some(cat),
            (Associativity { src, .. }, 1) => Some(src),
            (Associativity { mid1, .. }, 2) => Some(mid1),
            (Associativity { mid2, .. }, 3) => Some(mid2),
            (Associativity { dst, .. }, 4) => Some(dst),
            (Associativity { m1, .. }, 5) => Some(m1),
            (Associativity { m2, .. }, 6) => Some(m2),
            (Associativity { m3, .. }, 7) => Some(m3),
            (Associativity { .. }, _) => None,
            (LeftUnitality { cat, .. }, 0) => Some(cat),
            (LeftUnitality { src, .. }, 1) => Some(src),
            (LeftUnitality { dst, .. }, 2) => Some(dst),
            (LeftUnitality { mph, .. }, 3) => Some(mph),
            (LeftUnitality { .. }, _) => None,
            (RightUnitality { cat, .. }, 0) => Some(cat),
            (RightUnitality { src, .. }, 1) => Some(src),
            (RightUnitality { dst, .. }, 2) => Some(dst),
            (RightUnitality { mph, .. }, 3) => Some(mph),
            (RightUnitality { .. }, _) => None,
            (LeftApplication { cat, .. }, 0) => Some(cat),
            (LeftApplication { src, .. }, 1) => Some(src),
            (LeftApplication { mid, .. }, 2) => Some(mid),
            (LeftApplication { dst, .. }, 3) => Some(dst),
            (LeftApplication { mph, .. }, 4) => Some(mph),
            (LeftApplication { left, .. }, 5) => Some(left),
            (LeftApplication { right, .. }, 6) => Some(right),
            (LeftApplication { eq, .. }, 7) => Some(eq),
            (LeftApplication { .. }, _) => None,
            (RightApplication { cat, .. }, 0) => Some(cat),
            (RightApplication { src, .. }, 1) => Some(src),
            (RightApplication { mid, .. }, 2) => Some(mid),
            (RightApplication { dst, .. }, 3) => Some(dst),
            (RightApplication { left, .. }, 4) => Some(left),
            (RightApplication { right, .. }, 5) => Some(right),
            (RightApplication { eq, .. }, 6) => Some(eq),
            (RightApplication { mph, .. }, 7) => Some(mph),
            (RightApplication { .. }, _) => None,
            (FunctIdentity { scat, .. }, 0) => Some(scat),
            (FunctIdentity { dcat, .. }, 1) => Some(dcat),
            (FunctIdentity { funct, .. }, 2) => Some(funct),
            (FunctIdentity { obj, .. }, 3) => Some(obj),
            (FunctIdentity { .. }, _) => None,
            (FunctComposition { scat, .. }, 0) => Some(scat),
            (FunctComposition { dcat, .. }, 1) => Some(dcat),
            (FunctComposition { funct, .. }, 2) => Some(funct),
            (FunctComposition { src, .. }, 3) => Some(src),
            (FunctComposition { mid, .. }, 4) => Some(mid),
            (FunctComposition { dst, .. }, 5) => Some(dst),
            (FunctComposition { m1, .. }, 6) => Some(m1),
            (FunctComposition { m2, .. }, 7) => Some(m2),
            (FunctComposition { .. }, _) => None,
            (AppliedFunctEq { scat, .. }, 0) => Some(scat),
            (AppliedFunctEq { dcat, .. }, 1) => Some(dcat),
            (AppliedFunctEq { funct, .. }, 2) => Some(funct),
            (AppliedFunctEq { src, .. }, 3) => Some(src),
            (AppliedFunctEq { dst, .. }, 4) => Some(dst),
            (AppliedFunctEq { left, .. }, 5) => Some(left),
            (AppliedFunctEq { right, .. }, 6) => Some(right),
            (AppliedFunctEq { eq, .. }, 7) => Some(eq),
            (AppliedFunctEq { .. }, _) => None,
        };
        self.pos += 1;
        r.copied()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        use Feature::*;
        match self.feat {
            Category => (0, Some(0)),
            Object { .. } => (1, Some(1)),
            Morphism { .. } => (3, Some(3)),
            Functor { .. } => (2, Some(2)),
            Equality { .. } => (5, Some(5)),
            AppliedFunctObj { .. } => (4, Some(4)),
            Identity { .. } => (2, Some(2)),
            ComposeMph { .. } => (6, Some(6)),
            AppliedFunctMph { .. } => (6, Some(6)),
            Reflexivity { .. } => (4, Some(4)),
            Concat { .. } => (8, Some(8)),
            InverseEq { .. } => (6, Some(6)),
            ComposeEq { .. } => (10, Some(10)),
            Associativity { .. } => (8, Some(8)),
            LeftUnitality { .. } => (4, Some(4)),
            RightUnitality { .. } => (4, Some(4)),
            LeftApplication { .. } => (8, Some(8)),
            RightApplication { .. } => (8, Some(8)),
            FunctIdentity { .. } => (4, Some(4)),
            FunctComposition { .. } => (8, Some(8)),
            AppliedFunctEq { .. } => (8, Some(8)),
        }
    }
}
impl<'a> ExactSizeIterator for FeatureIterator<'a> {}

impl Feature {
    pub fn tag(&self) -> Tag {
        use Feature::*;
        match self {
            Category => Tag::Category,
            Object { .. } => Tag::Object,
            Morphism { .. } => Tag::Morphism,
            Functor { .. } => Tag::Functor,
            Equality { .. } => Tag::Equality,
            AppliedFunctObj { .. } => Tag::AppliedFunctObj,
            Identity { .. } => Tag::Identity,
            ComposeMph { .. } => Tag::ComposeMph,
            AppliedFunctMph { .. } => Tag::AppliedFunctMph,
            Reflexivity { .. } => Tag::Reflexivity,
            Concat { .. } => Tag::Concat,
            InverseEq { .. } => Tag::InverseEq,
            ComposeEq { .. } => Tag::ComposeEq,
            Associativity { .. } => Tag::Associativity,
            LeftUnitality { .. } => Tag::LeftUnitality,
            RightUnitality { .. } => Tag::RightUnitality,
            LeftApplication { .. } => Tag::LeftApplication,
            RightApplication { .. } => Tag::RightApplication,
            FunctIdentity { .. } => Tag::FunctIdentity,
            FunctComposition { .. } => Tag::FunctComposition,
            AppliedFunctEq { .. } => Tag::AppliedFunctEq,
        }
    }

    pub fn iter<'a>(&'a self) -> FeatureIterator<'a> {
        FeatureIterator { feat: self, pos: 0 }
    }

    pub fn from_iter<I>(tag: Tag, mut iter: I) -> Option<Self>
    where
        I: Iterator<Item = u64>,
    {
        use Feature::*;
        match tag {
            Tag::Category => {
                if iter.next().is_some() {
                    None
                } else {
                    Some(Category)
                }
            }
            Tag::Object => {
                let cat = iter.exactly_one().ok()?;
                Some(Object { cat })
            }
            Tag::Morphism => {
                let (cat, src, dst) = iter.collect_tuple()?;
                Some(Morphism { cat, src, dst })
            }
            Tag::Functor => {
                let (scat, dcat) = iter.collect_tuple()?;
                Some(Functor { scat, dcat })
            }
            Tag::Equality => {
                let (cat, src, dst, left, right) = iter.collect_tuple()?;
                Some(Equality {
                    cat,
                    src,
                    dst,
                    left,
                    right,
                })
            }
            Tag::AppliedFunctObj => {
                let (scat, dcat, funct, obj) = iter.collect_tuple()?;
                Some(AppliedFunctObj {
                    scat,
                    dcat,
                    funct,
                    obj,
                })
            }
            Tag::Identity => {
                let (cat, obj) = iter.collect_tuple()?;
                Some(Identity { cat, obj })
            }
            Tag::ComposeMph => {
                let (cat, src, mid, dst, m1, m2) = iter.collect_tuple()?;
                Some(ComposeMph {
                    cat,
                    src,
                    mid,
                    dst,
                    m1,
                    m2,
                })
            }
            Tag::AppliedFunctMph => {
                let (scat, dcat, funct, src, dst, mph) = iter.collect_tuple()?;
                Some(AppliedFunctMph {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst,
                    mph,
                })
            }
            Tag::Reflexivity => {
                let (cat, src, dst, mph) = iter.collect_tuple()?;
                Some(Reflexivity { cat, src, dst, mph })
            }
            Tag::Concat => {
                let (cat, src, dst, left, mid, right, eq1, eq2) = iter.collect_tuple()?;
                Some(Concat {
                    cat,
                    src,
                    dst,
                    left,
                    mid,
                    right,
                    eq1,
                    eq2,
                })
            }
            Tag::InverseEq => {
                let (cat, src, dst, left, right, eq) = iter.collect_tuple()?;
                Some(InverseEq {
                    cat,
                    src,
                    dst,
                    left,
                    right,
                    eq,
                })
            }
            Tag::ComposeEq => {
                let (cat, src, mid, dst, left1, right1, eq1, left2, right2, eq2) =
                    iter.collect_tuple()?;
                Some(ComposeEq {
                    cat,
                    src,
                    mid,
                    dst,
                    left1,
                    right1,
                    eq1,
                    left2,
                    right2,
                    eq2,
                })
            }
            Tag::Associativity => {
                let (cat, src, mid1, mid2, dst, m1, m2, m3) = iter.collect_tuple()?;
                Some(Associativity {
                    cat,
                    src,
                    mid1,
                    mid2,
                    dst,
                    m1,
                    m2,
                    m3,
                })
            }
            Tag::LeftUnitality => {
                let (cat, src, dst, mph) = iter.collect_tuple()?;
                Some(LeftUnitality { cat, src, dst, mph })
            }
            Tag::RightUnitality => {
                let (cat, src, dst, mph) = iter.collect_tuple()?;
                Some(RightUnitality { cat, src, dst, mph })
            }
            Tag::LeftApplication => {
                let (cat, src, mid, dst, mph, left, right, eq) = iter.collect_tuple()?;
                Some(LeftApplication {
                    cat,
                    src,
                    mid,
                    dst,
                    mph,
                    left,
                    right,
                    eq,
                })
            }
            Tag::RightApplication => {
                let (cat, src, mid, dst, left, right, eq, mph) = iter.collect_tuple()?;
                Some(RightApplication {
                    cat,
                    src,
                    mid,
                    dst,
                    left,
                    right,
                    eq,
                    mph,
                })
            }
            Tag::FunctIdentity => {
                let (scat, dcat, funct, obj) = iter.collect_tuple()?;
                Some(FunctIdentity {
                    scat,
                    dcat,
                    funct,
                    obj,
                })
            }
            Tag::FunctComposition => {
                let (scat, dcat, funct, src, mid, dst, m1, m2) = iter.collect_tuple()?;
                Some(FunctComposition {
                    scat,
                    dcat,
                    funct,
                    src,
                    mid,
                    dst,
                    m1,
                    m2,
                })
            }
            Tag::AppliedFunctEq => {
                let (scat, dcat, funct, src, dst, left, right, eq) = iter.collect_tuple()?;
                Some(AppliedFunctEq {
                    scat,
                    dcat,
                    funct,
                    src,
                    dst,
                    left,
                    right,
                    eq,
                })
            }
        }
    }
}
