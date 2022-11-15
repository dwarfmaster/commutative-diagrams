macro_rules! cat {
    ($ctx:expr, ?$e:expr) => {
        $ctx.mk(ActualCategory::Atomic(CategoryData {
            pobj: ProofObject::Existential($e),
        }))
    };
    ($ctx:expr, (?$e:expr)) => {
        cat!($ctx, ?$e)
    };
    ($ctx:expr, :$e:expr) => {
        {
            let name = format!("cat-{}", $e);
            let _ = $ctx.new_term($e, name.as_str());
            $ctx.mk(ActualCategory::Atomic(CategoryData {
                pobj: ProofObject::Term($e),
            }))
        }
    };
    ($ctx:expr, (:$e:expr)) => {
        cat!($ctx, :$e)
    };
    ($ctx:expr, $cat:expr) => {
        $cat.clone()
    };
}
pub(crate) use cat;

macro_rules! funct {
    ($ctx:expr, $e:tt : $s:tt => $d:tt) => {
        {
            let src = cat!($ctx,$s);
            let dst = cat!($ctx,$d);
            funct!($ctx, $e, src, dst)
        }
    };
    ($ctx:expr, ($e:tt : $s:tt => $d:tt)) => {
        funct!($ctx, $e : $s => $d)
    };
    ($ctx:expr, ?$e:expr, $src:expr, $dst:expr) => {
        $ctx.mk(ActualFunctor::Atomic(FunctorData {
            pobj: ProofObject::Existential($e),
            src: $src,
            dst: $dst,
        }))
    };
    ($ctx:expr, (?$e:expr), $src:expr, $dst:expr) => {
        funct!($ctx, ?$e, $src, $dst)
    };
    ($ctx:expr, :$e:expr, $src:expr, $dst:expr) => {
        {
            let name = format!("funct-{}", $e);
            let _ = $ctx.new_term($e, name.as_str());
            $ctx.mk(ActualFunctor::Atomic(FunctorData {
                pobj: ProofObject::Term($e),
                src: $src,
                dst: $dst,
            }))
        }
    };
    ($ctx:expr, (:$e:expr), $src:expr, $dst:expr) => {
        funct!($ctx, :$e, $src, $dst)
    };
    ($ctx:expr, $f:expr) => {
        $f.clone()
    };
}
pub(crate) use funct;

macro_rules! obj {
    ($ctx:expr, $e:tt in $cat:tt) => {
        {
            let cat = cat!($ctx,$cat);
            obj!($ctx, $e, cat)
        }
    };
    ($ctx:expr, ($e:tt in $cat:tt)) => {
        obj!($ctx, $e in $cat)
    };
    ($ctx:expr, $f:tt _0 $e:tt) => {
        {
            let fnt = funct!($ctx, $f);
            let obj = obj!($ctx,$e);
            $ctx.mk(ActualObject::Funct(fnt,obj))
        }
    };
    ($ctx:expr, ($f:tt _0 $e:tt)) => {
        obj!($ctx, $f _0 $e)
    };
    ($ctx:expr, ?$e:expr, $cat:expr) => {
        $ctx.mk(ActualObject::Atomic(ObjectData {
            pobj: ProofObject::Existential($e),
            category: $cat,
        }))
    };
    ($ctx:expr, (?$e:expr), $cat:expr) => {
        obj!($ctx, ?$e, $cat)
    };
    ($ctx:expr, :$e:expr, $cat:expr) => {
        {
            let name = format!("obj-{}", $e);
            let _ = $ctx.new_term($e, name.as_str());
            $ctx.mk(ActualObject::Atomic(ObjectData {
                pobj: ProofObject::Term($e),
                category: $cat,
            }))
        }
    };
    ($ctx:expr, (:$e:expr), $cat:expr) => {
        obj!($ctx, :$e, $cat)
    };
    ($ctx:expr, $obj:expr) => {
        $obj.clone()
    };
}
pub(crate) use obj;

macro_rules! mph {
    ($ctx:expr, $m:tt : $s:tt -> $d:tt) => {
        {
            let src = obj!($ctx,$s);
            let dst = obj!($ctx,$d);
            mph!($ctx,$m,src,dst)
        }
    };
    ($ctx:expr, ($m:tt : $s:tt -> $d:tt)) => {
        mph!($ctx, $m : $s -> $d)
    };
    ($ctx:expr, id $o:tt) => {
        {
            let obj = obj!($ctx, $o);
            $ctx.mk(ActualMorphism::Identity(obj))
        }
    };
    ($ctx:expr, (id $o:tt)) => {
        mph!($ctx, id $o)
    };
    ($ctx:expr, $m:tt >> $($ms:tt)>>+) => {
        {
            let mph1 = mph!($ctx,$m);
            let mph2 = mph!($ctx,$($ms)>>+);
            $ctx.mk(ActualMorphism::Comp(mph1,mph2))
        }
    };
    ($ctx:expr, ($m:tt >> $($ms:tt)>>+)) => {
        mph!($ctx, $m >> $($ms)>>+)
    };
    ($ctx:expr, $f:tt _1 $m:tt) => {
        {
            let fnt = funct!($ctx, $f);
            let mph = mph!($ctx,$m);
            $ctx.mk(ActualMorphism::Funct(fnt,mph))
        }
    };
    ($ctx:expr, ($f:tt _1 $m:tt)) => {
        mph!($ctx, $f _1 $m)
    };
    ($ctx:expr, ?$m:expr, $s:expr, $d:expr) => {
        {
            let cat = ($s).cat(&mut $ctx);
            $ctx.mk(ActualMorphism::Atomic(MorphismData {
                pobj: ProofObject::Existential($m),
                category: cat,
                src: $s,
                dst: $d,
            }))
        }
    };
    ($ctx:expr, (?$m:expr), $s:expr, $d:expr) => {
        mph!($ctx, ?$m, $s, $d)
    };
    ($ctx:expr, :$m:expr, $s:expr, $d:expr) => {
        {
            let name = format!("mph-{}", $m);
            let _ = $ctx.new_term($m, name.as_str());
            let cat = ($s).cat(&mut $ctx);
            $ctx.mk(ActualMorphism::Atomic(MorphismData {
                pobj: ProofObject::Term($m),
                category: cat,
                src: $s,
                dst: $d,
            }))
        }
    };
    ($ctx:expr, (:$m:expr), $s:expr, $d:expr) => {
        mph!($ctx, :$m, $s, $d)
    };
    ($ctx:expr, $mph:expr) => {
        $mph.clone()
    }
}
pub(crate) use mph;

#[cfg(test)]
mod test {
    use crate::data::{ActualCategory, ActualFunctor, ActualMorphism, ActualObject};
    use crate::data::{CategoryData, FunctorData, MorphismData, ObjectData};
    use crate::data::{Context, ProofObject};

    #[test]
    pub fn cat_macro() {
        let mut ctx = Context::new();
        let cat1 = cat!(ctx, :5);
        assert!(cat1.check(&mut ctx), "cat1 is invalid");
        assert_eq!(ctx.term(5), Some("cat-5"), "cat term has not been created");
        let cat2 = cat!(ctx, (?10));
        assert!(cat2.check(&mut ctx), "cat2 is invalid");
    }

    #[test]
    pub fn funct_macro() {
        let mut ctx = Context::new();
        let f = funct!(ctx, (?3) : (:4) => (:3));
        assert!(f.check(&mut ctx), "f is invalid");
        assert_eq!(ctx.term(3), Some("cat-3"), "cat term has not been created");
    }

    #[test]
    pub fn obj_macro() {
        let mut ctx = Context::new();
        let o = obj!(ctx, (:3) in (?2));
        assert!(o.check(&mut ctx), "o is invalid");
        let f = funct!(ctx, (?1) : (?2) => (:1));
        let o2 = obj!(ctx, ((?0) : (:1) => (:2)) _0 (f _0 o));
        assert!(o2.check(&mut ctx), "o2 is invalid");
    }

    #[test]
    pub fn mph_macro() {
        let mut ctx = Context::new();
        let cat1 = cat!(ctx, :0);
        let cat2 = cat!(ctx, :1);
        let f = funct!(ctx, (:2) : cat1 => cat2);
        let a = obj!(ctx, (:3) in cat1);
        let b = obj!(ctx, (:4) in cat1);
        let c = obj!(ctx, (:5) in cat2);
        let d = obj!(ctx, (:6) in cat2);
        let e = obj!(ctx, (:7) in cat2);
        let mab = mph!(ctx, (:8) : a -> b);
        let mcd = mph!(ctx, (:9) : c -> d);
        let mde = mph!(ctx, (:10) : d -> e);
        let mph = mph!(ctx, (f _1 mab) >> ((:11) : (f _0 b) -> c) >> mcd >> (id d) >> mde);
        assert!(mph.check(&mut ctx), "mph not valid");
    }
}
