mod hook;
pub mod proxy;
pub mod reference;
pub mod value;
pub mod var;

pub use proxy::RefProxy;
pub use reference::Ref;
pub use value::Value;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rf() {
        let mut i = Value::new(0);
        let mut ri = i.take_ref();
        *i += 1;
        *ri += 1;
        let mut ri2 = ri.duplicate();
        *ri2 += 1;
        *ri += 1;
        println!("DATA: {:?}", *ri);
    }

    #[test]
    fn parenting() {
        fn change_c(mut z: Value<Z>) {
            z.has_moved();
            *z.a.c += 15;
        }
        #[derive(Debug)]
        struct A {
            pub b: Value<i32>,
            pub c: Value<i32>,
        }
        #[derive(Debug)]
        struct Z {
            pub a: Value<A>,
        }
        {
            let (mut ra, mut rb, mut rc) = {
                let mut z = Value::new(Z {
                    a: Value::new(A {
                        b: Value::new(0),
                        c: Value::new(0),
                    }),
                });

                let child = &z.a.b as *const _;
                z.a.adopt(child);
                let child = &z.a.c as *const _;
                z.a.adopt(child);
                z.adopt(&z.a);

                let rz = z.take_ref();
                let ra = z.a.take_ref();
                let rb = z.a.b.take_ref();
                let rc = z.a.c.take_ref();
                let rc_ = z.a.c.take_ref();

                rz.dbg();
                ra.dbg();
                rb.dbg();
                rc.dbg();

                *z.a.c += 1;

                change_c(z);

                (ra, rb, rc)
            };

            *rb += 1;
            *ra.b += 3;

            ra.dbg();
            rb.dbg();
            rc.dbg();
        }
    }
}

mod benches {
    use super::*;
    use std::borrow::BorrowMut;
    use std::cell::RefCell;
    use std::mem::replace;
    use std::rc::Rc;
    use test::Bencher;

    #[derive(Debug)]
    struct A {
        a: Value<(Value<String>, Value<i32>)>,
        b: Value<f32>,
    }

    #[derive(Debug)]
    struct A_ {
        a: Rc<RefCell<(Rc<RefCell<String>>, Rc<RefCell<i32>>)>>,
        b: Rc<RefCell<f32>>,
    }

    fn a_value() -> Value<A> {
        let mut a = Value::new(A {
            a: Value::new((Value::new("yooo".into()), Value::new(9))),
            b: Value::new(0.9),
        });
        let child = &a.a as *const _;
        a.adopt(child);
        let child = &a.b as *const _;
        a.adopt(child);
        let child = &a.a.0 as *const _;
        a.a.adopt(child);
        let child = &a.a.1 as *const _;
        a.a.adopt(child);
        a
    }

    fn a_rc() -> Rc<RefCell<A_>> {
        Rc::new(RefCell::new(A_ {
            a: Rc::new(RefCell::new((
                Rc::new(RefCell::new("yooo".into())),
                Rc::new(RefCell::new(9)),
            ))),
            b: Rc::new(RefCell::new(0.9)),
        }))
    }

    #[bench]
    fn rp_create(b: &mut Bencher) {
        struct A {
            a: Value<(Value<String>, Value<i32>)>,
            b: Value<f32>,
        }
        b.iter(|| {
            let a = a_value();
        })
    }

    #[bench]
    fn rp_change_deep(b: &mut Bencher) {
        let mut a = a_value();

        b.iter(|| {
            *a.a.1 += 1;
        })
    }
    #[bench]
    fn rp_take_ref_deep(b: &mut Bencher) {
        let mut a = a_value();

        b.iter(|| {
            let deep = a.a.0.take_ref();
        })
    }
    #[bench]
    fn rp_take_ref_shallow(b: &mut Bencher) {
        let mut a = a_value();
        let mut a = a.take_ref();

        b.iter(|| {
            let shallow = a.a.0.take_ref();
        })
    }
    #[bench]
    fn rp_pass_fn(b: &mut Bencher) {
        let mut a = a_value();

        b.iter(|| {
            fn handle(mut x: Ref<A>) {
                x.a.0.push('o');
            }
            handle(a.take_ref());
        })
    }

    #[bench]
    fn rc_create(b: &mut Bencher) {
        struct A {
            a: Value<(Value<String>, Value<i32>)>,
            b: Value<f32>,
        }
        b.iter(|| {
            let a = a_rc();
        })
    }

    #[bench]
    fn rc_change_deep(b: &mut Bencher) {
        let a = a_rc();

        b.iter(|| {
            *(*(*(*a).borrow_mut().a).borrow_mut().1).borrow_mut() += 1;
        })
    }
    #[bench]
    fn rc_take_ref_deep(b: &mut Bencher) {
        let a = a_rc();

        b.iter(|| {
            let deep = a.borrow().a.borrow().1.clone();
        })
    }
    #[bench]
    fn rc_take_ref_shallow(b: &mut Bencher) {
        let a = a_rc();

        b.iter(|| {
            let shallow = a.borrow().b.clone();
        })
    }
    #[bench]
    fn rc_pass_fn(b: &mut Bencher) {
        let a = a_rc();

        b.iter(|| {
            fn handle(mut x: Rc<RefCell<A_>>) {
                (*(*(*x).borrow_mut().a).borrow_mut().0)
                    .borrow_mut()
                    .push('o');
            }
            handle(a.clone());
        })
    }
}
