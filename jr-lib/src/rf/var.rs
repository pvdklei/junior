use std::cell::RefCell;

use super::RefProxy;

pub struct Var<T> {
    proxy: *mut RefProxy,
    inner: VarInner<T>,
}

enum VarInner<T> {
    Value(T),
    Ref(usize),
}

impl<T> Var<T> {
    fn new(mut x: T) -> Self {
        Self {
            proxy: {
                let p = RefProxy::new_root(&mut x);
                unsafe {
                    p.as_mut().unwrap().increase_count();
                }
                p
            },
            inner: VarInner::Value(x),
        }
    }
}

#[derive(Clone, Copy)]
enum TestEnum {
    A(i32),
    C(i32),
}

mod benches {

    use test::{black_box, Bencher};

    use super::TestEnum;
    #[bench]
    fn a(b: &mut Bencher) {
        let mut a: i32 = black_box(0);
        b.iter(move || (0..N).fold(a, |a, e| a + e))
    }
    const N: i32 = 1000;

    #[bench]
    fn c(b: &mut Bencher) {
        let mut a = black_box(TestEnum::A(0));
        b.iter(move || {
            (0..N).fold(a, |a, e| match a {
                TestEnum::C(b) => TestEnum::C(1 + b),
                TestEnum::A(a) => TestEnum::A(a + e),
            })
        })
    }
}
