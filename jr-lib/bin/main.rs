use jr_lib::rf::*;

fn main() {
    #[derive(Debug)]
    struct A {
        pub b: Value<i32>,
        pub c: Value<i32>,
    }
    let mut a = Value::new(A {
        b: Value::new(0),
        c: Value::new(0),
    });
    a.adopt(&a.c);
    // a.adopt(&a.b);

    // let rb = a.b.take_ref();

    // println!("{:?}", *rb);
}
