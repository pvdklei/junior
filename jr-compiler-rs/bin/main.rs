//use std::ops::{Deref, Shr};

//struct JrFn<F, P, O>(F)
//where
//F: Fn(P) -> O;

//impl<F, P, O> Deref for JrFn<F, P, O> {
//type Target = F;
//fn deref(&self) -> &Self::Target {
//&self.0
//}
//}

//impl<T, F, P, O> Shr<JrFn<F, P, O>> for T
//where
//F: Fn(P) -> O,
//{
//type Output = O;
//fn shr(self, rhs: Self) -> Self::Output {}
//}

use jr_compiler_rs as jrc;

fn main() {
    // let tks = jrc::parse::lex_and_parse_with(include_str!("../jr.jr"), &jrc::parse::parse_file);
    // println!("{:#?}", tks);
}
