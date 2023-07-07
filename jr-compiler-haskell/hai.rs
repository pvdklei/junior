use lib::*;

mod bmod {

use crate::*; type Int = i32; type Int64 = i64; type Bool = bool; type Float = f64; type Float32 = f32; type Uint = u32;

#[derive(Clone, Copy, Debug)]
pub struct Tuple(String, Int)

}


mod down {

use crate::*; type Int = i32; type Int64 = i64; type Bool = bool; type Float = f64; type Float32 = f32; type Uint = u32;

pub const hai: String = String::from("hai");
}


mod ppp {

use crate::*; type Int = i32; type Int64 = i64; type Bool = bool; type Float = f64; type Float32 = f32; type Uint = u32;

pub trait Trait {
fn function(&self) -> &Self;
}

#[derive(Clone, Copy, Debug)]
pub struct Name {
hai: String,
name: Int
}

pub fn Name(hai: String, name: Int) -> Name { Name { hai, name } }

let haiString = format!("hai {#:?}", name);
}

