#![feature(test)]
extern crate test;

pub mod rf;
pub mod hooks;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
