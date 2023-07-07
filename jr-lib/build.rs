/// Yoo
///
/// call(&T): S
/// return &T -> Rc<T>
/// return &mut T -> Rc<RefCell<T>>
use inwelling;
use std::collections as coll;
use syn::{token::printing, visit as svisit};

struct TypeFinder {
    counts: coll::HashMap<syn::Type, Counts>,
}

struct Counts {
    objects: usize,
    refs: usize,
    mutrefs: usize,
}

impl<'a> svisit::Visit<'a> for TypeFinder {
    fn visit_macro(&mut self, m: &'a syn::Macro) {
        svisit::visit_macro(self, m);
        if m.path.segments.first().unwrap().ident != "jr" {
            return;
        }

        match &m.path.segments.last().unwrap().ident {
            ref ptr if *ptr == "ptr" => {
                let tks = m.tokens.clone();
                let expr = syn::parse2::<syn::Expr>(tks);
            }
            _ => {}
        }
    }
}

fn main() {
    // inwelling::inwelling(inwelling::Opts {
    //     watch_manifest: true,
    //     watch_rs_files: true,
    //     dump_rs_paths: true,
    // })
    // .sections
    // .into_iter()
    // .for_each(|s| {
    //     s.rs_paths.unwrap().into_iter().for_each(|path| {
    //         let src = std::fs::read_to_string(path).unwrap();
    //         println!("cargo:warning=lenfile{}", src.len());
    //     })
    // });
}
