#![feature(concat_idents)]
#![feature(try_trait)]
#![feature(proc_macro_hygiene)]
#![feature(associated_type_bounds)]

extern crate im_rc;
extern crate nom;
extern crate ena;
extern crate phf;

mod format;
mod syntax;
mod validation;
