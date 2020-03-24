#![feature(try_trait)]
#![feature(proc_macro_hygiene)]
#![feature(associated_type_bounds)]
#![feature(trace_macros)]
#![feature(never_type)]
extern crate im_rc;
extern crate nom;
extern crate ena;
extern crate phf;

pub(crate) mod defs;
pub mod format;
pub mod syntax;
pub mod validation;
pub mod execution;

