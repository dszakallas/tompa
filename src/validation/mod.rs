use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use im_rc;

use crate::ast::*;

#[derive(Debug)]
pub struct TypeError;

impl<'a> Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("typing error"))
    }
}

impl Error for TypeError {}


type WrappedResult<A> = Result<A, TypeError>;

#[derive(Clone, Debug, Default)]
pub struct Context {
    types: im_rc::Vector<FuncType>,
    funcs: im_rc::Vector<FuncType>,
    tables: im_rc::Vector<TableType>,
    mems: im_rc::Vector<MemType>,
    globals: im_rc::Vector<GlobalType>,
    labels: im_rc::Vector<Option<ValType>>,
    locals: im_rc::Vector<ValType>,
    ret: Option<Option<ValType>>,
}

pub trait Type {
    type Value;
    type Parameters;
    type Context;

    fn check(&self, parameters: &Self::Parameters, context: &Self::Context) -> Result<Self::Value, TypeError>;
}

macro_rules! def_rule {
    ($syntax:tt => $tpe:ty, $check:expr) => {
        def_rule!($syntax(()) => $tpe, $check);
    };

    ($syntax:tt($params:ty) => $tpe:ty, $check:expr) => {
        def_rule!(() |- $syntax($params) => $tpe, $check);
    };

    ($ctx:ty |- $syntax:tt => $tpe:ty, $check:expr) => {
        def_rule!($ctx |- $syntax(()) => $tpe, $check);
    };

    ($ctx:ty |- $syntax:tt($params:ty) => $tpe:ty, $check:expr) => {
        impl Type for $syntax {
            type Value = $tpe;
            type Parameters = $params;
            type Context = $ctx;

            #[inline]
            fn check(&self, parameters: &Self::Parameters, context: &Self::Context) -> Result<Self::Value, TypeError> {
                $check(self, parameters, context)
            }
        }
   };
}

macro_rules! some_if {
    ($condition:expr, $some:expr) => {
        match $condition {
            true => Some($some),
            _ => None,
        }
    };
}

mod types;

// mod instructions;
//
// mod modules;
