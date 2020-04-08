use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::option::NoneError;

use im_rc;

use crate::ast::*;

#[derive(Debug)]
pub struct TypeError {
    syntax: String
}

impl<'a> Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!(
            "unable to type {}",
            self.syntax,
        ))
    }
}

impl Error for TypeError {}

struct TypeErrorWrapper {
    inner: Option<TypeError>,
}

impl From<NoneError> for TypeErrorWrapper {
    fn from(_: NoneError) -> Self {
        TypeErrorWrapper { inner: None }
    }
}

impl From<TypeError> for TypeErrorWrapper {
    fn from(err: TypeError) -> Self {
        TypeErrorWrapper { inner: Some(err) }
    }
}

type WrappedResult<A> = Result<A, TypeErrorWrapper>;

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

    #[inline]
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

                $check(self, parameters, context).map_err(|e: TypeErrorWrapper| {
                    e.inner.unwrap_or_else(move || type_error!(self))
                })
            }
        }
    };
}

macro_rules! type_error {
    ($syntax:expr) => {
        TypeError {
            syntax: format!("{:?}", $syntax),
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
