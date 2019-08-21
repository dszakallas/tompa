use crate::syntax::types::*;

use std::error::Error;
use std::fmt::Formatter;
use std::fmt::Display;
use std::fmt;
use std::collections::HashSet;

use im_rc;

#[derive(Debug)]
pub struct TypeError {
    rule: String,
    syntax: String,
}

impl<'a> Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_fmt(format_args!("{} does not satisfy {}", self.syntax, self.rule))
    }
}

impl Error for TypeError {}

#[derive(Clone, Debug)]
pub struct Context {
    types: im_rc::Vector<FuncType>,
    funcs: im_rc::Vector<FuncType>,
    tables: im_rc::Vector<TableType>,
    mems: im_rc::Vector<MemType>,
    globals: im_rc::Vector<GlobalType>,
    labels: im_rc::Vector<Option<ValType>>,
    locals: im_rc::Vector<ValType>,
    ret: Option<ValType>,
}

impl Context {
    pub fn empty() -> Context {
        Context {
            types: im_rc::vector![],
            funcs: im_rc::vector![],
            tables: im_rc::vector![],
            mems: im_rc::vector![],
            globals: im_rc::vector![],
            labels: im_rc::vector![],
            locals: im_rc::vector![],
            ret: None,
        }
    }
}

pub trait Type<S, T> {
    fn check(self, syntax: &S, context: &Context) -> Result<T, TypeError>;
}

macro_rules! rule {

    ($rulename:ident: $syntax:ty => $tpe:ty, $check:expr) => {
        rule!($rulename {}: $syntax => $tpe, $check);
    };

    ($rulename:ident { $($pn:ident: $pt:ty),* }: $syntax:ty => $tpe:ty, $check:expr) => {
        #[derive(Debug, Clone)]
        pub struct $rulename {
          $(
            pub $pn: $pt,
          )*
        }

        impl Type<$syntax, $tpe> for $rulename {
            fn check(self, syntax: &$syntax, context: &Context) -> Result<$tpe, TypeError> {
                $check(syntax, self, context)
            }
        }
    };
}

macro_rules! type_error {
    ($syntax:expr, $rule: expr) => {
        TypeError { rule: format!("{:?}", $rule), syntax: format!("{:?}", $syntax) }
    }
}

mod types;

mod instructions;

mod modules;




