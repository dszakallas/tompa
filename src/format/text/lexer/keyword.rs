use phf::phf_map;
use crate::defs;

/*
    Hack: opcode is used as the enum ordinal for the keywords, as this way the enum can serve as
    the keyset for the instruction parser pfh. This is faster then using the underlying string and
    we don't have to implement counting in the macro. The side effect is that the other keywords get
    phony 'opcodes', so only use these if you are already sure that you are dealing with an instruction.
*/
macro_rules! to_keywords_cps {
    ($cb:ident($($args:tt)*) $($id:ident { params: $_params:expr, text: $kw:expr, opcode: $opcode:expr, $($_rest:tt)* }),*) => {
        $cb!{$($args)* $($id, $kw, $opcode;)*}
    }
}

macro_rules! with_misc_keywords_cps { ($cb:ident($($args:tt)*) $($stack:tt)*) => { $cb!{
    $($args)*
    $($stack)*
    I32,			            "i32";
    I64,			            "i64";
    F32,			            "f32";
    F64,			            "f64";
    Func,			            "func";
    Param,			            "param";
    Result,			            "result";
    Funcref,			        "funcref";
    Mut,			            "mut";
    OffsetEqU32,	            "offset=$u32";
    AlignEqU32,                 "align=$u32";
    Else,                       "else";
    End,                        "end";
    Type,                       "type";
}};
}

macro_rules! def_keywords {
    ($($id:ident, $kw:expr $(,$opcode:expr)?;)*) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum Keyword {
            $($id $(=$opcode as isize)*),*
        }

        pub static KEYWORDS_HASH: phf::Map<&'static str, Keyword> = phf_map! {
            $($kw => Keyword::$id),*
        };
    };
}

instruction_defs_cps!(to_keywords_cps(with_misc_keywords_cps(def_keywords())));
