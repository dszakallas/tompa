#![macro_use]

use phf::phf_map;

macro_rules! to_keywords_cps {
    ($cb:ident($($args:tt)*) $($id:ident; $_params:expr; $text:expr; $_parse_type:expr;)*) => {
        $cb!{$($args)* $($id; $text;)*}
    }
}

macro_rules! with_misc_keywords_cps { ($cb:ident($($args:tt)*) $($stack:tt)*) => { $cb!{
    $($args)*
    $($stack)*
    I32;			            "i32";
    I64;			            "i64";
    F32;			            "f32";
    F64;			            "f64";
    Func;			            "func";
    Param;			            "param";
    Result;			            "result";
    Funcref;			        "funcref";
    Mut;			            "mut";
    OffsetEqU32;	            "offset=$u32";
    AlignEqU32;                 "align=$u32";
    Else;                       "else";
    End;                        "end";
    Type;                       "type";
}};
}

macro_rules! def_keywords {
    ($($id:ident; $kw:expr;)*) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        #[repr(u8)]
        pub enum Keyword {
            $($id),*
        }

        pub static KEYWORDS_PHF: phf::Map<&'static str, Keyword> = phf_map! {
            $($kw => Keyword::$id),*
        };
    };
}

def_instruction_text_cps!(to_keywords_cps(with_misc_keywords_cps(def_keywords())));
