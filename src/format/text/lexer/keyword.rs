use phf::phf_map;

macro_rules! keywords {
    ($($name:ident $str:expr)*) => {
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        pub enum Keyword {
            $($name,)*
        }

        pub static KEYWORDS_HASH: phf::Map<&'static str, Keyword> = phf_map! {
            $($str => Keyword::$name,)*
        };
    }
}

keywords! {
    I32			            "i32"
    I64			            "i64"
    F32			            "f32"
    F64			            "f64"
    Func			        "func"
    Param			        "param"
    Result			        "result"
    Funcref			        "funcref"
    Mut			            "mut"
    Block			        "block"
    End			            "end"
    Loop			        "loop"
    If			            "if"
    Then                    "then"
    Else			        "else"
    Unreachable			    "unreachable"
    Nop			            "nop"
    Br			            "br"
    BrIf			        "br_if"
    BrTable			        "br_table"
    Return			        "return"
    CallIndirect			"call"
    Drop			        "drop"
    Select			        "select"
    LocalGet			    "local.get"
    LocalSet			    "local.set"
    LocalTee			    "local.tee"
    GlobalGet               "global.get"
    GlobalSet			    "global.set"
//OffsetEq(NumParts<I>)	"offset=(num)"
//AlignEq(NUmParts<I>)	"align=(num"
    I32Load			        "i32.load"
    I64Load			        "i64.load"
    F32Load			        "f32.load"
    F64Load			        "f64.load"
    I32Load8S			    "i32.load8_s"
    I32Load8U			    "i32.load8_u"
    I32Load16S			    "i32.load16_s"
    I32Load16U			    "i32.load16_u"
    I64Load8S			    "i64.load8_s"
    I64Load8U			    "i64.load8_u"
    I64Load16S			    "i64.load16_s"
    I64Load16U			    "i64.load16_u"
    I64Load32S			    "i64.load32_s"
    I64Load32U			    "i64.load32_u"
    I32Store                "i32.store"
    I64Store                "i64.store"
    F32Store                "f32.store"
    F64Store                "f64.store"
    I32Store8               "i32.store8"
    I32Store16              "i32.store16"
    I64Store8               "i64.store8"
    I64Store16              "i64.store16"
    I64Store32              "i64.store32"
    MemorySize              "memory.size"
    MemoryGrow              "memory.grow"
    // define rest...
}
