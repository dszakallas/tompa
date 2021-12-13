#![macro_use]

use phf::phf_map;

use crate::ast::{Instruction, ResultType, GlobalIdx, LocalIdx, Memarg};

macro_rules! def_instruction_text_cps { ($cb:ident($($args:tt)*)) => { $cb!{
$($args)*
/*
Keep tabulated with column -t -s ";" -o ";" table > table_organized

Name             ; Parameters                                                                           ; Text                 ; Parse type ;
*/
Unreachable      ; ()                                                                                   ; "unreachable"        ; NoArg()    ;
Nop              ; ()                                                                                   ; "nop"                ; NoArg()    ;
Block            ; (result: ResultType, instrs: Vec<Instruction>)                                       ; "block"              ; Block()    ;
Loop             ; (result: ResultType, instrs: Vec<Instruction>)                                       ; "loop"               ; Loop()     ;
If               ; (result: ResultType, if_instrs: Vec<Instruction>, else_instrs: Vec<Instruction>)     ; "if"                 ; If()       ;
Br               ; ()                                                                                   ; "br"                 ; NoArg()    ;
BrIf             ; ()                                                                                   ; "br_if"              ; NoArg()    ;
BrTable          ; ()                                                                                   ; "br_table"           ; NoArg()    ;
Return           ; ()                                                                                   ; "return"             ; NoArg()    ;
Call             ; ()                                                                                   ; "call"               ; NoArg()    ;
CallIndirect     ; ()                                                                                   ; "call_indirect"      ; NoArg()    ;
Drop             ; ()                                                                                   ; "drop"               ; NoArg()    ;
Select           ; ()                                                                                   ; "select"             ; NoArg()    ;
LocalGet         ; (localidx: LocalIdx)                                                                 ; "local.get"          ; LocalIdx() ;
LocalSet         ; (localidx: LocalIdx)                                                                 ; "local.set"          ; LocalIdx() ;
LocalTee         ; (localidx: LocalIdx)                                                                 ; "local.tee"          ; LocalIdx() ;
GlobalGet        ; (globalidx: GlobalIdx)                                                               ; "global.get"         ; GlobalIdx();
GlobalSet        ; (globalidx: GlobalIdx)                                                               ; "global.set"         ; GlobalIdx();
I32Load          ; (memarg: Memarg)                                                                     ; "i32.load"           ; MemLs(4)   ;
I64Load          ; (memarg: Memarg)                                                                     ; "i64.load"           ; MemLs(8)   ;
F32Load          ; (memarg: Memarg)                                                                     ; "f32.load"           ; MemLs(4)   ;
F64Load          ; (memarg: Memarg)                                                                     ; "f64.load"           ; MemLs(8)   ;
I32Load8S        ; (memarg: Memarg)                                                                     ; "i32.load8_s"        ; MemLs(1)   ;
I32Load8U        ; (memarg: Memarg)                                                                     ; "i32.load8_u"        ; MemLs(1)   ;
I32Load16S       ; (memarg: Memarg)                                                                     ; "i32.load16_s"       ; MemLs(2)   ;
I32Load16U       ; (memarg: Memarg)                                                                     ; "i32.load16_u"       ; MemLs(2)   ;
I64Load8S        ; (memarg: Memarg)                                                                     ; "i64.load8_s"        ; MemLs(1)   ;
I64Load8U        ; (memarg: Memarg)                                                                     ; "i64.load8_u"        ; MemLs(1)   ;
I64Load16S       ; (memarg: Memarg)                                                                     ; "i64.load16_s"       ; MemLs(2)   ;
I64Load16U       ; (memarg: Memarg)                                                                     ; "i64.load16_u"       ; MemLs(2)   ;
I64Load32S       ; (memarg: Memarg)                                                                     ; "i64.load32_s"       ; MemLs(4)   ;
I64Load32U       ; (memarg: Memarg)                                                                     ; "i64.load32_u"       ; MemLs(4)   ;
I32Store         ; (memarg: Memarg)                                                                     ; "i32.store"          ; MemLs(4)   ;
I64Store         ; (memarg: Memarg)                                                                     ; "i64.store"          ; MemLs(8)   ;
F32Store         ; (memarg: Memarg)                                                                     ; "f32.store"          ; MemLs(4)   ;
F64Store         ; (memarg: Memarg)                                                                     ; "f64.store"          ; MemLs(8)   ;
I32Store8        ; (memarg: Memarg)                                                                     ; "i32.store8"         ; MemLs(1)   ;
I32Store16       ; (memarg: Memarg)                                                                     ; "i32.store16"        ; MemLs(2)   ;
I64Store8        ; (memarg: Memarg)                                                                     ; "i64.store8"         ; MemLs(1)   ;
I64Store16       ; (memarg: Memarg)                                                                     ; "i64.store16"        ; MemLs(2)   ;
I64Store32       ; (memarg: Memarg)                                                                     ; "i64.store32"        ; MemLs(4)   ;
MemorySize       ; ()                                                                                   ; "memory.size"        ; NoArg()    ;
MemoryGrow       ; ()                                                                                   ; "memory.grow"        ; NoArg()    ;
I32Const         ; (param: u32)                                                                         ; "i32.const"          ; ConstI32() ;
I64Const         ; (param: u64)                                                                         ; "i64.const"          ; ConstI64() ;
F32Const         ; (param: f32)                                                                         ; "f32.const"          ; ConstF32() ;
F64Const         ; (param: f64)                                                                         ; "f64.const"          ; ConstF64() ;
I32Eqz           ; ()                                                                                   ; "i32.eqz"            ; NoArg()    ;
I32Eq            ; ()                                                                                   ; "i32.eq"             ; NoArg()    ;
I32Ne            ; ()                                                                                   ; "i32.ne"             ; NoArg()    ;
I32LtS           ; ()                                                                                   ; "i32.lt_s"           ; NoArg()    ;
I32LtU           ; ()                                                                                   ; "i32.lt_u"           ; NoArg()    ;
I32GtS           ; ()                                                                                   ; "i32.gt_s"           ; NoArg()    ;
I32GtU           ; ()                                                                                   ; "i32.gt_u"           ; NoArg()    ;
I32LeS           ; ()                                                                                   ; "i32.le_s"           ; NoArg()    ;
I32LeU           ; ()                                                                                   ; "i32.le_u"           ; NoArg()    ;
I32GeS           ; ()                                                                                   ; "i32.ge_s"           ; NoArg()    ;
I32GeU           ; ()                                                                                   ; "i32.ge_u"           ; NoArg()    ;
I64Eqz           ; ()                                                                                   ; "i64.eqz"            ; NoArg()    ;
I64Eq            ; ()                                                                                   ; "i64.eq"             ; NoArg()    ;
I64Ne            ; ()                                                                                   ; "i64.ne"             ; NoArg()    ;
I64LtS           ; ()                                                                                   ; "i64.lt_s"           ; NoArg()    ;
I64LtU           ; ()                                                                                   ; "i64.lt_u"           ; NoArg()    ;
I64GtS           ; ()                                                                                   ; "i64.gt_s"           ; NoArg()    ;
I64GtU           ; ()                                                                                   ; "i64.gt_u"           ; NoArg()    ;
I64LeS           ; ()                                                                                   ; "i64.le_s"           ; NoArg()    ;
I64LeU           ; ()                                                                                   ; "i64.le_u"           ; NoArg()    ;
I64GeS           ; ()                                                                                   ; "i64.ge_s"           ; NoArg()    ;
I64GeU           ; ()                                                                                   ; "i64.ge_u"           ; NoArg()    ;
F32Eq            ; ()                                                                                   ; "f32.eq"             ; NoArg()    ;
F32Ne            ; ()                                                                                   ; "f32.ne"             ; NoArg()    ;
F32Lt            ; ()                                                                                   ; "f32.lt"             ; NoArg()    ;
F32Gt            ; ()                                                                                   ; "f32.gt"             ; NoArg()    ;
F32Le            ; ()                                                                                   ; "f32.le"             ; NoArg()    ;
F32Ge            ; ()                                                                                   ; "f32.ge"             ; NoArg()    ;
F64Eq            ; ()                                                                                   ; "f64.eq"             ; NoArg()    ;
F64Ne            ; ()                                                                                   ; "f64.ne"             ; NoArg()    ;
F64Lt            ; ()                                                                                   ; "f64.lt"             ; NoArg()    ;
F64Gt            ; ()                                                                                   ; "f64.gt"             ; NoArg()    ;
F64Le            ; ()                                                                                   ; "f64.le"             ; NoArg()    ;
F64Ge            ; ()                                                                                   ; "f64.ge"             ; NoArg()    ;
I32Clz           ; ()                                                                                   ; "i32.clz"            ; NoArg()    ;
I32Ctz           ; ()                                                                                   ; "i32.ctz"            ; NoArg()    ;
I32Popcnt        ; ()                                                                                   ; "i32.popcnt"         ; NoArg()    ;
I32Add           ; ()                                                                                   ; "i32.add"            ; NoArg()    ;
I32Sub           ; ()                                                                                   ; "i32.sub"            ; NoArg()    ;
I32Mul           ; ()                                                                                   ; "i32.mul"            ; NoArg()    ;
I32DivS          ; ()                                                                                   ; "i32.div_s"          ; NoArg()    ;
I32DivU          ; ()                                                                                   ; "i32.div_u"          ; NoArg()    ;
I32RemS          ; ()                                                                                   ; "i32.rem_s"          ; NoArg()    ;
I32RemU          ; ()                                                                                   ; "i32.rem_u"          ; NoArg()    ;
I32And           ; ()                                                                                   ; "i32.and"            ; NoArg()    ;
I32Or            ; ()                                                                                   ; "i32.or"             ; NoArg()    ;
I32Xor           ; ()                                                                                   ; "i32.xor"            ; NoArg()    ;
I32Shl           ; ()                                                                                   ; "i32.shl"            ; NoArg()    ;
I32ShrS          ; ()                                                                                   ; "i32.shr_s"          ; NoArg()    ;
I32ShrU          ; ()                                                                                   ; "i32.shr_u"          ; NoArg()    ;
I32Rotl          ; ()                                                                                   ; "i32.rotl"           ; NoArg()    ;
I32Rotr          ; ()                                                                                   ; "i32.rotr"           ; NoArg()    ;
I64Clz           ; ()                                                                                   ; "i64.clz"            ; NoArg()    ;
I64Ctz           ; ()                                                                                   ; "i64.ctz"            ; NoArg()    ;
I64Popcnt        ; ()                                                                                   ; "i64.popcnt"         ; NoArg()    ;
I64Add           ; ()                                                                                   ; "i64.add"            ; NoArg()    ;
I64Sub           ; ()                                                                                   ; "i64.sub"            ; NoArg()    ;
I64Mul           ; ()                                                                                   ; "i64.mul"            ; NoArg()    ;
I64DivS          ; ()                                                                                   ; "i64.div_s"          ; NoArg()    ;
I64DivU          ; ()                                                                                   ; "i64.div_u"          ; NoArg()    ;
I64RemS          ; ()                                                                                   ; "i64.rem_s"          ; NoArg()    ;
I64RemU          ; ()                                                                                   ; "i64.rem_u"          ; NoArg()    ;
I64And           ; ()                                                                                   ; "i64.and"            ; NoArg()    ;
I64Or            ; ()                                                                                   ; "i64.or"             ; NoArg()    ;
I64Xor           ; ()                                                                                   ; "i64.xor"            ; NoArg()    ;
I64Shl           ; ()                                                                                   ; "i64.shl"            ; NoArg()    ;
I64ShrS          ; ()                                                                                   ; "i64.shr_s"          ; NoArg()    ;
I64ShrU          ; ()                                                                                   ; "i64.shr_u"          ; NoArg()    ;
I64Rotl          ; ()                                                                                   ; "i64.rotl"           ; NoArg()    ;
I64Rotr          ; ()                                                                                   ; "i64.rotr"           ; NoArg()    ;
F32Abs           ; ()                                                                                   ; "f32.abs"            ; NoArg()    ;
F32Neg           ; ()                                                                                   ; "f32.neg"            ; NoArg()    ;
F32Ceil          ; ()                                                                                   ; "f32.ceil"           ; NoArg()    ;
F32Floor         ; ()                                                                                   ; "f32.floor"          ; NoArg()    ;
F32Trunc         ; ()                                                                                   ; "f32.trunc"          ; NoArg()    ;
F32Nearest       ; ()                                                                                   ; "f32.nearest"        ; NoArg()    ;
F32Sqrt          ; ()                                                                                   ; "f32.sqrt"           ; NoArg()    ;
F32Add           ; ()                                                                                   ; "f32.add"            ; NoArg()    ;
F32Sub           ; ()                                                                                   ; "f32.sub"            ; NoArg()    ;
F32Mul           ; ()                                                                                   ; "f32.mul"            ; NoArg()    ;
F32Div           ; ()                                                                                   ; "f32.div"            ; NoArg()    ;
F32Min           ; ()                                                                                   ; "f32.min"            ; NoArg()    ;
F32Max           ; ()                                                                                   ; "f32.max"            ; NoArg()    ;
F32Copysign      ; ()                                                                                   ; "f32.copysign"       ; NoArg()    ;
F64Abs           ; ()                                                                                   ; "f64.abs"            ; NoArg()    ;
F64Neg           ; ()                                                                                   ; "f64.neg"            ; NoArg()    ;
F64Ceil          ; ()                                                                                   ; "f64.ceil"           ; NoArg()    ;
F64Floor         ; ()                                                                                   ; "f64.floor"          ; NoArg()    ;
F64Trunc         ; ()                                                                                   ; "f64.trunc"          ; NoArg()    ;
F64Nearest       ; ()                                                                                   ; "f64.nearest"        ; NoArg()    ;
F64Sqrt          ; ()                                                                                   ; "f64.sqrt"           ; NoArg()    ;
F64Add           ; ()                                                                                   ; "f64.add"            ; NoArg()    ;
F64Sub           ; ()                                                                                   ; "f64.sub"            ; NoArg()    ;
F64Mul           ; ()                                                                                   ; "f64.mul"            ; NoArg()    ;
F64Div           ; ()                                                                                   ; "f64.div"            ; NoArg()    ;
F64Min           ; ()                                                                                   ; "f64.min"            ; NoArg()    ;
F64Max           ; ()                                                                                   ; "f64.max"            ; NoArg()    ;
F64Copysign      ; ()                                                                                   ; "f64.copysign"       ; NoArg()    ;
I32WrapI64       ; ()                                                                                   ; "i32.wrap_i64"       ; NoArg()    ;
I32TruncF32S     ; ()                                                                                   ; "i32.trunc_f32_s"    ; NoArg()    ;
I32TruncF32U     ; ()                                                                                   ; "i32.trunc_f32_u"    ; NoArg()    ;
I32TruncF64S     ; ()                                                                                   ; "i32.trunc_f64_s"    ; NoArg()    ;
I32TruncF64U     ; ()                                                                                   ; "i32.trunc_f64_u"    ; NoArg()    ;
I64ExtendI32S    ; ()                                                                                   ; "i64.extend_i32_s"   ; NoArg()    ;
I64ExtendI32U    ; ()                                                                                   ; "i64.extend_i32_u"   ; NoArg()    ;
I64TruncF32S     ; ()                                                                                   ; "i64.trunc_f32_s"    ; NoArg()    ;
I64TruncF32U     ; ()                                                                                   ; "i64.trunc_f32_u"    ; NoArg()    ;
I64TruncF64S     ; ()                                                                                   ; "i64.trunc_f64_s"    ; NoArg()    ;
I64TruncF64U     ; ()                                                                                   ; "i64.trunc_f64_u"    ; NoArg()    ;
F32ConvertI32S   ; ()                                                                                   ; "f32.convert_i32_s"  ; NoArg()    ;
F32ConvertI32U   ; ()                                                                                   ; "f32.convert_i32_u"  ; NoArg()    ;
F32ConvertI64S   ; ()                                                                                   ; "f32.convert_i64_s"  ; NoArg()    ;
F32ConvertI64U   ; ()                                                                                   ; "f32.convert_i64_u"  ; NoArg()    ;
F32DemoteF64     ; ()                                                                                   ; "f32.demote_f64"     ; NoArg()    ;
F64ConvertI32S   ; ()                                                                                   ; "f64.convert_i32_s"  ; NoArg()    ;
F64ConvertI32U   ; ()                                                                                   ; "f64.convert_i32_u"  ; NoArg()    ;
F64ConvertI64S   ; ()                                                                                   ; "f64.convert_i64_s"  ; NoArg()    ;
F64ConvertI64U   ; ()                                                                                   ; "f64.convert_i64_u"  ; NoArg()    ;
F64PromoteF32    ; ()                                                                                   ; "f64.promote_f32"    ; NoArg()    ;
I32ReinterpretF32; ()                                                                                   ; "i32.reinterpret_f32"; NoArg()    ;
I64ReinterpretF64; ()                                                                                   ; "i64.reinterpret_f64"; NoArg()    ;
F32ReinterpretI32; ()                                                                                   ; "f32.reinterpret_i32"; NoArg()    ;
F64ReinterpretI64; ()                                                                                   ; "f64.reinterpret_i64"; NoArg()    ;
}};
}


#[derive(Clone, Debug)]
pub enum InstrParseType {
    NoArg(fn() -> Instruction),
    Block(fn(ResultType, Vec<Instruction>) -> Instruction),
    Loop(fn(ResultType, Vec<Instruction>) -> Instruction),
    If(fn(ResultType, Vec<Instruction>, Vec<Instruction>) -> Instruction),
    LocalIdx(fn(LocalIdx) -> Instruction),
    GlobalIdx(fn(GlobalIdx) -> Instruction),
    // LabelIdx(fn(LabelIdx) -> Instruction),
    // LabelIdxN(fn(Vec<LabelIdx>) -> Instruction),
    // FuncIdx(fn(FuncIdx) -> Instruction),
    // TypeIdx(fn(TypeIdx) -> Instruction),
    MemLs(u32, fn(Memarg) -> Instruction),
    ConstI32(fn(u32) -> Instruction),
    ConstI64(fn(u64) -> Instruction),
    ConstF32(fn(f32) -> Instruction),
    ConstF64(fn(f64) -> Instruction),
}

macro_rules! def_instruction_text_parser_phf {
    ($($id:ident; ($($pname:ident: $pty:ty),*); $text:expr; $parse_tpe:ident($($arg:expr),*);)*) => {
        pub(crate) static INSTR_PARSERS: phf::Map<&'static str, InstrParseType> = phf_map! {
            $(
                $text => InstrParseType::$parse_tpe($($arg,)* |$($pname: $pty),*| crate::ast::Instruction::$id(crate::ast::$id { $($pname),* }))
            ),*
        };
    }
}

def_instruction_text_cps!(def_instruction_text_parser_phf());
