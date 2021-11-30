#![macro_use]

use super::types::*;

// Note: This, as most macros in this crate are written in continuation passing style
// which enables us to reuse it in other macros. See https://danielkeep.github.io/tlborm/book/pat-callbacks.html
macro_rules! instruction_defs_cps { ($cb:ident($($args:tt)*)) => { $cb!{
$($args)*
/*
Keep tabulated with column -t -s ";" -o ";" table > table_organized

Name             ; Parameters                                                                           ; Text                 ; Opcode ; Parse type ; Typecheck rule                                               ;
*/
Unreachable      ; ()                                                                                   ; "unreachable"        ; 0x00i32; NoArg()    ; UnreachableType { }                                          ;
Nop              ; ()                                                                                   ; "nop"                ; 0x01i32; NoArg()    ; NopType { }                                                  ;
Block            ; (result: ResultType, instrs: Vec<Instruction>)                                  ; "block"              ; 0x02i32; Block()    ; BlockType {}                                                 ;
Loop             ; (result: ResultType, instrs: Vec<Instruction>)                                  ; "loop"               ; 0x03i32; Loop()     ; LoopType {}                                                  ;
If               ; (result: ResultType, if_instrs: Vec<Instruction>, else_instrs: Vec<Instruction>); "if"                 ; 0x04i32; If()       ; IfType {}                                                    ;
Br               ; ()                                                                                   ; "br"                 ; 0x0ci32; NoArg()    ; BrType {}                                                    ;
BrIf             ; ()                                                                                   ; "br_if"              ; 0x0di32; NoArg()    ; BrIfType {}                                                  ;
BrTable          ; ()                                                                                   ; "br_table"           ; 0x0ei32; NoArg()    ; BrTableType {}                                               ;
Return           ; ()                                                                                   ; "return"             ; 0x0fi32; NoArg()    ; ReturnType{}                                                 ;
Call             ; ()                                                                                   ; "call"               ; 0x10i32; NoArg()    ; CallType{}                                                   ;
CallIndirect     ; ()                                                                                   ; "call_indirect"      ; 0x11i32; NoArg()    ; CallIndirectType{}                                           ;
Drop             ; ()                                                                                   ; "drop"               ; 0x1ai32; NoArg()    ; ()                                                           ;
Select           ; ()                                                                                   ; "select"             ; 0x1bi32; NoArg()    ; ()                                                           ;
LocalGet         ; (localidx: LocalIdx)                                                                 ; "local.get"          ; 0x20i32; LocalIdx() ; ()                                                           ;
LocalSet         ; (localidx: LocalIdx)                                                                 ; "local.set"          ; 0x21i32; LocalIdx() ; ()                                                           ;
LocalTee         ; (localidx: LocalIdx)                                                                 ; "local.tee"          ; 0x22i32; LocalIdx() ; ()                                                           ;
GlobalGet        ; (globalidx: GlobalIdx)                                                               ; "global.get"         ; 0x23i32; GlobalIdx(); ()                                                           ;
GlobalSet        ; (globalidx: GlobalIdx)                                                               ; "global.set"         ; 0x24i32; GlobalIdx(); ()                                                           ;
I32Load          ; (memarg: Memarg)                                                                     ; "i32.load"           ; 0x28i32; MemLs(4)   ; LoadType { valtype: ValType::I32, storage: None }            ;
I64Load          ; (memarg: Memarg)                                                                     ; "i64.load"           ; 0x29i32; MemLs(8)   ; LoadType { valtype: ValType::I64, storage: None }            ;
F32Load          ; (memarg: Memarg)                                                                     ; "f32.load"           ; 0x2ai32; MemLs(4)   ; LoadType { valtype: ValType::F32, storage: None }            ;
F64Load          ; (memarg: Memarg)                                                                     ; "f64.load"           ; 0x2bi32; MemLs(8)   ; LoadType { valtype: ValType::F64, storage: None }            ;
I32Load8S        ; (memarg: Memarg)                                                                     ; "i32.load8_s"        ; 0x2ci32; MemLs(1)   ; LoadType { valtype: ValType::I32, storage: Some((1, Sx::S)) };
I32Load8U        ; (memarg: Memarg)                                                                     ; "i32.load8_u"        ; 0x2di32; MemLs(1)   ; LoadType { valtype: ValType::I32, storage: Some((1, Sx::U)) };
I32Load16S       ; (memarg: Memarg)                                                                     ; "i32.load16_s"       ; 0x2ei32; MemLs(2)   ; LoadType { valtype: ValType::I32, storage: Some((2, Sx::S)) };
I32Load16U       ; (memarg: Memarg)                                                                     ; "i32.load16_u"       ; 0x2fi32; MemLs(2)   ; LoadType { valtype: ValType::I32, storage: Some((2, Sx::U)) };
I64Load8S        ; (memarg: Memarg)                                                                     ; "i64.load8_s"        ; 0x30i32; MemLs(1)   ; LoadType { valtype: ValType::I64, storage: Some((1, Sx::S)) };
I64Load8U        ; (memarg: Memarg)                                                                     ; "i64.load8_u"        ; 0x31i32; MemLs(1)   ; LoadType { valtype: ValType::I64, storage: Some((1, Sx::U)) };
I64Load16S       ; (memarg: Memarg)                                                                     ; "i64.load16_s"       ; 0x32i32; MemLs(2)   ; LoadType { valtype: ValType::I64, storage: Some((2, Sx::S)) };
I64Load16U       ; (memarg: Memarg)                                                                     ; "i64.load16_u"       ; 0x33i32; MemLs(2)   ; LoadType { valtype: ValType::I64, storage: Some((3, Sx::U)) };
I64Load32S       ; (memarg: Memarg)                                                                     ; "i64.load32_s"       ; 0x34i32; MemLs(4)   ; LoadType { valtype: ValType::I64, storage: Some((4, Sx::S)) };
I64Load32U       ; (memarg: Memarg)                                                                     ; "i64.load32_u"       ; 0x35i32; MemLs(4)   ; LoadType { valtype: ValType::I64, storage: Some((4, Sx::U)) };
I32Store         ; (memarg: Memarg)                                                                     ; "i32.store"          ; 0x36i32; MemLs(4)   ; StoreType { valtype: ValType::I32, storage: None }           ;
I64Store         ; (memarg: Memarg)                                                                     ; "i64.store"          ; 0x37i32; MemLs(8)   ; StoreType { valtype: ValType::I64, storage: None }           ;
F32Store         ; (memarg: Memarg)                                                                     ; "f32.store"          ; 0x38i32; MemLs(4)   ; StoreType { valtype: ValType::F32, storage: None }           ;
F64Store         ; (memarg: Memarg)                                                                     ; "f64.store"          ; 0x39i32; MemLs(8)   ; StoreType { valtype: ValType::F64, storage: None }           ;
I32Store8        ; (memarg: Memarg)                                                                     ; "i32.store8"         ; 0x3ai32; MemLs(1)   ; StoreType { valtype: ValType::I32, storage: Some(1) }        ;
I32Store16       ; (memarg: Memarg)                                                                     ; "i32.store16"        ; 0x3bi32; MemLs(2)   ; StoreType { valtype: ValType::I32, storage: Some(2) }        ;
I64Store8        ; (memarg: Memarg)                                                                     ; "i64.store8"         ; 0x3ci32; MemLs(1)   ; StoreType { valtype: ValType::I64, storage: Some(1) }        ;
I64Store16       ; (memarg: Memarg)                                                                     ; "i64.store16"        ; 0x3di32; MemLs(2)   ; StoreType { valtype: ValType::I64, storage: Some(2) }        ;
I64Store32       ; (memarg: Memarg)                                                                     ; "i64.store32"        ; 0x3ei32; MemLs(4)   ; StoreType { valtype: ValType::I64, storage: Some(4) }        ;
MemorySize       ; ()                                                                                   ; "memory.size"        ; 0x3fi32; NoArg()    ; ()                                                           ;
MemoryGrow       ; ()                                                                                   ; "memory.grow"        ; 0x40i32; NoArg()    ; ()                                                           ;
I32Const         ; (param: u32)                                                                         ; "i32.const"          ; 0x41i32; ConstI32() ; ()                                                           ;
I64Const         ; (param: u64)                                                                         ; "i64.const"          ; 0x42i32; ConstI64() ; ()                                                           ;
F32Const         ; (param: f32)                                                                         ; "f32.const"          ; 0x43i32; ConstF32() ; ()                                                           ;
F64Const         ; (param: f64)                                                                         ; "f64.const"          ; 0x44i32; ConstF64() ; ()                                                           ;
I32Eqz           ; ()                                                                                   ; "i32.eqz"            ; 0x45i32; NoArg()    ; ()                                                           ;
I32Eq            ; ()                                                                                   ; "i32.eq"             ; 0x46i32; NoArg()    ; ()                                                           ;
I32Ne            ; ()                                                                                   ; "i32.ne"             ; 0x47i32; NoArg()    ; ()                                                           ;
I32LtS           ; ()                                                                                   ; "i32.lt_s"           ; 0x48i32; NoArg()    ; ()                                                           ;
I32LtU           ; ()                                                                                   ; "i32.lt_u"           ; 0x49i32; NoArg()    ; ()                                                           ;
I32GtS           ; ()                                                                                   ; "i32.gt_s"           ; 0x4ai32; NoArg()    ; ()                                                           ;
I32GtU           ; ()                                                                                   ; "i32.gt_u"           ; 0x4bi32; NoArg()    ; ()                                                           ;
I32LeS           ; ()                                                                                   ; "i32.le_s"           ; 0x4ci32; NoArg()    ; ()                                                           ;
I32LeU           ; ()                                                                                   ; "i32.le_u"           ; 0x4di32; NoArg()    ; ()                                                           ;
I32GeS           ; ()                                                                                   ; "i32.ge_s"           ; 0x4ei32; NoArg()    ; ()                                                           ;
I32GeU           ; ()                                                                                   ; "i32.ge_u"           ; 0x4fi32; NoArg()    ; ()                                                           ;
I64Eqz           ; ()                                                                                   ; "i64.eqz"            ; 0x50i32; NoArg()    ; ()                                                           ;
I64Eq            ; ()                                                                                   ; "i64.eq"             ; 0x51i32; NoArg()    ; ()                                                           ;
I64Ne            ; ()                                                                                   ; "i64.ne"             ; 0x52i32; NoArg()    ; ()                                                           ;
I64LtS           ; ()                                                                                   ; "i64.lt_s"           ; 0x53i32; NoArg()    ; ()                                                           ;
I64LtU           ; ()                                                                                   ; "i64.lt_u"           ; 0x54i32; NoArg()    ; ()                                                           ;
I64GtS           ; ()                                                                                   ; "i64.gt_s"           ; 0x55i32; NoArg()    ; ()                                                           ;
I64GtU           ; ()                                                                                   ; "i64.gt_u"           ; 0x56i32; NoArg()    ; ()                                                           ;
I64LeS           ; ()                                                                                   ; "i64.le_s"           ; 0x57i32; NoArg()    ; ()                                                           ;
I64LeU           ; ()                                                                                   ; "i64.le_u"           ; 0x58i32; NoArg()    ; ()                                                           ;
I64GeS           ; ()                                                                                   ; "i64.ge_s"           ; 0x59i32; NoArg()    ; ()                                                           ;
I64GeU           ; ()                                                                                   ; "i64.ge_u"           ; 0x5ai32; NoArg()    ; ()                                                           ;
F32Eq            ; ()                                                                                   ; "f32.eq"             ; 0x5bi32; NoArg()    ; ()                                                           ;
F32Ne            ; ()                                                                                   ; "f32.ne"             ; 0x5ci32; NoArg()    ; ()                                                           ;
F32Lt            ; ()                                                                                   ; "f32.lt"             ; 0x5di32; NoArg()    ; ()                                                           ;
F32Gt            ; ()                                                                                   ; "f32.gt"             ; 0x5ei32; NoArg()    ; ()                                                           ;
F32Le            ; ()                                                                                   ; "f32.le"             ; 0x5fi32; NoArg()    ; ()                                                           ;
F32Ge            ; ()                                                                                   ; "f32.ge"             ; 0x60i32; NoArg()    ; ()                                                           ;
F64Eq            ; ()                                                                                   ; "f64.eq"             ; 0x61i32; NoArg()    ; ()                                                           ;
F64Ne            ; ()                                                                                   ; "f64.ne"             ; 0x62i32; NoArg()    ; ()                                                           ;
F64Lt            ; ()                                                                                   ; "f64.lt"             ; 0x63i32; NoArg()    ; ()                                                           ;
F64Gt            ; ()                                                                                   ; "f64.gt"             ; 0x64i32; NoArg()    ; ()                                                           ;
F64Le            ; ()                                                                                   ; "f64.le"             ; 0x65i32; NoArg()    ; ()                                                           ;
F64Ge            ; ()                                                                                   ; "f64.ge"             ; 0x66i32; NoArg()    ; ()                                                           ;
I32Clz           ; ()                                                                                   ; "i32.clz"            ; 0x67i32; NoArg()    ; ()                                                           ;
I32Ctz           ; ()                                                                                   ; "i32.ctz"            ; 0x68i32; NoArg()    ; ()                                                           ;
I32Popcnt        ; ()                                                                                   ; "i32.popcnt"         ; 0x69i32; NoArg()    ; ()                                                           ;
I32Add           ; ()                                                                                   ; "i32.add"            ; 0x6ai32; NoArg()    ; ()                                                           ;
I32Sub           ; ()                                                                                   ; "i32.sub"            ; 0x6bi32; NoArg()    ; ()                                                           ;
I32Mul           ; ()                                                                                   ; "i32.mul"            ; 0x6ci32; NoArg()    ; ()                                                           ;
I32DivS          ; ()                                                                                   ; "i32.div_s"          ; 0x6di32; NoArg()    ; ()                                                           ;
I32DivU          ; ()                                                                                   ; "i32.div_u"          ; 0x6ei32; NoArg()    ; ()                                                           ;
I32RemS          ; ()                                                                                   ; "i32.rem_s"          ; 0x6fi32; NoArg()    ; ()                                                           ;
I32RemU          ; ()                                                                                   ; "i32.rem_u"          ; 0x70i32; NoArg()    ; ()                                                           ;
I32And           ; ()                                                                                   ; "i32.and"            ; 0x71i32; NoArg()    ; ()                                                           ;
I32Or            ; ()                                                                                   ; "i32.or"             ; 0x72i32; NoArg()    ; ()                                                           ;
I32Xor           ; ()                                                                                   ; "i32.xor"            ; 0x73i32; NoArg()    ; ()                                                           ;
I32Shl           ; ()                                                                                   ; "i32.shl"            ; 0x74i32; NoArg()    ; ()                                                           ;
I32ShrS          ; ()                                                                                   ; "i32.shr_s"          ; 0x75i32; NoArg()    ; ()                                                           ;
I32ShrU          ; ()                                                                                   ; "i32.shr_u"          ; 0x76i32; NoArg()    ; ()                                                           ;
I32Rotl          ; ()                                                                                   ; "i32.rotl"           ; 0x77i32; NoArg()    ; ()                                                           ;
I32Rotr          ; ()                                                                                   ; "i32.rotr"           ; 0x78i32; NoArg()    ; ()                                                           ;
I64Clz           ; ()                                                                                   ; "i64.clz"            ; 0x79i32; NoArg()    ; ()                                                           ;
I64Ctz           ; ()                                                                                   ; "i64.ctz"            ; 0x7ai32; NoArg()    ; ()                                                           ;
I64Popcnt        ; ()                                                                                   ; "i64.popcnt"         ; 0x7bi32; NoArg()    ; ()                                                           ;
I64Add           ; ()                                                                                   ; "i64.add"            ; 0x7ci32; NoArg()    ; ()                                                           ;
I64Sub           ; ()                                                                                   ; "i64.sub"            ; 0x7di32; NoArg()    ; ()                                                           ;
I64Mul           ; ()                                                                                   ; "i64.mul"            ; 0x7ei32; NoArg()    ; ()                                                           ;
I64DivS          ; ()                                                                                   ; "i64.div_s"          ; 0x7fi32; NoArg()    ; ()                                                           ;
I64DivU          ; ()                                                                                   ; "i64.div_u"          ; 0x80i32; NoArg()    ; ()                                                           ;
I64RemS          ; ()                                                                                   ; "i64.rem_s"          ; 0x81i32; NoArg()    ; ()                                                           ;
I64RemU          ; ()                                                                                   ; "i64.rem_u"          ; 0x82i32; NoArg()    ; ()                                                           ;
I64And           ; ()                                                                                   ; "i64.and"            ; 0x83i32; NoArg()    ; ()                                                           ;
I64Or            ; ()                                                                                   ; "i64.or"             ; 0x84i32; NoArg()    ; ()                                                           ;
I64Xor           ; ()                                                                                   ; "i64.xor"            ; 0x85i32; NoArg()    ; ()                                                           ;
I64Shl           ; ()                                                                                   ; "i64.shl"            ; 0x86i32; NoArg()    ; ()                                                           ;
I64ShrS          ; ()                                                                                   ; "i64.shr_s"          ; 0x87i32; NoArg()    ; ()                                                           ;
I64ShrU          ; ()                                                                                   ; "i64.shr_u"          ; 0x88i32; NoArg()    ; ()                                                           ;
I64Rotl          ; ()                                                                                   ; "i64.rotl"           ; 0x89i32; NoArg()    ; ()                                                           ;
I64Rotr          ; ()                                                                                   ; "i64.rotr"           ; 0x8ai32; NoArg()    ; ()                                                           ;
F32Abs           ; ()                                                                                   ; "f32.abs"            ; 0x8bi32; NoArg()    ; ()                                                           ;
F32Neg           ; ()                                                                                   ; "f32.neg"            ; 0x8ci32; NoArg()    ; ()                                                           ;
F32Ceil          ; ()                                                                                   ; "f32.ceil"           ; 0x8di32; NoArg()    ; ()                                                           ;
F32Floor         ; ()                                                                                   ; "f32.floor"          ; 0x8ei32; NoArg()    ; ()                                                           ;
F32Trunc         ; ()                                                                                   ; "f32.trunc"          ; 0x8fi32; NoArg()    ; ()                                                           ;
F32Nearest       ; ()                                                                                   ; "f32.nearest"        ; 0x90i32; NoArg()    ; ()                                                           ;
F32Sqrt          ; ()                                                                                   ; "f32.sqrt"           ; 0x91i32; NoArg()    ; ()                                                           ;
F32Add           ; ()                                                                                   ; "f32.add"            ; 0x92i32; NoArg()    ; ()                                                           ;
F32Sub           ; ()                                                                                   ; "f32.sub"            ; 0x93i32; NoArg()    ; ()                                                           ;
F32Mul           ; ()                                                                                   ; "f32.mul"            ; 0x94i32; NoArg()    ; ()                                                           ;
F32Div           ; ()                                                                                   ; "f32.div"            ; 0x95i32; NoArg()    ; ()                                                           ;
F32Min           ; ()                                                                                   ; "f32.min"            ; 0x96i32; NoArg()    ; ()                                                           ;
F32Max           ; ()                                                                                   ; "f32.max"            ; 0x97i32; NoArg()    ; ()                                                           ;
F32Copysign      ; ()                                                                                   ; "f32.copysign"       ; 0x98i32; NoArg()    ; ()                                                           ;
F64Abs           ; ()                                                                                   ; "f64.abs"            ; 0x99i32; NoArg()    ; ()                                                           ;
F64Neg           ; ()                                                                                   ; "f64.neg"            ; 0x9ai32; NoArg()    ; ()                                                           ;
F64Ceil          ; ()                                                                                   ; "f64.ceil"           ; 0x9bi32; NoArg()    ; ()                                                           ;
F64Floor         ; ()                                                                                   ; "f64.floor"          ; 0x9ci32; NoArg()    ; ()                                                           ;
F64Trunc         ; ()                                                                                   ; "f64.trunc"          ; 0x9di32; NoArg()    ; ()                                                           ;
F64Nearest       ; ()                                                                                   ; "f64.nearest"        ; 0x9ei32; NoArg()    ; ()                                                           ;
F64Sqrt          ; ()                                                                                   ; "f64.sqrt"           ; 0x9fi32; NoArg()    ; ()                                                           ;
F64Add           ; ()                                                                                   ; "f64.add"            ; 0xa0i32; NoArg()    ; ()                                                           ;
F64Sub           ; ()                                                                                   ; "f64.sub"            ; 0xa1i32; NoArg()    ; ()                                                           ;
F64Mul           ; ()                                                                                   ; "f64.mul"            ; 0xa2i32; NoArg()    ; ()                                                           ;
F64Div           ; ()                                                                                   ; "f64.div"            ; 0xa3i32; NoArg()    ; ()                                                           ;
F64Min           ; ()                                                                                   ; "f64.min"            ; 0xa4i32; NoArg()    ; ()                                                           ;
F64Max           ; ()                                                                                   ; "f64.max"            ; 0xa5i32; NoArg()    ; ()                                                           ;
F64Copysign      ; ()                                                                                   ; "f64.copysign"       ; 0xa6i32; NoArg()    ; ()                                                           ;
I32WrapI64       ; ()                                                                                   ; "i32.wrap_i64"       ; 0xa7i32; NoArg()    ; ()                                                           ;
I32TruncF32S     ; ()                                                                                   ; "i32.trunc_f32_s"    ; 0xa8i32; NoArg()    ; ()                                                           ;
I32TruncF32U     ; ()                                                                                   ; "i32.trunc_f32_u"    ; 0xa9i32; NoArg()    ; ()                                                           ;
I32TruncF64S     ; ()                                                                                   ; "i32.trunc_f64_s"    ; 0xaai32; NoArg()    ; ()                                                           ;
I32TruncF64U     ; ()                                                                                   ; "i32.trunc_f64_u"    ; 0xabi32; NoArg()    ; ()                                                           ;
I64ExtendI32S    ; ()                                                                                   ; "i64.extend_i32_s"   ; 0xaci32; NoArg()    ; ()                                                           ;
I64ExtendI32U    ; ()                                                                                   ; "i64.extend_i32_u"   ; 0xadi32; NoArg()    ; ()                                                           ;
I64TruncF32S     ; ()                                                                                   ; "i64.trunc_f32_s"    ; 0xaei32; NoArg()    ; ()                                                           ;
I64TruncF32U     ; ()                                                                                   ; "i64.trunc_f32_u"    ; 0xafi32; NoArg()    ; ()                                                           ;
I64TruncF64S     ; ()                                                                                   ; "i64.trunc_f64_s"    ; 0xb0i32; NoArg()    ; ()                                                           ;
I64TruncF64U     ; ()                                                                                   ; "i64.trunc_f64_u"    ; 0xb1i32; NoArg()    ; ()                                                           ;
F32ConvertI32S   ; ()                                                                                   ; "f32.convert_i32_s"  ; 0xb2i32; NoArg()    ; ()                                                           ;
F32ConvertI32U   ; ()                                                                                   ; "f32.convert_i32_u"  ; 0xb3i32; NoArg()    ; ()                                                           ;
F32ConvertI64S   ; ()                                                                                   ; "f32.convert_i64_s"  ; 0xb4i32; NoArg()    ; ()                                                           ;
F32ConvertI64U   ; ()                                                                                   ; "f32.convert_i64_u"  ; 0xb5i32; NoArg()    ; ()                                                           ;
F32DemoteF64     ; ()                                                                                   ; "f32.demote_f64"     ; 0xb6i32; NoArg()    ; ()                                                           ;
F64ConvertI32S   ; ()                                                                                   ; "f64.convert_i32_s"  ; 0xb7i32; NoArg()    ; ()                                                           ;
F64ConvertI32U   ; ()                                                                                   ; "f64.convert_i32_u"  ; 0xb8i32; NoArg()    ; ()                                                           ;
F64ConvertI64S   ; ()                                                                                   ; "f64.convert_i64_s"  ; 0xb9i32; NoArg()    ; ()                                                           ;
F64ConvertI64U   ; ()                                                                                   ; "f64.convert_i64_u"  ; 0xbai32; NoArg()    ; ()                                                           ;
F64PromoteF32    ; ()                                                                                   ; "f64.promote_f32"    ; 0xbbi32; NoArg()    ; ()                                                           ;
I32ReinterpretF32; ()                                                                                   ; "i32.reinterpret_f32"; 0xbci32; NoArg()    ; ()                                                           ;
I64ReinterpretF64; ()                                                                                   ; "i64.reinterpret_f64"; 0xbdi32; NoArg()    ; ()                                                           ;
F32ReinterpretI32; ()                                                                                   ; "f32.reinterpret_i32"; 0xbei32; NoArg()    ; ()                                                           ;
F64ReinterpretI64; ()                                                                                   ; "f64.reinterpret_i64"; 0xbfi32; NoArg()    ; ()                                                           ;
}};
}

macro_rules! def_instructions {
    ($($id:ident; ($($arg_key:ident: $arg_tpe:ty),*); $_text:expr; $_opcode:expr; $_parsing:expr; $_typing:expr;)*) => {
        $(
            #[derive(Clone, Debug, PartialEq)]
            pub struct $id {
                $(pub $arg_key: $arg_tpe),*
            }
        )*
    }
}

macro_rules! def_instructions_enum {
    ($($id:ident; $_arg:expr; $_text:expr; $_opcode:expr; $_parsing:expr; $_typing:expr;)*) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum Instruction {
            $($id($id)),*
        }
    };
}

instruction_defs_cps!(def_instructions());

instruction_defs_cps!(def_instructions_enum());

