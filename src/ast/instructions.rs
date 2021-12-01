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
Unreachable      ; ()                                                                                   ; "unreachable"        ; 0x00u8; NoArg()    ; UnreachableType { }                                          ;
Nop              ; ()                                                                                   ; "nop"                ; 0x01u8; NoArg()    ; NopType { }                                                  ;
Block            ; (result: ResultType, instrs: Vec<Instruction>)                                  ; "block"              ; 0x02u8; Block()    ; BlockType {}                                                 ;
Loop             ; (result: ResultType, instrs: Vec<Instruction>)                                  ; "loop"               ; 0x03u8; Loop()     ; LoopType {}                                                  ;
If               ; (result: ResultType, if_instrs: Vec<Instruction>, else_instrs: Vec<Instruction>); "if"                 ; 0x04u8; If()       ; IfType {}                                                    ;
Br               ; ()                                                                                   ; "br"                 ; 0x0cu8; NoArg()    ; BrType {}                                                    ;
BrIf             ; ()                                                                                   ; "br_if"              ; 0x0du8; NoArg()    ; BrIfType {}                                                  ;
BrTable          ; ()                                                                                   ; "br_table"           ; 0x0eu8; NoArg()    ; BrTableType {}                                               ;
Return           ; ()                                                                                   ; "return"             ; 0x0fu8; NoArg()    ; ReturnType{}                                                 ;
Call             ; ()                                                                                   ; "call"               ; 0x10u8; NoArg()    ; CallType{}                                                   ;
CallIndirect     ; ()                                                                                   ; "call_indirect"      ; 0x11u8; NoArg()    ; CallIndirectType{}                                           ;
Drop             ; ()                                                                                   ; "drop"               ; 0x1au8; NoArg()    ; ()                                                           ;
Select           ; ()                                                                                   ; "select"             ; 0x1bu8; NoArg()    ; ()                                                           ;
LocalGet         ; (localidx: LocalIdx)                                                                 ; "local.get"          ; 0x20u8; LocalIdx() ; ()                                                           ;
LocalSet         ; (localidx: LocalIdx)                                                                 ; "local.set"          ; 0x21u8; LocalIdx() ; ()                                                           ;
LocalTee         ; (localidx: LocalIdx)                                                                 ; "local.tee"          ; 0x22u8; LocalIdx() ; ()                                                           ;
GlobalGet        ; (globalidx: GlobalIdx)                                                               ; "global.get"         ; 0x23u8; GlobalIdx(); ()                                                           ;
GlobalSet        ; (globalidx: GlobalIdx)                                                               ; "global.set"         ; 0x24u8; GlobalIdx(); ()                                                           ;
I32Load          ; (memarg: Memarg)                                                                     ; "i32.load"           ; 0x28u8; MemLs(4)   ; LoadType { valtype: ValType::I32, storage: None }            ;
I64Load          ; (memarg: Memarg)                                                                     ; "i64.load"           ; 0x29u8; MemLs(8)   ; LoadType { valtype: ValType::I64, storage: None }            ;
F32Load          ; (memarg: Memarg)                                                                     ; "f32.load"           ; 0x2au8; MemLs(4)   ; LoadType { valtype: ValType::F32, storage: None }            ;
F64Load          ; (memarg: Memarg)                                                                     ; "f64.load"           ; 0x2bu8; MemLs(8)   ; LoadType { valtype: ValType::F64, storage: None }            ;
I32Load8S        ; (memarg: Memarg)                                                                     ; "i32.load8_s"        ; 0x2cu8; MemLs(1)   ; LoadType { valtype: ValType::I32, storage: Some((1, Sx::S)) };
I32Load8U        ; (memarg: Memarg)                                                                     ; "i32.load8_u"        ; 0x2du8; MemLs(1)   ; LoadType { valtype: ValType::I32, storage: Some((1, Sx::U)) };
I32Load16S       ; (memarg: Memarg)                                                                     ; "i32.load16_s"       ; 0x2eu8; MemLs(2)   ; LoadType { valtype: ValType::I32, storage: Some((2, Sx::S)) };
I32Load16U       ; (memarg: Memarg)                                                                     ; "i32.load16_u"       ; 0x2fu8; MemLs(2)   ; LoadType { valtype: ValType::I32, storage: Some((2, Sx::U)) };
I64Load8S        ; (memarg: Memarg)                                                                     ; "i64.load8_s"        ; 0x30u8; MemLs(1)   ; LoadType { valtype: ValType::I64, storage: Some((1, Sx::S)) };
I64Load8U        ; (memarg: Memarg)                                                                     ; "i64.load8_u"        ; 0x31u8; MemLs(1)   ; LoadType { valtype: ValType::I64, storage: Some((1, Sx::U)) };
I64Load16S       ; (memarg: Memarg)                                                                     ; "i64.load16_s"       ; 0x32u8; MemLs(2)   ; LoadType { valtype: ValType::I64, storage: Some((2, Sx::S)) };
I64Load16U       ; (memarg: Memarg)                                                                     ; "i64.load16_u"       ; 0x33u8; MemLs(2)   ; LoadType { valtype: ValType::I64, storage: Some((3, Sx::U)) };
I64Load32S       ; (memarg: Memarg)                                                                     ; "i64.load32_s"       ; 0x34u8; MemLs(4)   ; LoadType { valtype: ValType::I64, storage: Some((4, Sx::S)) };
I64Load32U       ; (memarg: Memarg)                                                                     ; "i64.load32_u"       ; 0x35u8; MemLs(4)   ; LoadType { valtype: ValType::I64, storage: Some((4, Sx::U)) };
I32Store         ; (memarg: Memarg)                                                                     ; "i32.store"          ; 0x36u8; MemLs(4)   ; StoreType { valtype: ValType::I32, storage: None }           ;
I64Store         ; (memarg: Memarg)                                                                     ; "i64.store"          ; 0x37u8; MemLs(8)   ; StoreType { valtype: ValType::I64, storage: None }           ;
F32Store         ; (memarg: Memarg)                                                                     ; "f32.store"          ; 0x38u8; MemLs(4)   ; StoreType { valtype: ValType::F32, storage: None }           ;
F64Store         ; (memarg: Memarg)                                                                     ; "f64.store"          ; 0x39u8; MemLs(8)   ; StoreType { valtype: ValType::F64, storage: None }           ;
I32Store8        ; (memarg: Memarg)                                                                     ; "i32.store8"         ; 0x3au8; MemLs(1)   ; StoreType { valtype: ValType::I32, storage: Some(1) }        ;
I32Store16       ; (memarg: Memarg)                                                                     ; "i32.store16"        ; 0x3bu8; MemLs(2)   ; StoreType { valtype: ValType::I32, storage: Some(2) }        ;
I64Store8        ; (memarg: Memarg)                                                                     ; "i64.store8"         ; 0x3cu8; MemLs(1)   ; StoreType { valtype: ValType::I64, storage: Some(1) }        ;
I64Store16       ; (memarg: Memarg)                                                                     ; "i64.store16"        ; 0x3du8; MemLs(2)   ; StoreType { valtype: ValType::I64, storage: Some(2) }        ;
I64Store32       ; (memarg: Memarg)                                                                     ; "i64.store32"        ; 0x3eu8; MemLs(4)   ; StoreType { valtype: ValType::I64, storage: Some(4) }        ;
MemorySize       ; ()                                                                                   ; "memory.size"        ; 0x3fu8; NoArg()    ; ()                                                           ;
MemoryGrow       ; ()                                                                                   ; "memory.grow"        ; 0x40u8; NoArg()    ; ()                                                           ;
I32Const         ; (param: u32)                                                                         ; "i32.const"          ; 0x41u8; ConstI32() ; ()                                                           ;
I64Const         ; (param: u64)                                                                         ; "i64.const"          ; 0x42u8; ConstI64() ; ()                                                           ;
F32Const         ; (param: f32)                                                                         ; "f32.const"          ; 0x43u8; ConstF32() ; ()                                                           ;
F64Const         ; (param: f64)                                                                         ; "f64.const"          ; 0x44u8; ConstF64() ; ()                                                           ;
I32Eqz           ; ()                                                                                   ; "i32.eqz"            ; 0x45u8; NoArg()    ; ()                                                           ;
I32Eq            ; ()                                                                                   ; "i32.eq"             ; 0x46u8; NoArg()    ; ()                                                           ;
I32Ne            ; ()                                                                                   ; "i32.ne"             ; 0x47u8; NoArg()    ; ()                                                           ;
I32LtS           ; ()                                                                                   ; "i32.lt_s"           ; 0x48u8; NoArg()    ; ()                                                           ;
I32LtU           ; ()                                                                                   ; "i32.lt_u"           ; 0x49u8; NoArg()    ; ()                                                           ;
I32GtS           ; ()                                                                                   ; "i32.gt_s"           ; 0x4au8; NoArg()    ; ()                                                           ;
I32GtU           ; ()                                                                                   ; "i32.gt_u"           ; 0x4bu8; NoArg()    ; ()                                                           ;
I32LeS           ; ()                                                                                   ; "i32.le_s"           ; 0x4cu8; NoArg()    ; ()                                                           ;
I32LeU           ; ()                                                                                   ; "i32.le_u"           ; 0x4du8; NoArg()    ; ()                                                           ;
I32GeS           ; ()                                                                                   ; "i32.ge_s"           ; 0x4eu8; NoArg()    ; ()                                                           ;
I32GeU           ; ()                                                                                   ; "i32.ge_u"           ; 0x4fu8; NoArg()    ; ()                                                           ;
I64Eqz           ; ()                                                                                   ; "i64.eqz"            ; 0x50u8; NoArg()    ; ()                                                           ;
I64Eq            ; ()                                                                                   ; "i64.eq"             ; 0x51u8; NoArg()    ; ()                                                           ;
I64Ne            ; ()                                                                                   ; "i64.ne"             ; 0x52u8; NoArg()    ; ()                                                           ;
I64LtS           ; ()                                                                                   ; "i64.lt_s"           ; 0x53u8; NoArg()    ; ()                                                           ;
I64LtU           ; ()                                                                                   ; "i64.lt_u"           ; 0x54u8; NoArg()    ; ()                                                           ;
I64GtS           ; ()                                                                                   ; "i64.gt_s"           ; 0x55u8; NoArg()    ; ()                                                           ;
I64GtU           ; ()                                                                                   ; "i64.gt_u"           ; 0x56u8; NoArg()    ; ()                                                           ;
I64LeS           ; ()                                                                                   ; "i64.le_s"           ; 0x57u8; NoArg()    ; ()                                                           ;
I64LeU           ; ()                                                                                   ; "i64.le_u"           ; 0x58u8; NoArg()    ; ()                                                           ;
I64GeS           ; ()                                                                                   ; "i64.ge_s"           ; 0x59u8; NoArg()    ; ()                                                           ;
I64GeU           ; ()                                                                                   ; "i64.ge_u"           ; 0x5au8; NoArg()    ; ()                                                           ;
F32Eq            ; ()                                                                                   ; "f32.eq"             ; 0x5bu8; NoArg()    ; ()                                                           ;
F32Ne            ; ()                                                                                   ; "f32.ne"             ; 0x5cu8; NoArg()    ; ()                                                           ;
F32Lt            ; ()                                                                                   ; "f32.lt"             ; 0x5du8; NoArg()    ; ()                                                           ;
F32Gt            ; ()                                                                                   ; "f32.gt"             ; 0x5eu8; NoArg()    ; ()                                                           ;
F32Le            ; ()                                                                                   ; "f32.le"             ; 0x5fu8; NoArg()    ; ()                                                           ;
F32Ge            ; ()                                                                                   ; "f32.ge"             ; 0x60u8; NoArg()    ; ()                                                           ;
F64Eq            ; ()                                                                                   ; "f64.eq"             ; 0x61u8; NoArg()    ; ()                                                           ;
F64Ne            ; ()                                                                                   ; "f64.ne"             ; 0x62u8; NoArg()    ; ()                                                           ;
F64Lt            ; ()                                                                                   ; "f64.lt"             ; 0x63u8; NoArg()    ; ()                                                           ;
F64Gt            ; ()                                                                                   ; "f64.gt"             ; 0x64u8; NoArg()    ; ()                                                           ;
F64Le            ; ()                                                                                   ; "f64.le"             ; 0x65u8; NoArg()    ; ()                                                           ;
F64Ge            ; ()                                                                                   ; "f64.ge"             ; 0x66u8; NoArg()    ; ()                                                           ;
I32Clz           ; ()                                                                                   ; "i32.clz"            ; 0x67u8; NoArg()    ; ()                                                           ;
I32Ctz           ; ()                                                                                   ; "i32.ctz"            ; 0x68u8; NoArg()    ; ()                                                           ;
I32Popcnt        ; ()                                                                                   ; "i32.popcnt"         ; 0x69u8; NoArg()    ; ()                                                           ;
I32Add           ; ()                                                                                   ; "i32.add"            ; 0x6au8; NoArg()    ; ()                                                           ;
I32Sub           ; ()                                                                                   ; "i32.sub"            ; 0x6bu8; NoArg()    ; ()                                                           ;
I32Mul           ; ()                                                                                   ; "i32.mul"            ; 0x6cu8; NoArg()    ; ()                                                           ;
I32DivS          ; ()                                                                                   ; "i32.div_s"          ; 0x6du8; NoArg()    ; ()                                                           ;
I32DivU          ; ()                                                                                   ; "i32.div_u"          ; 0x6eu8; NoArg()    ; ()                                                           ;
I32RemS          ; ()                                                                                   ; "i32.rem_s"          ; 0x6fu8; NoArg()    ; ()                                                           ;
I32RemU          ; ()                                                                                   ; "i32.rem_u"          ; 0x70u8; NoArg()    ; ()                                                           ;
I32And           ; ()                                                                                   ; "i32.and"            ; 0x71u8; NoArg()    ; ()                                                           ;
I32Or            ; ()                                                                                   ; "i32.or"             ; 0x72u8; NoArg()    ; ()                                                           ;
I32Xor           ; ()                                                                                   ; "i32.xor"            ; 0x73u8; NoArg()    ; ()                                                           ;
I32Shl           ; ()                                                                                   ; "i32.shl"            ; 0x74u8; NoArg()    ; ()                                                           ;
I32ShrS          ; ()                                                                                   ; "i32.shr_s"          ; 0x75u8; NoArg()    ; ()                                                           ;
I32ShrU          ; ()                                                                                   ; "i32.shr_u"          ; 0x76u8; NoArg()    ; ()                                                           ;
I32Rotl          ; ()                                                                                   ; "i32.rotl"           ; 0x77u8; NoArg()    ; ()                                                           ;
I32Rotr          ; ()                                                                                   ; "i32.rotr"           ; 0x78u8; NoArg()    ; ()                                                           ;
I64Clz           ; ()                                                                                   ; "i64.clz"            ; 0x79u8; NoArg()    ; ()                                                           ;
I64Ctz           ; ()                                                                                   ; "i64.ctz"            ; 0x7au8; NoArg()    ; ()                                                           ;
I64Popcnt        ; ()                                                                                   ; "i64.popcnt"         ; 0x7bu8; NoArg()    ; ()                                                           ;
I64Add           ; ()                                                                                   ; "i64.add"            ; 0x7cu8; NoArg()    ; ()                                                           ;
I64Sub           ; ()                                                                                   ; "i64.sub"            ; 0x7du8; NoArg()    ; ()                                                           ;
I64Mul           ; ()                                                                                   ; "i64.mul"            ; 0x7eu8; NoArg()    ; ()                                                           ;
I64DivS          ; ()                                                                                   ; "i64.div_s"          ; 0x7fu8; NoArg()    ; ()                                                           ;
I64DivU          ; ()                                                                                   ; "i64.div_u"          ; 0x80u8; NoArg()    ; ()                                                           ;
I64RemS          ; ()                                                                                   ; "i64.rem_s"          ; 0x81u8; NoArg()    ; ()                                                           ;
I64RemU          ; ()                                                                                   ; "i64.rem_u"          ; 0x82u8; NoArg()    ; ()                                                           ;
I64And           ; ()                                                                                   ; "i64.and"            ; 0x83u8; NoArg()    ; ()                                                           ;
I64Or            ; ()                                                                                   ; "i64.or"             ; 0x84u8; NoArg()    ; ()                                                           ;
I64Xor           ; ()                                                                                   ; "i64.xor"            ; 0x85u8; NoArg()    ; ()                                                           ;
I64Shl           ; ()                                                                                   ; "i64.shl"            ; 0x86u8; NoArg()    ; ()                                                           ;
I64ShrS          ; ()                                                                                   ; "i64.shr_s"          ; 0x87u8; NoArg()    ; ()                                                           ;
I64ShrU          ; ()                                                                                   ; "i64.shr_u"          ; 0x88u8; NoArg()    ; ()                                                           ;
I64Rotl          ; ()                                                                                   ; "i64.rotl"           ; 0x89u8; NoArg()    ; ()                                                           ;
I64Rotr          ; ()                                                                                   ; "i64.rotr"           ; 0x8au8; NoArg()    ; ()                                                           ;
F32Abs           ; ()                                                                                   ; "f32.abs"            ; 0x8bu8; NoArg()    ; ()                                                           ;
F32Neg           ; ()                                                                                   ; "f32.neg"            ; 0x8cu8; NoArg()    ; ()                                                           ;
F32Ceil          ; ()                                                                                   ; "f32.ceil"           ; 0x8du8; NoArg()    ; ()                                                           ;
F32Floor         ; ()                                                                                   ; "f32.floor"          ; 0x8eu8; NoArg()    ; ()                                                           ;
F32Trunc         ; ()                                                                                   ; "f32.trunc"          ; 0x8fu8; NoArg()    ; ()                                                           ;
F32Nearest       ; ()                                                                                   ; "f32.nearest"        ; 0x90u8; NoArg()    ; ()                                                           ;
F32Sqrt          ; ()                                                                                   ; "f32.sqrt"           ; 0x91u8; NoArg()    ; ()                                                           ;
F32Add           ; ()                                                                                   ; "f32.add"            ; 0x92u8; NoArg()    ; ()                                                           ;
F32Sub           ; ()                                                                                   ; "f32.sub"            ; 0x93u8; NoArg()    ; ()                                                           ;
F32Mul           ; ()                                                                                   ; "f32.mul"            ; 0x94u8; NoArg()    ; ()                                                           ;
F32Div           ; ()                                                                                   ; "f32.div"            ; 0x95u8; NoArg()    ; ()                                                           ;
F32Min           ; ()                                                                                   ; "f32.min"            ; 0x96u8; NoArg()    ; ()                                                           ;
F32Max           ; ()                                                                                   ; "f32.max"            ; 0x97u8; NoArg()    ; ()                                                           ;
F32Copysign      ; ()                                                                                   ; "f32.copysign"       ; 0x98u8; NoArg()    ; ()                                                           ;
F64Abs           ; ()                                                                                   ; "f64.abs"            ; 0x99u8; NoArg()    ; ()                                                           ;
F64Neg           ; ()                                                                                   ; "f64.neg"            ; 0x9au8; NoArg()    ; ()                                                           ;
F64Ceil          ; ()                                                                                   ; "f64.ceil"           ; 0x9bu8; NoArg()    ; ()                                                           ;
F64Floor         ; ()                                                                                   ; "f64.floor"          ; 0x9cu8; NoArg()    ; ()                                                           ;
F64Trunc         ; ()                                                                                   ; "f64.trunc"          ; 0x9du8; NoArg()    ; ()                                                           ;
F64Nearest       ; ()                                                                                   ; "f64.nearest"        ; 0x9eu8; NoArg()    ; ()                                                           ;
F64Sqrt          ; ()                                                                                   ; "f64.sqrt"           ; 0x9fu8; NoArg()    ; ()                                                           ;
F64Add           ; ()                                                                                   ; "f64.add"            ; 0xa0u8; NoArg()    ; ()                                                           ;
F64Sub           ; ()                                                                                   ; "f64.sub"            ; 0xa1u8; NoArg()    ; ()                                                           ;
F64Mul           ; ()                                                                                   ; "f64.mul"            ; 0xa2u8; NoArg()    ; ()                                                           ;
F64Div           ; ()                                                                                   ; "f64.div"            ; 0xa3u8; NoArg()    ; ()                                                           ;
F64Min           ; ()                                                                                   ; "f64.min"            ; 0xa4u8; NoArg()    ; ()                                                           ;
F64Max           ; ()                                                                                   ; "f64.max"            ; 0xa5u8; NoArg()    ; ()                                                           ;
F64Copysign      ; ()                                                                                   ; "f64.copysign"       ; 0xa6u8; NoArg()    ; ()                                                           ;
I32WrapI64       ; ()                                                                                   ; "i32.wrap_i64"       ; 0xa7u8; NoArg()    ; ()                                                           ;
I32TruncF32S     ; ()                                                                                   ; "i32.trunc_f32_s"    ; 0xa8u8; NoArg()    ; ()                                                           ;
I32TruncF32U     ; ()                                                                                   ; "i32.trunc_f32_u"    ; 0xa9u8; NoArg()    ; ()                                                           ;
I32TruncF64S     ; ()                                                                                   ; "i32.trunc_f64_s"    ; 0xaau8; NoArg()    ; ()                                                           ;
I32TruncF64U     ; ()                                                                                   ; "i32.trunc_f64_u"    ; 0xabu8; NoArg()    ; ()                                                           ;
I64ExtendI32S    ; ()                                                                                   ; "i64.extend_i32_s"   ; 0xacu8; NoArg()    ; ()                                                           ;
I64ExtendI32U    ; ()                                                                                   ; "i64.extend_i32_u"   ; 0xadu8; NoArg()    ; ()                                                           ;
I64TruncF32S     ; ()                                                                                   ; "i64.trunc_f32_s"    ; 0xaeu8; NoArg()    ; ()                                                           ;
I64TruncF32U     ; ()                                                                                   ; "i64.trunc_f32_u"    ; 0xafu8; NoArg()    ; ()                                                           ;
I64TruncF64S     ; ()                                                                                   ; "i64.trunc_f64_s"    ; 0xb0u8; NoArg()    ; ()                                                           ;
I64TruncF64U     ; ()                                                                                   ; "i64.trunc_f64_u"    ; 0xb1u8; NoArg()    ; ()                                                           ;
F32ConvertI32S   ; ()                                                                                   ; "f32.convert_i32_s"  ; 0xb2u8; NoArg()    ; ()                                                           ;
F32ConvertI32U   ; ()                                                                                   ; "f32.convert_i32_u"  ; 0xb3u8; NoArg()    ; ()                                                           ;
F32ConvertI64S   ; ()                                                                                   ; "f32.convert_i64_s"  ; 0xb4u8; NoArg()    ; ()                                                           ;
F32ConvertI64U   ; ()                                                                                   ; "f32.convert_i64_u"  ; 0xb5u8; NoArg()    ; ()                                                           ;
F32DemoteF64     ; ()                                                                                   ; "f32.demote_f64"     ; 0xb6u8; NoArg()    ; ()                                                           ;
F64ConvertI32S   ; ()                                                                                   ; "f64.convert_i32_s"  ; 0xb7u8; NoArg()    ; ()                                                           ;
F64ConvertI32U   ; ()                                                                                   ; "f64.convert_i32_u"  ; 0xb8u8; NoArg()    ; ()                                                           ;
F64ConvertI64S   ; ()                                                                                   ; "f64.convert_i64_s"  ; 0xb9u8; NoArg()    ; ()                                                           ;
F64ConvertI64U   ; ()                                                                                   ; "f64.convert_i64_u"  ; 0xbau8; NoArg()    ; ()                                                           ;
F64PromoteF32    ; ()                                                                                   ; "f64.promote_f32"    ; 0xbbu8; NoArg()    ; ()                                                           ;
I32ReinterpretF32; ()                                                                                   ; "i32.reinterpret_f32"; 0xbcu8; NoArg()    ; ()                                                           ;
I64ReinterpretF64; ()                                                                                   ; "i64.reinterpret_f64"; 0xbdu8; NoArg()    ; ()                                                           ;
F32ReinterpretI32; ()                                                                                   ; "f32.reinterpret_i32"; 0xbeu8; NoArg()    ; ()                                                           ;
F64ReinterpretI64; ()                                                                                   ; "f64.reinterpret_i64"; 0xbfu8; NoArg()    ; ()                                                           ;
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

