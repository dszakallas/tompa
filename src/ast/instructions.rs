#![macro_use]

use super::types::*;

// Note: This, as most macros in this crate are written in continuation passing style
// which enables us to reuse it in other macros. See https://danielkeep.github.io/tlborm/book/pat-callbacks.html
macro_rules! def_instruction_ast_cps { ($cb:ident($($args:tt)*)) => { $cb!{
$($args)*
/*
Keep tabulated with column -t -s ";" -o ";" table > table_organized

Name             ; Parameters                                                                           ;
*/
Unreachable      ; ()                                                                                   ;
Nop              ; ()                                                                                   ;
Block            ; (result: ResultType, instrs: Vec<Instruction>)                                       ;
Loop             ; (result: ResultType, instrs: Vec<Instruction>)                                       ;
If               ; (result: ResultType, if_instrs: Vec<Instruction>, else_instrs: Vec<Instruction>)     ;
Br               ; ()                                                                                   ;
BrIf             ; ()                                                                                   ;
BrTable          ; ()                                                                                   ;
Return           ; ()                                                                                   ;
Call             ; ()                                                                                   ;
CallIndirect     ; ()                                                                                   ;
Drop             ; ()                                                                                   ;
Select           ; ()                                                                                   ;
LocalGet         ; (localidx: LocalIdx)                                                                 ;
LocalSet         ; (localidx: LocalIdx)                                                                 ;
LocalTee         ; (localidx: LocalIdx)                                                                 ;
GlobalGet        ; (globalidx: GlobalIdx)                                                               ;
GlobalSet        ; (globalidx: GlobalIdx)                                                               ;
I32Load          ; (memarg: Memarg)                                                                     ;
I64Load          ; (memarg: Memarg)                                                                     ;
F32Load          ; (memarg: Memarg)                                                                     ;
F64Load          ; (memarg: Memarg)                                                                     ;
I32Load8S        ; (memarg: Memarg)                                                                     ;
I32Load8U        ; (memarg: Memarg)                                                                     ;
I32Load16S       ; (memarg: Memarg)                                                                     ;
I32Load16U       ; (memarg: Memarg)                                                                     ;
I64Load8S        ; (memarg: Memarg)                                                                     ;
I64Load8U        ; (memarg: Memarg)                                                                     ;
I64Load16S       ; (memarg: Memarg)                                                                     ;
I64Load16U       ; (memarg: Memarg)                                                                     ;
I64Load32S       ; (memarg: Memarg)                                                                     ;
I64Load32U       ; (memarg: Memarg)                                                                     ;
I32Store         ; (memarg: Memarg)                                                                     ;
I64Store         ; (memarg: Memarg)                                                                     ;
F32Store         ; (memarg: Memarg)                                                                     ;
F64Store         ; (memarg: Memarg)                                                                     ;
I32Store8        ; (memarg: Memarg)                                                                     ;
I32Store16       ; (memarg: Memarg)                                                                     ;
I64Store8        ; (memarg: Memarg)                                                                     ;
I64Store16       ; (memarg: Memarg)                                                                     ;
I64Store32       ; (memarg: Memarg)                                                                     ;
MemorySize       ; ()                                                                                   ;
MemoryGrow       ; ()                                                                                   ;
I32Const         ; (param: u32)                                                                         ;
I64Const         ; (param: u64)                                                                         ;
F32Const         ; (param: f32)                                                                         ;
F64Const         ; (param: f64)                                                                         ;
I32Eqz           ; ()                                                                                   ;
I32Eq            ; ()                                                                                   ;
I32Ne            ; ()                                                                                   ;
I32LtS           ; ()                                                                                   ;
I32LtU           ; ()                                                                                   ;
I32GtS           ; ()                                                                                   ;
I32GtU           ; ()                                                                                   ;
I32LeS           ; ()                                                                                   ;
I32LeU           ; ()                                                                                   ;
I32GeS           ; ()                                                                                   ;
I32GeU           ; ()                                                                                   ;
I64Eqz           ; ()                                                                                   ;
I64Eq            ; ()                                                                                   ;
I64Ne            ; ()                                                                                   ;
I64LtS           ; ()                                                                                   ;
I64LtU           ; ()                                                                                   ;
I64GtS           ; ()                                                                                   ;
I64GtU           ; ()                                                                                   ;
I64LeS           ; ()                                                                                   ;
I64LeU           ; ()                                                                                   ;
I64GeS           ; ()                                                                                   ;
I64GeU           ; ()                                                                                   ;
F32Eq            ; ()                                                                                   ;
F32Ne            ; ()                                                                                   ;
F32Lt            ; ()                                                                                   ;
F32Gt            ; ()                                                                                   ;
F32Le            ; ()                                                                                   ;
F32Ge            ; ()                                                                                   ;
F64Eq            ; ()                                                                                   ;
F64Ne            ; ()                                                                                   ;
F64Lt            ; ()                                                                                   ;
F64Gt            ; ()                                                                                   ;
F64Le            ; ()                                                                                   ;
F64Ge            ; ()                                                                                   ;
I32Clz           ; ()                                                                                   ;
I32Ctz           ; ()                                                                                   ;
I32Popcnt        ; ()                                                                                   ;
I32Add           ; ()                                                                                   ;
I32Sub           ; ()                                                                                   ;
I32Mul           ; ()                                                                                   ;
I32DivS          ; ()                                                                                   ;
I32DivU          ; ()                                                                                   ;
I32RemS          ; ()                                                                                   ;
I32RemU          ; ()                                                                                   ;
I32And           ; ()                                                                                   ;
I32Or            ; ()                                                                                   ;
I32Xor           ; ()                                                                                   ;
I32Shl           ; ()                                                                                   ;
I32ShrS          ; ()                                                                                   ;
I32ShrU          ; ()                                                                                   ;
I32Rotl          ; ()                                                                                   ;
I32Rotr          ; ()                                                                                   ;
I64Clz           ; ()                                                                                   ;
I64Ctz           ; ()                                                                                   ;
I64Popcnt        ; ()                                                                                   ;
I64Add           ; ()                                                                                   ;
I64Sub           ; ()                                                                                   ;
I64Mul           ; ()                                                                                   ;
I64DivS          ; ()                                                                                   ;
I64DivU          ; ()                                                                                   ;
I64RemS          ; ()                                                                                   ;
I64RemU          ; ()                                                                                   ;
I64And           ; ()                                                                                   ;
I64Or            ; ()                                                                                   ;
I64Xor           ; ()                                                                                   ;
I64Shl           ; ()                                                                                   ;
I64ShrS          ; ()                                                                                   ;
I64ShrU          ; ()                                                                                   ;
I64Rotl          ; ()                                                                                   ;
I64Rotr          ; ()                                                                                   ;
F32Abs           ; ()                                                                                   ;
F32Neg           ; ()                                                                                   ;
F32Ceil          ; ()                                                                                   ;
F32Floor         ; ()                                                                                   ;
F32Trunc         ; ()                                                                                   ;
F32Nearest       ; ()                                                                                   ;
F32Sqrt          ; ()                                                                                   ;
F32Add           ; ()                                                                                   ;
F32Sub           ; ()                                                                                   ;
F32Mul           ; ()                                                                                   ;
F32Div           ; ()                                                                                   ;
F32Min           ; ()                                                                                   ;
F32Max           ; ()                                                                                   ;
F32Copysign      ; ()                                                                                   ;
F64Abs           ; ()                                                                                   ;
F64Neg           ; ()                                                                                   ;
F64Ceil          ; ()                                                                                   ;
F64Floor         ; ()                                                                                   ;
F64Trunc         ; ()                                                                                   ;
F64Nearest       ; ()                                                                                   ;
F64Sqrt          ; ()                                                                                   ;
F64Add           ; ()                                                                                   ;
F64Sub           ; ()                                                                                   ;
F64Mul           ; ()                                                                                   ;
F64Div           ; ()                                                                                   ;
F64Min           ; ()                                                                                   ;
F64Max           ; ()                                                                                   ;
F64Copysign      ; ()                                                                                   ;
I32WrapI64       ; ()                                                                                   ;
I32TruncF32S     ; ()                                                                                   ;
I32TruncF32U     ; ()                                                                                   ;
I32TruncF64S     ; ()                                                                                   ;
I32TruncF64U     ; ()                                                                                   ;
I64ExtendI32S    ; ()                                                                                   ;
I64ExtendI32U    ; ()                                                                                   ;
I64TruncF32S     ; ()                                                                                   ;
I64TruncF32U     ; ()                                                                                   ;
I64TruncF64S     ; ()                                                                                   ;
I64TruncF64U     ; ()                                                                                   ;
F32ConvertI32S   ; ()                                                                                   ;
F32ConvertI32U   ; ()                                                                                   ;
F32ConvertI64S   ; ()                                                                                   ;
F32ConvertI64U   ; ()                                                                                   ;
F32DemoteF64     ; ()                                                                                   ;
F64ConvertI32S   ; ()                                                                                   ;
F64ConvertI32U   ; ()                                                                                   ;
F64ConvertI64S   ; ()                                                                                   ;
F64ConvertI64U   ; ()                                                                                   ;
F64PromoteF32    ; ()                                                                                   ;
I32ReinterpretF32; ()                                                                                   ;
I64ReinterpretF64; ()                                                                                   ;
F32ReinterpretI32; ()                                                                                   ;
F64ReinterpretI64; ()                                                                                   ;
}};
}

macro_rules! def_instructions {
    ($($id:ident; ($($arg_key:ident: $arg_tpe:ty),*);)*) => {
        $(
            #[derive(Clone, Debug, PartialEq)]
            pub struct $id {
                $(pub $arg_key: $arg_tpe),*
            }
        )*

        #[derive(Clone, Debug, PartialEq)]
        pub enum Instruction {
            $($id($id)),*
        }
    }
}

def_instruction_ast_cps!(def_instructions());
