
macro_rules! def_instruction_parser_helpers {
    ($($id:ident; ($($pname:ident: $pty:ty),*); $_text:expr; $opcode:expr; $parse_tpe:ident($($arg:expr),*); $_rule:expr;)*) => {
        static INSTR_PARSERS: phf::Map<i32, InstrParseType> = phf_map! {
            $(
                $opcode => InstrParseType::$parse_tpe($($arg,)* |$($pname: $pty),*| crate::ast::Instruction::$id(crate::ast::$id { $($pname),* }))
            ),*
        };
    }
}

instruction_defs_cps!(def_instruction_parser_helpers());
