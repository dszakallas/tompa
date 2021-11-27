
// #[inline]
// pub fn valtype<'a, I: ParserInput<'a> + 'a>(i: I) -> IResult<I, ValType, I::Error>
// where I::Inner: LexerInput<'a>
// {
//     alt((
//         value(ValType::I32, keyword(I32)),
//         value(ValType::I64, keyword(I64)),
//         value(ValType::F32, keyword(F32)),
//         value(ValType::F64, keyword(F64))
//     ))(i)
// }
