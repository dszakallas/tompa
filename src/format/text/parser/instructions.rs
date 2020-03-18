
use std::ops::{RangeFrom};

use lexical_core::{Integer as LcInteger};

use nom::{AsChar as NomAsChar, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, IResult, Slice};

use nom::bytes::complete::{tag};

use nom::combinator::{map, map_res, not, opt, peek, recognize, value};
use nom::error::{ErrorKind, ParseError};
use nom::lib::std::ops::{Range, RangeTo};
use nom::multi::{fold_many0, many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple, Tuple};

use crate::syntax::instructions::{Block, IfElse, Instr, Loop, Load, Store};
use crate::syntax::types::{ValType, MemArg, Sx};

use crate::format::text::parser::{IdCtx, ParserInput, id, keyword};
use crate::format::text::parser::values::uxx;
use crate::format::text::lexer::LexerInput;

use crate::format::text::lexer::keyword::Keyword::*;

use crate::format::text::parser::types::resulttype;

#[inline]
fn label<'a, 'b, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(ctx: &'b IdCtx) -> impl Fn(I1) -> IResult<I1, IdCtx, E1> + 'b
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    move |input: I1| {
        let _i = input.clone();
        let (i, c) = map_res(opt(id), |id_opt: Option<&I2>| {
            match id_opt {
                Some(id) => {
                    let item = Some(id.as_str().to_owned());
                    if ctx.labels.index_of(&item) != None {
                        Err(()) // TODO pretty error
                    } else {
                        Ok(item)
                    }
                }
                None => Ok(None)
            }
        })(input)?;
        let mut inner_ctx = ctx.clone();
        inner_ctx.labels.push_back(c);
        Ok((i, inner_ctx))
    }
}

#[inline]
fn instr<'a, 'b, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(_ctx: &'b IdCtx) -> impl Fn(I1) -> IResult<I1, Instr, E1>
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    move |i: I1| {
        // TODO implement instr parser
        unimplemented!()
    }
}
//
#[inline]
fn id_checker<'a, 'b, I1: 'a, E1: ParseError<I1> + 'a, I2: 'a>(ctx: &'b IdCtx) -> impl Fn(I1) -> IResult<I1, (), E1> + 'b
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>, {
    move |i: I1| {
        map_res(
            opt(id),
            |id_ch_opt: Option<&'a I2>| {
                if let Some(id_ch) = id_ch_opt {
                    let id_opt = &ctx.labels[ctx.labels.len() - 1];
                    match id_opt {
                        None => Err(()),
                        Some(id) if id.as_str() != id_ch.as_str() => Err(()),
                        _ => Ok(())
                    }
                } else {
                    Ok(())
                }
            }
        )(i)
    }
}

#[inline]
fn block<'a, 'b, I1: 'a, E: ParseError<I1> + 'a, I2: 'a>(ctx: &'b IdCtx) -> impl Fn(I1) -> IResult<I1, Block, E> + 'b
    where
        I1: ParserInput<'a, I2>,
        I2: LexerInput<'a>,
{
    move |i: I1| {
        let (i, _) = keyword(Block)(i)?;
        let (i, inner_ctx) = label(ctx)(i)?;
        let (i, result) = opt(resulttype)(i)?;
        let (i, instrs) = many0(instr(&inner_ctx))(i)?;
        let (i, _) = keyword(End)(i)?;
        let (i, _) = id_checker(&inner_ctx)(i)?;
        Ok((i, Block { result, instrs }))
    }
}

// #[inline]
// fn loop_<'a, I: 'a, E: ParseError<I> + 'a>(ctx: IdCtx) -> impl Fn(I) -> IResult<I, Loop, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     move |i: I| {
//         let (i, _) = token(tag("loop"))(i)?;
//         let (i, inner_ctx) = label(ctx.clone())(i)?;
//         let (i, result) = opt(result)(i)?;
//         let (i, instrs) = many0(instr(inner_ctx.clone()))(i)?;
//         let (i, _) = token(tag("end"))(i)?;
//         let (i, _) = id_checker(&inner_ctx)(i)?;
//         Ok((i, Loop { result, instrs }))
//     }
// }
//
// #[inline]
// fn if_else<'a, I: 'a, E: ParseError<I> + 'a>(ctx: IdCtx) -> impl Fn(I) -> IResult<I, IfElse, E>
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//     move |i: I| {
//         let (i, _) = token(tag("if"))(i)?;
//         let (i, inner_ctx) = label(ctx.clone())(i)?;
//         let (i, result) = opt(result)(i)?;
//         let (i, if_instrs) = many0(instr(inner_ctx.clone()))(i)?;
//         let (i, else_) = opt(token(tag("else")))(i)?;
//         let (i, else_instrs) = if let Some(_) = else_ {
//             let (i, _) = id_checker(&inner_ctx)(i)?;
//             let (i, else_instrs) = many0(instr(inner_ctx.clone()))(i)?;
//             (i, else_instrs)
//         } else {
//             (i, vec![])
//         };
//         let (i, _) = token(tag("end"))(i)?;
//         let (i, _) = id_checker(&inner_ctx)(i)?;
//         Ok((i, IfElse { result, if_instrs, else_instrs }))
//     }
// }

//#[inline]
//fn blockinstr<'a, I: 'a, E: ParseError<I> + 'a>(ctx: IdCtx) -> impl Fn(I) -> IResult<I, BlockInstr, E>
//    where
//        I: Clone
//        + Slice<RangeFrom<usize>>
//        + Slice<Range<usize>>
//        + Slice<RangeTo<usize>>
//        + PartialEq
//        + InputIter
//        + InputTake
//        + InputLength
//        + InputTakeAtPosition
//        + Compare<&'static str>
//        + AsStr<'a>,
//        <I as InputIter>::Item: NomAsChar + AsChar,
//        <I as InputTakeAtPosition>::Item: NomAsChar + AsChar, {
//    alt((
//        map(block(ctx.clone()), |block| BlockInstr::Block(block)),
//        map(loop_(ctx.clone()), |loop_| BlockInstr::Loop(loop_)),
//        map(if_else(ctx), |if_else| BlockInstr::IfElse(if_else)),
//    ))
//}

// fn memarg<'a, I: 'a, E: ParseError<I> + 'a>(n: u32) -> impl Fn(I) -> IResult<I, MemArg, E> + 'a
//     where
//         I: Clone
//         + Slice<RangeFrom<usize>>
//         + Slice<Range<usize>>
//         + Slice<RangeTo<usize>>
//         + PartialEq
//         + InputIter
//         + InputTake
//         + InputLength
//         + InputTakeAtPosition
//         + Compare<&'static str>
//         + AsStr<'a>,
//         <I as InputIter>::Item: NomAsChar + AsChar,
//         <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
// {
//     move |i: I| {
//         let (i, offset) = if let Ok((i, _)) = tag::<&str, I, E>("offset=")(i.clone()) {
//             token(uxx::<I, E, u32>)(i)?
//         } else {
//             (i, 0)
//         };
//
//         let (i, align) = if let Ok((i, _)) = tag::<&str, I, E>("align=")(i.clone()) {
//             let (i, a) = token(uxx::<I, E, u32>)(i.clone())?;
//             if a.count_ones() != 1 {
//                 Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Digit)))?
//             } else {
//                 (i, a.trailing_zeros())
//             }
//         } else {
//             (i, n)
//         };
//
//         Ok((i, MemArg { offset , align }))
//     }
// }
//
// macro_rules! lsint {
//     ($name:ident, $tag:expr, $syntax:tt, $valtype:expr, $storage_size:expr, $memarg:expr) => {
//         #[inline]
//         pub fn $name<'a, I: 'a, E: ParseError<I> + 'a>(_ctx: IdCtx) -> impl Fn(I) -> IResult<I, $syntax, E> + 'a
//             where
//                 I: Clone
//                     + Slice<RangeFrom<usize>>
//                     + Slice<Range<usize>>
//                     + Slice<RangeTo<usize>>
//                     + PartialEq
//                     + InputIter
//                     + InputTake
//                     + InputLength
//                     + InputTakeAtPosition
//                     + Compare<&'static str>
//                     + AsStr<'a>,
//                     <I as InputIter>::Item: NomAsChar + AsChar,
//                     <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
//         {
//            map(preceded(token(tag($tag)), memarg($memarg)), |memarg| { $syntax { valtype: $valtype, storage_size: $storage_size, memarg } })
//         }
//     }
// }
//
// macro_rules! numint {
//     ($name:ident, $tag:expr, $syntax:tt, $valtype:expr, $storage_size:expr, $memarg:expr) => {
//         #[inline]
//         pub fn $name<'a, I: 'a, E: ParseError<I> + 'a>(ctx: IdCtx) -> impl Fn(I) -> IResult<I, $syntax, E> + 'a
//             where
//                 I: Clone
//                     + Slice<RangeFrom<usize>>
//                     + Slice<Range<usize>>
//                     + Slice<RangeTo<usize>>
//                     + PartialEq
//                     + InputIter
//                     + InputTake
//                     + InputLength
//                     + InputTakeAtPosition
//                     + Compare<&'static str>
//                     + AsStr<'a>,
//                     <I as InputIter>::Item: NomAsChar + AsChar,
//                     <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
//         {
//            map(preceded(token(tag($tag)), memarg($memarg)), |memarg| { $syntax { valtype: $valtype, storage_size: $storage_size, memarg } })
//         }
//     }
// }
//
// lsint!(i32load, "i32.load", Load, ValType::I32, None, 4);
// lsint!(i64load, "i64.load", Load, ValType::I64, None, 8);
// lsint!(f32load, "f32.load", Load, ValType::F32, None, 4);
// lsint!(f64load, "f64.load", Load, ValType::F64, None, 8);
// lsint!(i32load8_s, "i32.load8_s", Load, ValType::I32, Some((8, Sx::S)), 1);
// lsint!(i32load8_u, "i32.load8_u", Load, ValType::I32, Some((8, Sx::U)), 1);
// lsint!(i32load16_s, "i32.load16_s", Load, ValType::I32, Some((16, Sx::S)), 2);
// lsint!(i32load16_u, "i32.load16_u", Load, ValType::I32, Some((16, Sx::U)), 2);
// lsint!(i64load8_s, "i64.load8_s", Load, ValType::I64, Some((8, Sx::S)), 1);
// lsint!(i64load8_u, "i64.load8_u", Load, ValType::I64, Some((8, Sx::U)), 1);
// lsint!(i64load16_s, "i64.load16_s", Load, ValType::I64, Some((16, Sx::S)), 2);
// lsint!(i64load16_u, "i64.load16_u", Load, ValType::I64, Some((16, Sx::U)), 2);
// lsint!(i64load32_s, "i64.load32_s", Load, ValType::I64, Some((32, Sx::S)), 4);
// lsint!(i64load32_u, "i64.load32_u", Load, ValType::I64, Some((32, Sx::U)), 4);
// lsint!(i32store, "i32.store", Store, ValType::I32, None, 4);
// lsint!(i64store, "i64.store", Store, ValType::I64, None, 8);
// lsint!(f32store, "f32.store", Store, ValType::F32, None, 4);
// lsint!(f64store, "f64.store", Store, ValType::F64, None, 8);
// lsint!(i32store8, "i32.store8", Store, ValType::I32, Some(8), 1);
// lsint!(i32store16, "i32.store16", Store, ValType::I32, Some(16), 2);
// lsint!(i64store8, "i64.store8", Store, ValType::I64, Some(8), 1);
// lsint!(i64store16, "i64.store16", Store, ValType::I64, Some(16), 2);
// lsint!(i64store32, "i64.store32", Store, ValType::I64, Some(32), 4);
//
// // TODO memory.grow memory.size
//
//
//
// //numint!(i32load, I32Load, "i32.load", 4);
// //numint!(jajj2, I32Load, "i32.load", 4);
//
//
//
// //fn meminstr<'a, I: 'a, E: ParseError<I> + 'a>(ctx: IdCtx) -> impl Fn(I) -> IResult<I, BlockInstr, E>
// //    where
// //        I: Clone
// //        + Slice<RangeFrom<usize>>
// //        + Slice<Range<usize>>
// //        + Slice<RangeTo<usize>>
// //        + PartialEq
// //        + InputIter
// //        + InputTake
// //        + InputLength
// //        + InputTakeAtPosition
// //        + Compare<&'static str>
// //        + AsStr<'a>,
// //        <I as InputIter>::Item: NomAsChar + AsChar,
// //        <I as InputTakeAtPosition>::Item: NomAsChar + AsChar,
// //{
// //    (tag("i32.load"), memarg(4))
// //}
//
// //instr!("i32.load", I32Load, memarg(4));
//
// #[cfg(test)]
// mod test {
//
//     use nom::error::{ErrorKind};
//
//
//     use super::*;
//
//     type FastError<T> = (T, ErrorKind);
//
//     #[test]
//     fn test_label() {
//         {
//             let (i, inner_ctx) = label::<&str, FastError<&str>>(Default::default())("$id").unwrap();
//             assert_eq!(i, "");
//             assert_eq!(inner_ctx.labels, im_rc::vector![Some("id".to_owned())])
//         }
//     }
//
//     #[test]
//     fn test_block() {
//         assert_eq!(
//             block::<'static, &str, FastError<&str>>(Default::default())("block end"),
//             Ok(("", Block { result: None, instrs: vec![] }))
//         );
//         assert_eq!(
//             block::<'static, &str, FastError<&str>>(Default::default())("block $my_block end"),
//             Ok(("", Block { result: None, instrs: vec![] }))
//         );
//
//         {
//             let mut id_ctx: IdCtx = Default::default();
//             id_ctx.labels.push_back(Some("my_block".to_owned()));
//             block::<'static, &str, FastError<&str>>(id_ctx)("block $my_block end").unwrap_err();
//         }
//
//         assert_eq!(
//             block::<'static, &str, FastError<&str>>(Default::default())("block $my_block end $my_block"),
//             Ok(("", Block { result: None, instrs: vec![] }))
//         );
//
//         block::<'static, &str, FastError<&str>>(Default::default())("block $my_block end $wrong_block").unwrap_err();
//
//         assert_eq!(
//             block::<'static, &str, FastError<&str>>(Default::default())("block (result f64) end"),
//             Ok(("", Block { result: Some(ValType::F64), instrs: vec![] }))
//         );
//     }
//
//     #[test]
//     fn test_loop() {
//         assert_eq!(
//             loop_::<'static, &str, FastError<&str>>(Default::default())("loop end"),
//             Ok(("", Loop { result: None, instrs: vec![] }))
//         );
//         assert_eq!(
//             loop_::<'static, &str, FastError<&str>>(Default::default())("loop $my_loop_ end"),
//             Ok(("", Loop { result: None, instrs: vec![] }))
//         );
//
//         {
//             let mut id_ctx: IdCtx = Default::default();
//             id_ctx.labels.push_back(Some("my_loop".to_owned()));
//             loop_::<'static, &str, FastError<&str>>(id_ctx)("loop $my_loop end").unwrap_err();
//         }
//
//         assert_eq!(
//             loop_::<'static, &str, FastError<&str>>(Default::default())("loop $my_loop end $my_loop"),
//             Ok(("", Loop { result: None, instrs: vec![] }))
//         );
//
//         loop_::<'static, &str, FastError<&str>>(Default::default())("loop $my_loop end $wrong_loop").unwrap_err();
//
//         assert_eq!(
//             loop_::<'static, &str, FastError<&str>>(Default::default())("loop (result f64) end"),
//             Ok(("", Loop { result: Some(ValType::F64), instrs: vec![] }))
//         );
//     }
//
//     #[test]
//     fn test_if_else() {
//         assert_eq!(
//             if_else::<'static, &str, FastError<&str>>(Default::default())("if end"),
//             Ok(("", IfElse { result: None, if_instrs: vec![], else_instrs: vec![] }))
//         );
//
//         assert_eq!(
//             if_else::<'static, &str, FastError<&str>>(Default::default())("if else end"),
//             Ok(("", IfElse { result: None, if_instrs: vec![], else_instrs: vec![] }))
//         );
//
//         assert_eq!(
//             if_else::<'static, &str, FastError<&str>>(Default::default())("if $my_if else end"),
//             Ok(("", IfElse { result: None, if_instrs: vec![], else_instrs: vec![] }))
//         );
//
//         assert_eq!(
//             if_else::<'static, &str, FastError<&str>>(Default::default())("if $my_if else $my_if end"),
//             Ok(("", IfElse { result: None, if_instrs: vec![], else_instrs: vec![] }))
//         );
//
//         assert_eq!(
//             if_else::<'static, &str, FastError<&str>>(Default::default())("if $my_if else $my_if end $my_if"),
//             Ok(("", IfElse { result: None, if_instrs: vec![], else_instrs: vec![] }))
//         );
//
//         if_else::<'static, &str, FastError<&str>>(Default::default())("if $my_if else $wrong_if end").unwrap_err();
//
//         if_else::<'static, &str, FastError<&str>>(Default::default())("if $my_if else end $wrong_if").unwrap_err();
//     }
//
//     #[test]
//     fn test_mem_arg() {
//         assert_eq!(
//             memarg::<'static, &str, FastError<&str>>(1)(""),
//             Ok(("", MemArg { offset: 0, align: 1 }))
//         );
//
//         assert_eq!(
//             memarg::<'static, &str, FastError<&str>>(2)("offset=4"),
//             Ok(("", MemArg { offset: 4, align: 2 }))
//         );
//
//         assert_eq!(
//             memarg::<'static, &str, FastError<&str>>(2)("align=16"),
//             Ok(("", MemArg { offset: 0, align: 4 }))
//         );
//
//         assert_eq!(
//             memarg::<'static, &str, FastError<&str>>(2)("offset=8 align=64"),
//             Ok(("", MemArg { offset: 8, align: 6 }))
//         );
//
//         memarg::<'static, &str, FastError<&str>>(2)("offset=-8").unwrap_err();
//         memarg::<'static, &str, FastError<&str>>(2)("align=111115235").unwrap_err();
//     }
// }
