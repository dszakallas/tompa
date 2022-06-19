use nom::{AsChar as NomAsChar, IResult, Slice};

use nom::combinator::{map, map_res, not, opt, value};
use nom::error::{ParseError, ErrorKind};

use nom::multi::{many0, many1};

use crate::ast::*;

use crate::format::text::instructions::{INSTR_PARSERS, InstrParseType};

use crate::format::text::parser::{id, tag, WithWrappedStream, anykeyword, par, parc};
use crate::format::text::parser::TokenStream;
use crate::format::text::parser::IdCtx;

use crate::format::text::lexer::CharStream;

use crate::format::text::keywords::Keyword;

use crate::format::text::parser::types::{resulttype, params, results};
use crate::format::text::parser::values::{ixx, fxx};

use crate::format::text::parser::lexical::parsed_uxx;
use nom::sequence::{pair, preceded, tuple};

use crate::format::text::lexer::AsStr;

use crate::format::text::parser::ParserError;

use std::result::Result as StdResult;
use std::collections::HashSet;
use nom::branch::alt;
use crate::format::text::parser::values::uxx;


use crate::ast::{FuncIdx, GlobalIdx, Instruction, LabelIdx, LocalIdx, Memarg, ResultType, TypeIdx};

#[inline]
fn block<'a, 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, Block, I::Error> + 'b
    where
        I::Inner: CharStream<'a>,
{
    move |i: I| {
        let (i, inner_ctx) = label(ctx)(i)?;
        let (i, result) = resulttype(i)?;
        let (i, instrs) = many0(instr(&inner_ctx))(i)?;
        let (i, _) = tag(Keyword::End)(i)?;
        let (i, _) = id_checker(&inner_ctx)(i)?;
        Ok((i, Block { result, instrs }))
    }
}

#[inline]
fn loop_<'a, 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, Loop, I::Error> + 'b
    where
        I::Inner: CharStream<'a>,
{
    move |i: I| {
        let (i, inner_ctx) = label(ctx)(i)?;
        let (i, result) = resulttype(i)?;
        let (i, instrs) = many0(instr(&inner_ctx))(i)?;
        let (i, _) = tag(Keyword::End)(i)?;
        let (i, _) = id_checker(&inner_ctx)(i)?;
        Ok((i, Loop { result, instrs }))
    }
}

#[inline]
fn if_<'a, 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, If, I::Error> + 'b
    where
        I::Inner: CharStream<'a>, {
    move |i: I| {
        let (i, inner_ctx) = label(ctx)(i)?;
        let (i, result) = resulttype(i)?;
        let (i, if_instrs) = many0(instr(&inner_ctx))(i)?;
        let (i, else_) = opt(tag(Keyword::Else))(i)?;
        let (i, else_instrs) = if let Some(_) = else_ {
            let (i, _) = id_checker(&inner_ctx)(i)?;
            let (i, else_instrs) = many0(instr(&inner_ctx))(i)?;
            (i, else_instrs)
        } else {
            (i, vec![])
        };
        let (i, _) = tag(Keyword::End)(i)?;
        let (i, _) = id_checker(&inner_ctx)(i)?;
        Ok((i, If { result, if_instrs, else_instrs }))
    }
}


macro_rules! def_idx_parsers {
    ($($name:ident -> $table_name:ident),*) => {
        $(
            #[inline]
            fn $name<'b, 'a: 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, u32, I::Error> + 'b
                where I::Inner: CharStream<'a>,
            {
                move |i: I| {
                    alt((
                        map_res(id, move |lit| -> StdResult<u32, ParserError> {
                            ctx.$table_name.index_of(&Some(lit.to_owned()))
                                .map_or_else(|| Err(ParserError), |x: usize| Ok(x as u32))
                        }),
                        uxx::<u32, I>
                    ))(i)
                }
            }
        )*
    }
}

def_idx_parsers! {
    localidx   -> locals,
    globalidx  -> globals,
    funcidx    -> funcs,
    labelidx   -> labels,
    typeidx    -> types
}

#[inline]
pub fn instr<'b, 'a: 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, Instruction, I::Error> + 'b
    where I::Inner: CharStream<'a>,
{
    move |i: I| {
        // FIXME we might need to handle terminal folded instructions here
        let (i, (inner_i, _)) = anykeyword(i)?;
        match INSTR_PARSERS.get(inner_i.as_str()) {
            Some(InstrParseType::Block(_)) => map(block(ctx), |v| Instruction::Block(v))(i),
            Some(InstrParseType::Loop(_)) => map(loop_(ctx), |v| Instruction::Loop(v))(i),
            Some(InstrParseType::If(_)) => map(if_(ctx), |v| Instruction::If(v))(i),
            Some(InstrParseType::NoArg(constr)) => Ok((i, constr())),
            Some(InstrParseType::LocalIdx(constr)) => map(localidx(ctx), constr)(i),
            Some(InstrParseType::GlobalIdx(constr)) => map(globalidx(ctx), constr)(i),
            // Some(InstrParseType::LabelIdx(constr)) => map(labelidx(ctx), constr)(i),
            // Some(InstrParseType::LabelIdxN(constr)) => map(many1(labelidx(ctx)), constr)(i),
            // Some(InstrParseType::FuncIdx(constr)) => map(funcidx(ctx), constr)(i),
            // Some(InstrParseType::TypeIdx(constr)) => {
            //     let (ok_i, (typeidx, locals)) = typeuse(ctx)(i.clone())?;
            //     for l in locals.iter() {
            //         if let Some(_) = l {
            //             // id ctx most be empty
            //             return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)));
            //         }
            //     };
            //     Ok((ok_i, constr(typeidx)))
            // }
            Some(InstrParseType::MemLs(default_align, constr)) => {
                map(memarg(*default_align), constr)(i)
            },
            Some(InstrParseType::ConstI32(constr)) => map(ixx::<u32, I>, constr)(i),
            Some(InstrParseType::ConstI64(constr)) => map(ixx::<u64, I>, constr)(i),
            Some(InstrParseType::ConstF32(constr)) => map(fxx::<f32, I>, constr)(i),
            Some(InstrParseType::ConstF64(constr)) => map(fxx::<f64, I>, constr)(i),
            None => Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Char))),
        }
    }
}


#[inline]
fn label<'a, 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, IdCtx, I::Error> + 'b
    where I::Inner: CharStream<'a>,
{
    move |input: I| {
        let _i = input.clone();
        let (i, c) = map_res(opt(id), |id_opt| {
            match id_opt {
                Some(id) => {
                    let item = Some(id.to_owned());
                    if ctx.labels.index_of(&item) != None {
                        Err(())
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
fn id_checker<'a, 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, (), I::Error> + 'b
    where I::Inner: CharStream<'a>,
{
    move |i: I| {
        map_res(
            opt(id),
            |id_ch_opt| {
                if let Some(id_ch) = id_ch_opt {
                    let id_opt = &ctx.labels[ctx.labels.len() - 1];
                    match id_opt {
                        None => Err(()),
                        Some(id) if id.as_str() != id_ch => Err(()),
                        _ => Ok(())
                    }
                } else {
                    Ok(())
                }
            },
        )(i)
    }
}

#[inline]
fn memarg<'a, I: TokenStream<'a> + 'a>(n: u32) -> impl Fn(I) -> IResult<I, Memarg, I::Error> + 'a
    where I::Inner: CharStream<'a>,
{
    type Inner<I> = <I as WithWrappedStream>::Inner;
    move |input: I| {
        let (i, (offset, align)) = pair(
            map_res(
                opt(tag::<I>(Keyword::OffsetEqU32)),
                |opt_o: Option<&I::Inner>| -> StdResult<u32, ParserError> {
                    if let Some(kw) = opt_o {
                        let o_i = kw.slice(7..);
                        let (_, offset) = parsed_uxx::<u32, Inner<I>>(o_i)
                            .map_err(|_| ParserError)?;
                        Ok(offset)
                    } else {
                        Ok(0)
                    }
                },
            ),
            map_res(
                opt(tag::<I>(Keyword::AlignEqU32)),
                |opt_a: Option<&I::Inner>| -> StdResult<u32, ParserError> {
                    if let Some(kw) = opt_a {
                        let a_i = kw.slice(6..);
                        let (_, align) = map_res(
                            parsed_uxx::<u32, I::Inner>,
                            |n| if n.count_ones() != 1 {
                                Err(ParserError)
                            } else {
                                Ok(n.trailing_zeros())
                            },
                        )(a_i).map_err(|_| ParserError)?;
                        Ok(align)
                    } else {
                        Ok(n)
                    }
                },
            ),
        )(input)?;

        Ok((i, Memarg { offset, align }))
    }
}

#[inline]
fn typeuse<'b, 'a: 'b, I: TokenStream<'a> + 'a>(ctx: &'b IdCtx) -> impl Fn(I) -> IResult<I, (TypeIdx, Vec<Option<String>>), I::Error> + 'b
    where I::Inner: CharStream<'a>,
{
    move |i: I| {
        let (i, (type_idx, inline)) = parc(preceded(
            tag(Keyword::Type), tuple((
                typeidx::<I>(ctx),
                alt((
                    // we should attempt inline parsing when only the result is given
                    value(None, alt((not(params), not(results)))),
                    map(pair(params, results), |v| Some(v))
                ))
            ))
        ), i)?;

        let functype = &ctx.typedefs[type_idx as usize];

        let bound_params = if let Some((params, results)) = inline {
            // results
            if results.len() != functype.results.len() {
                // index out of bound
                return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)))
            }
            for (j, vt) in results.iter().enumerate() {
                if &functype.parameters[j] != vt {
                    // types do not match
                    return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)))
                }
            }
            if params.len() != functype.parameters.len() {
                // index out of bound
                return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)))
            }

            // params

            let mut dupes = HashSet::<&'a str>::new();

            for (j, (inner_i, vt)) in params.iter().enumerate() {
                if let Some(inner_i) = inner_i {
                    if dupes.insert(inner_i.as_str()) {
                        // duplicate name
                        return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)))
                    }
                }
                if &functype.parameters[j] != vt {
                    // types do not match
                    return Err(nom::Err::Error(ParseError::from_error_kind(i.clone(), ErrorKind::Char)))
                }
            }
            params.into_iter().map(|(opt_i, _)| opt_i.map(|i| i.as_str().to_owned())).collect()
        } else {
            vec![None; functype.parameters.len()]
        };

        Ok((i, (type_idx, bound_params)))
    }
}



#[cfg(test)]
mod test {

    use nom::error::{ErrorKind};


    use crate::format::input::Stream;

    use super::*;

    type FastError<T> = (T, ErrorKind);

    #[test]
    fn test_block() {
        {
            let res = Instruction::Block(Block{ result: ResultType { valtype: Some(ValType::F32) }, instrs: vec![]});
            {
                let t = lex!("block (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("block $a (result f32) end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("block $a (result f32) end $b").unwrap();
                consumed!(instr(&Default::default()), Stream::new(&t)).unwrap_err();
            }
            {
                let t = lex!("block $a (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
        {
            let res = Instruction::Block(Block{ result: ResultType { valtype: None }, instrs: vec![]});
            {
                let t = lex!("block $a end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
    }

    #[test]
    fn test_loop() {
        {
            let res = Instruction::Loop(Loop{ result: ResultType { valtype: Some(ValType::F32) }, instrs: vec![]});
            {
                let t = lex!("loop (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("loop $a (result f32) end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("loop $a (result f32) end $b").unwrap();
                consumed!(instr(&Default::default()), Stream::new(&t)).unwrap_err();
            }
            {
                let t = lex!("loop $a (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
        {
            let res = Instruction::Loop(Loop{ result: ResultType { valtype: None }, instrs: vec![]});
            {
                let t = lex!("loop $a end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
    }

    #[test]
    fn test_if() {
        {
            let res = Instruction::If(If{ result: ResultType { valtype: Some(ValType::F32) }, if_instrs: vec![], else_instrs: vec![] });
            {
                let t = lex!("if (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("if $a (result f32) end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("if $a (result f32) end $b").unwrap();
                consumed!(instr(&Default::default()), Stream::new(&t)).unwrap_err();
            }
            {
                let t = lex!("if $a (result f32) end").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("if $a (result f32) else $a end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
            {
                let t = lex!("if $a (result f32) else $b end $a").unwrap();
                consumed!(instr(&Default::default()), Stream::new(&t)).unwrap_err();
            }
            {
                let t = lex!("if $a (result f32) else end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
        {
            let res = Instruction::If(If{ result: ResultType { valtype: None }, if_instrs: vec![], else_instrs: vec![]});
            {
                let t = lex!("if $a end $a").unwrap();
                assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
            }
        }
    }

    #[test]
    fn test_memory() {
        {
            let t = lex!("i32.load").unwrap();
            let res = Instruction::I32Load(I32Load {
                memarg: Memarg {offset: 0, align: 4}
            });
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
        {
            let t = lex!("f64.store").unwrap();
            let res = Instruction::F64Store(F64Store {
                memarg: Memarg {offset: 0, align: 8}
            });
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
        {
            let t = lex!("i64.load16_u").unwrap();
            let res = Instruction::I64Load16U(I64Load16U {
                memarg: Memarg {offset: 0, align: 2}
            });
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
        {
            let t = lex!("i64.load32_u offset=1").unwrap();
            let res = Instruction::I64Load32U(I64Load32U {
                memarg: Memarg {offset: 1, align: 4}
            });
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
        {
            let t = lex!("i32.load8_u offset=1 align=4").unwrap();
            let res = Instruction::I32Load8U(I32Load8U {
                memarg: Memarg {offset: 1, align: 4}
            });
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
        {
            let t = lex!("i32.load8_s offset=1 align=3").unwrap();
            consumed!(instr(&Default::default()), Stream::new(&t)).unwrap_err();
        }

        {
            let t = lex!("memory.size").unwrap();
            let res = Instruction::MemorySize(MemorySize {});
            assert_eq!(consumed!(instr(&Default::default()), Stream::new(&t)), Ok(res.clone()));
        }
    }


    #[test]
    fn test_num() {
        {
            let t = lex!("i32.rotr").unwrap();
            assert_eq!(
                consumed!(instr(&Default::default()), Stream::new(&t)),
                Ok(Instruction::I32Rotr(I32Rotr {}))
            );
        }

        {
            let t = lex!("i64.const 0xff43").unwrap();
            assert_eq!(
                consumed!(instr(&Default::default()), Stream::new(&t)),
                Ok(Instruction::I64Const(I64Const { param: 65347 }))
            );
        }
    }
}
