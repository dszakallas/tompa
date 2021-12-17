use nom::{IResult, bytes::complete::take, combinator::{opt, map_res, map}, multi::many0, eof, sequence::{tuple, preceded, pair}, branch::alt};

use crate::{format::binary::{values::byte, types::{tabletype, memtype, globaltype}}, ast::{FuncType, Import, ExternFunc, ImportDesc, ExternTable, ExternMemType, ExternGlobalType, ExternTableType, TypeIdx, Table, Mem, Global, Export}};

use super::{BinaryInput, values::{uxx, vec_, name}, types::functype, instructions::expr};

use std::str;

pub fn section<'a, I: 'a + BinaryInput<'a>>(n: u8) -> impl Fn(I) -> IResult<I, Option<I>, I::Error> {
    move |i: I| {
        let (i, b) = opt(byte(n))(i)?;
        if let Some(_) = b {
            let (i, size) = uxx::<u32, I>(i)?;

            let (i, c) = take(size)(i)?;
            Ok((i, Some(c)))
        } else {
            Ok((i, None))   
        }
    }
}

pub struct Custom<I> {
    name: String,
    content: I
}

pub fn custom_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Custom<I>, I::Error> {
    let (i, name) = name(i)?;
    let (end, i) = take(i.input_len())(i)?;
    Ok((end, Custom { name, content: i }))
}

pub fn type_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<FuncType>, I::Error> {
    let (i, fts) = vec_(functype)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, fts)) 
}

pub fn importdesc<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, crate::ast::ImportDesc, I::Error> {
    alt((
        map(preceded(byte(0x00), uxx::<u32, I>), |v| ImportDesc::Func(ExternFunc(v))),
        map(preceded(byte(0x01), tabletype), |v| ImportDesc::Table(ExternTableType(v))),
        map(preceded(byte(0x02), memtype), |v| ImportDesc::Mem(ExternMemType(v))),
        map(preceded(byte(0x03), globaltype), |v| ImportDesc::Global(ExternGlobalType(v)))
    ))(i)
}

pub fn import<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Import, I::Error> {
    map(
        tuple((name, name, importdesc)),
        |(m, n, importdesc)| Import { module: m.to_owned(), name: n.to_owned(), importdesc }
    )(i)
}

pub fn import_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Import>, I::Error> {
    let (i, imports) = vec_(import)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, imports)) 
}

pub fn function_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<TypeIdx>, I::Error> {
    let (i, functions) = vec_(uxx::<u32, I>)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, functions)) 
}

pub fn table_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Table>, I::Error> {
    let (i, tables) = vec_(map(tabletype, |tpe| Table { tpe }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, tables)) 
}


pub fn mem_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Mem>, I::Error> {
    let (i, mems) = vec_(map(memtype, |tpe| Mem { tpe }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, mems)) 
}

pub fn global_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Global>, I::Error> {
    let (i, mems) = vec_(map(pair(globaltype, expr), |(tpe, expr)| Global { tpe, expr }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, mems)) 
}

pub fn export_section<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Export>, I::Error> {
    todo!()
}

// pub fn module<'a, Out, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Out, I::Error>
// {
//     let (custom, i) = custom_section(i);
//     let (tpe, i) = type_section(i);
//     let (import, i) = import_section(i);
//     let (function, i) = function_section(i);
//     let (table, i) = table_section(i);
//     let (memory, i) = memory_section(i);
//     let (global, i) = global_section(i);
// }
