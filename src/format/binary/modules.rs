use nom::{IResult, bytes::complete::{take, tag}, combinator::{opt, map_res, map}, multi::many0, eof, sequence::{tuple, preceded, pair, terminated}, branch::alt, error::{ParseError, ErrorKind}};

use crate::{format::binary::{values::byte, types::{tabletype, memtype, globaltype}}, ast::{FuncType, Import, ExternFunc, ImportDesc, ExternTable, ExternMemType, ExternGlobalType, ExternTableType, TypeIdx, Table, Mem, Global, Export, ExportDesc, ExternMem, ExternGlobal, Start, Element, Instruction, ValType, Function, Module, Data}};

use super::{BinaryInput, values::{uxx, vec_, name, vec_of_byte}, types::{functype, valtype}, instructions::expr};

use std::iter::{zip, repeat};

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

fn custom_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Custom<I>, I::Error> {
    let (i, name) = name(i)?;
    let (end, i) = take(i.input_len())(i)?;
    Ok((end, Custom { name, content: i }))
}

fn type_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<FuncType>, I::Error> {
    let (i, fts) = vec_(functype)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, fts)) 
}

fn importdesc<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, ImportDesc, I::Error> {
    alt((
        map(preceded(byte(0x00), uxx::<u32, I>), |v| ImportDesc::Func(ExternFunc(v))),
        map(preceded(byte(0x01), tabletype), |v| ImportDesc::Table(ExternTableType(v))),
        map(preceded(byte(0x02), memtype), |v| ImportDesc::Mem(ExternMemType(v))),
        map(preceded(byte(0x03), globaltype), |v| -> ImportDesc {ImportDesc::Global(ExternGlobalType(v))})
    ))(i)
}

fn import<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Import, I::Error> {
    map(
        tuple((name, name, importdesc)),
        |(m, n, importdesc)| Import { module: m.to_owned(), name: n.to_owned(), importdesc }
    )(i)
}

fn import_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Import>, I::Error> {
    let (i, imports) = vec_(import)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, imports)) 
}

fn function_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<TypeIdx>, I::Error> {
    let (i, functions) = vec_(uxx::<u32, I>)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, functions)) 
}

fn table_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Table>, I::Error> {
    let (i, tables) = vec_(map(tabletype, |tpe| Table { tpe }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, tables)) 
}


fn mem_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Mem>, I::Error> {
    let (i, mems) = vec_(map(memtype, |tpe| Mem { tpe }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, mems)) 
}

fn global_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Global>, I::Error> {
    let (i, mems) = vec_(map(pair(globaltype, expr), |(tpe, expr)| Global { tpe, expr }))(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, mems)) 
}

fn exportdesc<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, ExportDesc, I::Error> {
    alt((
        map(preceded(byte(0x00), uxx::<u32, I>), |v| ExportDesc::Func(ExternFunc(v))),
        map(preceded(byte(0x01), uxx::<u32, I>), |v| ExportDesc::Table(ExternTable(v))),
        map(preceded(byte(0x02), uxx::<u32, I>), |v| ExportDesc::Mem(ExternMem(v))),
        map(preceded(byte(0x03), uxx::<u32, I>), |v| ExportDesc::Global(ExternGlobal(v)))
    ))(i)
}

fn export<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Export, I::Error> {
    map(
        tuple((name, exportdesc)),
        |(name, exportdesc)| Export { name, exportdesc }
    )(i)
}

fn export_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Export>, I::Error> {
    let (i, mems) = vec_(export)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, mems)) 
}

fn start_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Option<Start>, I::Error> {
    let (i, start) = uxx::<u32, I>(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, Some(Start(start))))
}

fn element<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Element, I::Error> {
    let (i, table) = uxx::<u32, I>(i)?;
    let (i, offset) = expr(i)?;
    let (i, init) = vec_(uxx::<u32, I>)(i)?;
    Ok((i, Element { table, offset, init }))
}

fn element_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Element>, I::Error> {
    let (i, elements) = vec_(element)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, elements)) 
}

pub struct Code {
    pub size: u32,
    pub func: FuncBody
}

pub struct FuncBody {
    pub locals: Vec<Locals>,
    pub expr: Vec<Instruction>
}

pub struct Locals {
    pub n: u32,
    pub t: ValType
}


fn code<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Code, I::Error> {
    let (i, size) = uxx(i)?;
    let (i, j) = take(size)(i)?;
    let (j, locals) = vec_(map(pair(uxx::<u32, I>, valtype), |(n, t)| Locals { n, t }))(j)?;
    let (_, expr) = expr(j)?;
    let func = FuncBody { locals, expr };
    Ok((i, Code { size, func })) 
}

fn code_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Code>, I::Error> {
    let (i, code) = vec_(code)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, code)) 
}


fn data<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Data, I::Error> {
    let (i, data) = uxx::<u32, I>(i)?;
    let (i, offset) = expr(i)?;
    let (i, init) = vec_of_byte(i)?;
    Ok((i, Data { data, offset, init }))
}

fn data_section<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Vec<Data>, I::Error> {
    let (i, data) = vec_(data)(i)?;
    let (i, _) = eof!(i.clone(),)?;
    Ok((i, data)) 
}

static MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];
static VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

pub fn module<'a, I: 'a + BinaryInput<'a>>(i: I) -> IResult<I, Module, I::Error>
{
    let (i, _) = tag(MAGIC.as_slice())(i)?;
    let (i, _) = tag(VERSION.as_slice())(i)?;
    let (i, _) = many0(custom_section)(i)?;
    let (i, types) = terminated(type_section, many0(custom_section))(i)?;
    let (i, imports) = terminated(import_section, many0(custom_section))(i)?;
    let (i, function) = terminated(function_section, many0(custom_section))(i)?;
    let (i, tables) = terminated(table_section, many0(custom_section))(i)?;
    let (i, mems) = terminated(mem_section, many0(custom_section))(i)?;
    let (i, globals) = terminated(global_section, many0(custom_section))(i)?;
    let (i, exports) = terminated(export_section, many0(custom_section))(i)?;
    let (i, start) = terminated(start_section, many0(custom_section))(i)?;
    let (i, elem) = terminated(element_section, many0(custom_section))(i)?;
    let (i, code) = terminated(code_section, many0(custom_section))(i)?;
    let (i, data) = terminated(data_section, many0(custom_section))(i)?;

    if code.len() != function.len() {
        return Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::MapRes)))
    }

    let funcs: Vec<Function> = zip(function, code)
        .map(|(f, c)| Function {
            type_idx: f,
            locals: c.func.locals.into_iter().map(|l| repeat(l.t).take(l.n as usize)).flatten().collect(),
            body: c.func.expr
        })
        .collect();

    let module = Module {
        types,
        funcs,
        tables,
        mems,
        globals,
        elem,
        data,
        start,
        imports,
        exports,
    };
    Ok((i, module))
}
