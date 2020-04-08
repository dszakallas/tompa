use std::path::{Path, PathBuf};
use std::{io, fs};
use std::rc::Rc;
use std::ops::{Sub, Add};


#[derive(Debug)]
pub struct OffsetOverflowError;


/// Differentiates between real files and common virtual files.
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum FileName {
    Real(PathBuf)
}

/// An abstraction over the fs operations used by the Parser.
pub trait FileLoader {
    /// Read the contents of an UTF-8 file into memory.
    fn read_file(&self, path: &Path) -> io::Result<String>;
}

pub struct RealFileLoader;

/// A FileLoader that uses std::fs to load real files.
impl FileLoader for RealFileLoader {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }
}

#[derive(Default)]
pub(super) struct SourceMapFiles {
    source_files: Vec<Rc<SourceFile>>
}

pub struct SourceMap {
    /// The address space below this value is currently used by the files in the source map.
    used_address_space: u32,
    files: SourceMapFiles,
    file_loader: Box<dyn FileLoader + Sync + Send>,
    // This is used to apply the file path remapping as specified via
    // `--remap-path-prefix` to all `SourceFile`s allocated within this `SourceMap`.
    path_mapping: FilePathMapping,
}

impl SourceMap {

    /// Creates a new `SourceFile`.
    /// If a file already exists in the `SourceMap` with the same ID, that file is returned
    /// unmodified.
    pub fn new_source_file(&mut self, filename: FileName, src: String) -> Rc<SourceFile> {
        self.try_new_source_file(filename, src).unwrap_or_else(|_: OffsetOverflowError| {
            panic!("fatal error: wasmc does not support files larger than 4GB");
        })
    }

    fn try_new_source_file(
        &mut self,
        filename: FileName,
        src: String,
    ) -> Result<Rc<SourceFile>, OffsetOverflowError> {
        // The path is used to determine the directory for loading submodules and
        // include files, so it must be before remapping.
        // Note that filename may not be a valid path, eg it may be `<anon>` etc,
        // but this is okay because the directory determined by `path.pop()` will
        // be empty, so the working directory will be used.
        let unmapped_path = filename.clone();

        let (filename, was_remapped) = match filename {
            FileName::Real(filename) => {
                let (filename, was_remapped) = self.path_mapping.map_prefix(filename);
                (FileName::Real(filename), was_remapped)
            }
            other => (other, false),
        };

        let start_pos = self.allocate_address_space(src.len())?;

        let source_file = Rc::new(SourceFile::new(
            filename,
            was_remapped,
            unmapped_path,
            src,
            Pos::from_usize(start_pos),
        ));

        let mut files = &mut self.files;

        files.source_files.push(source_file.clone());

        Ok(source_file)
    }
}

/// A single source in the `SourceMap`.
#[derive(Clone)]
pub struct SourceFile {
    /// The name of the file that the source came from. Source that doesn't
    /// originate from files has names between angle brackets by convention
    /// (e.g., `<anon>`).
    pub name: FileName,
    /// `true` if the `name` field above has been modified by `--remap-path-prefix`.
    pub name_was_remapped: bool,
    /// The unmapped path of the file that the source came from.
    pub unmapped_path: Option<FileName>,
    /// The complete source code.
    pub src: Rc<String>,
    /// The start position of this source in the `SourceMap`.
    pub start_pos: BytePos,
    /// The end position of this source in the `SourceMap`.
    pub end_pos: BytePos,
    /// Locations of lines beginnings in the source code.
    pub lines: Vec<BytePos>,
    /// Locations of multi-byte characters in the source code.
    pub multibyte_chars: Vec<MultiByteChar>,
    /// Width of characters that are not narrow in the source code.
    pub non_narrow_chars: Vec<NonNarrowChar>,
    /// Locations of characters removed during normalization.
    pub normalized_pos: Vec<NormalizedPos>,
}

impl SourceFile {
    pub fn new(
        name: FileName,
        name_was_remapped: bool,
        unmapped_path: FileName,
        mut src: String,
        start_pos: BytePos,
    ) -> Self {
        let normalized_pos = normalize_src(&mut src, start_pos);

        let end_pos = start_pos.to_usize() + src.len();
        assert!(end_pos <= u32::max_value() as usize);

        let (lines, multibyte_chars, non_narrow_chars) =
            analyze_source_file::analyze_source_file(&src[..], start_pos);

        SourceFile {
            name,
            name_was_remapped,
            unmapped_path: Some(unmapped_path),
            src: Rc::new(src),
            start_pos,
            end_pos: Pos::from_usize(end_pos),
            lines,
            multibyte_chars,
            non_narrow_chars,
            normalized_pos
        }
    }
}

/// Normalizes the source code and records the normalizations.
fn normalize_src(src: &mut String, start_pos: BytePos) -> Vec<NormalizedPos> {
    let mut normalized_pos = vec![];
    remove_bom(src, &mut normalized_pos);
    //normalize_newlines(src, &mut normalized_pos);

    // Offset all the positions by start_pos to match the final file positions.
    for np in &mut normalized_pos {
        np.pos.0 += start_pos.0;
    }

    normalized_pos
}

#[derive(Clone)]
pub struct FilePathMapping {
    mapping: Vec<(PathBuf, PathBuf)>,
}

impl FilePathMapping {
    pub fn empty() -> FilePathMapping {
        FilePathMapping { mapping: vec![] }
    }

    pub fn new(mapping: Vec<(PathBuf, PathBuf)>) -> FilePathMapping {
        FilePathMapping { mapping }
    }

    /// Applies any path prefix substitution as defined by the mapping.
    /// The return value is the remapped path and a boolean indicating whether
    /// the path was affected by the mapping.
    pub fn map_prefix(&self, path: PathBuf) -> (PathBuf, bool) {

        // NOTE: We are iterating over the mapping entries from last to first
        //       because entries specified later on the command line should
        //       take precedence.
        for &(ref from, ref to) in self.mapping.iter().rev() {
            if let Ok(rest) = path.strip_prefix(from) {
                return (to.join(rest), true);
            }
        }

        (path, false)
    }
}

// _____________________________________________________________________________
// Pos, BytePos, CharPos
//

pub trait Pos {
    fn from_usize(n: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn from_u32(n: u32) -> Self;
    fn to_u32(&self) -> u32;
}

/// A byte offset. Keep this small (currently 32-bits), as AST contains
/// a lot of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

/// A character offset. Because of multibyte UTF-8 characters, a byte offset
/// is not equivalent to a character offset. The `SourceMap` will convert `BytePos`
/// values to `CharPos` values as necessary.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CharPos(pub usize);

// FIXME: lots of boilerplate in these impls, but so far my attempts to fix
// have been unsuccessful.

impl Pos for BytePos {
    #[inline(always)]
    fn from_usize(n: usize) -> BytePos {
        BytePos(n as u32)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    fn from_u32(n: u32) -> BytePos {
        BytePos(n)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0
    }
}

impl Add for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn add(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() + rhs.to_usize()) as u32)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() - rhs.to_usize()) as u32)
    }
}

impl Pos for CharPos {
    #[inline(always)]
    fn from_usize(n: usize) -> CharPos {
        CharPos(n)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0
    }

    #[inline(always)]
    fn from_u32(n: u32) -> CharPos {
        CharPos(n as usize)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl Add for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn add(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() + rhs.to_usize())
    }
}

impl Sub for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn sub(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() - rhs.to_usize())
    }
}

/// Identifies an offset of a multi-byte character in a `SourceFile`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct MultiByteChar {
    /// The absolute offset of the character in the `SourceMap`.
    pub pos: BytePos,
    /// The number of bytes, `>= 2`.
    pub bytes: u8,
}

/// Identifies an offset of a non-narrow character in a `SourceFile`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NonNarrowChar {
    /// Represents a zero-width character.
    ZeroWidth(BytePos),
    /// Represents a wide (full-width) character.
    Wide(BytePos),
    /// Represents a tab character, represented visually with a width of 4 characters.
    Tab(BytePos),
}

/// Identifies an offset of a character that was normalized away from `SourceFile`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct NormalizedPos {
    /// The absolute offset of the character in the `SourceMap`.
    pub pos: BytePos,
    /// The difference between original and normalized string at position.
    pub diff: u32,
}

