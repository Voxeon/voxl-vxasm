use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use either::Either;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Position {
    index: usize,
    row: usize,
    col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TextRange {
    starting_pos: Position,
    ending_pos: Position,
    source: Either<FilePtr, Rc<AssemblyString>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AssemblyString(String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileInfo {
    file_name: String,
    file_contents: String,
    id: usize,
}

pub type Source = Either<FilePtr, Rc<AssemblyString>>;
pub type FilePtr = Rc<FileInfo>;

#[derive(Debug)]
pub struct FileInfoManager {
    file_info_refs: Vec<Rc<FileInfo>>,
}

impl Position {
    pub fn new(index: usize, row: usize, col: usize) -> Self {
        return Self { index, row, col };
    }

    pub fn index(&self) -> usize {
        return self.index;
    }

    pub fn row(&self) -> usize {
        return self.row;
    }

    pub fn col(&self) -> usize {
        return self.col;
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}:{}", self.row, self.col);
    }
}

impl TextRange {
    pub fn new(starting_pos: Position, ending_pos: Position, source: Source) -> Self {
        return Self {
            starting_pos,
            ending_pos,
            source,
        };
    }

    pub fn start(&self) -> Position {
        return self.starting_pos;
    }

    pub fn end(&self) -> Position {
        return self.ending_pos;
    }

    pub fn len(&self) -> usize {
        return self.ending_pos.index() - self.starting_pos.index();
    }

    pub fn string(&self) -> String {
        return match &self.source {
            Either::Left(f) => f.substring(self.starting_pos, self.ending_pos),
            Either::Right(s) => s.substring(self.starting_pos, self.ending_pos),
        };
    }

    pub fn compare_contents(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        return self.string() == other.string();
    }
}

impl fmt::Display for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(feature = "show-source_string"))]
        return write!(
            f,
            "\"{}\" in {} at {}",
            self.string(),
            self.source,
            self.starting_pos
        );
        #[cfg(feature = "show-source_string")]
        return match &self.source {
            Either::Left(file) => write!(
                f,
                "\"{}\" in {} at {}",
                self.string(),
                file,
                self.starting_pos
            ),
            Either::Right(s) => write!(
                f,
                "\"{}\" at {} in source string:\n{}",
                self.string(),
                self.starting_pos,
                s
            ),
        };
    }
}

impl AssemblyString {
    /// Returns a clone of the sub-string from start to end, inclusive of start but exclusive of end.
    ///
    /// Panics if the end position is greater than the lenght of the file.
    pub fn substring(&self, start: Position, end: Position) -> String {
        if end.index() > self.0.len() {
            panic!("End position larger than string contents.");
        }

        return self.0[start.index()..end.index()].to_string();
    }
}

impl fmt::Display for AssemblyString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(feature = "show-source_string"))]
        return write!(f, "Source String");
        #[cfg(feature = "show-source_string")]
        return write!(f, "{}", self.0);
    }
}

impl From<String> for AssemblyString {
    fn from(s: String) -> Self {
        return Self(s);
    }
}

impl Into<String> for AssemblyString {
    fn into(self) -> String {
        return self.0;
    }
}

impl FileInfo {
    /// Creates a new instance of FileInfo.
    fn new(id: usize, name: String, contents: String) -> Self {
        return Self {
            id,
            file_name: name,
            file_contents: contents,
        };
    }

    pub fn id(&self) -> usize {
        return self.id;
    }

    pub fn name(&self) -> &String {
        return &self.file_name;
    }

    pub fn contents(&self) -> &String {
        return &self.file_contents;
    }

    /// Returns a clone of the sub-string from start to end, inclusive of start but exclusive of end.
    ///
    /// Panics if the end position is greater than the lenght of the file.
    pub fn substring(&self, start: Position, end: Position) -> String {
        if end.index() > self.file_contents.len() {
            panic!("End position larger than file contents.");
        }

        return self.file_contents[start.index()..end.index()].to_string();
    }
}

impl fmt::Display for FileInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", &self.file_name);
    }
}

impl FileInfoManager {
    pub fn new() -> Self {
        return Self {
            file_info_refs: Vec::new(),
        };
    }

    pub fn new_file(&mut self, name: String, contents: String) -> Rc<FileInfo> {
        let file_info = Rc::new(FileInfo::new(self.file_info_refs.len(), name, contents));

        self.file_info_refs.push(file_info.clone());

        return file_info;
    }

    pub fn get_file_info(&self, name: &str) -> Option<Rc<FileInfo>> {
        for f in &self.file_info_refs {
            if f.name() == name {
                return Some(f.clone());
            }
        }

        return None;
    }

    pub fn get_file_info_refs(&self) -> &Vec<Rc<FileInfo>> {
        return &self.file_info_refs;
    }
}
