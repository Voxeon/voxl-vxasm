use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

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
    file_obj: Rc<FileInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileInfo {
    file_name: String,
    file_contents: String,
    id: usize,
}

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

impl TextRange {
    pub fn new(starting_pos: Position, ending_pos: Position, file: Rc<FileInfo>) -> Self {
        return Self {
            starting_pos,
            ending_pos,
            file_obj: file,
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
        return self.file_obj.substring(self.starting_pos, self.ending_pos);
    }

    pub fn compare_contents(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        return self.string() == other.string();
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
}
