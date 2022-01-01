use core::fmt;
use std::str::FromStr;

use clap::{ArgEnum, Parser};

const DEFAULT_OUTPUT_FILE: &str = "out.xvl";
const DEFAULT_FORMAT: &str = "xvl";

#[derive(Parser, Debug)]
#[clap(version, name = "vxasm", rename_all = "lower")]
#[clap(about = "Assembles .vsm files into files executable by the voxeol virtual machine.")]
pub struct CLIArgs {
    /// Specifies the type of assembled output from the assembler
    #[clap(arg_enum, short = 'f', long = "format", default_value = DEFAULT_FORMAT)]
    pub format: AssemblyFormat,

    /// The files to assemble
    #[clap(min_values = 1)]
    pub input_files: Vec<String>,

    /// The output file name
    #[clap(short = 'o', default_value=DEFAULT_OUTPUT_FILE)]
    pub output_file: String,

    /// The root file for assembly
    #[clap(short = 'r')]
    pub root_file: Option<String>,

    /// Specify flags for the preprocessor
    #[clap(short = 'F')]
    pub flags: Vec<String>,

    /// When assembling with the xvl format use sha2 for the checksum instead of sha3.
    #[clap(long = "sha2")]
    pub sha2: bool,

    /// Specify the label to be used as the entry point
    #[clap(long = "entry")]
    pub entry_point: Option<String>,
}

#[derive(ArgEnum, Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum AssemblyFormat {
    Raw,
    Xvl,
}

impl FromStr for AssemblyFormat {
    type Err = clap::ErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return Ok(match s {
            "raw" => Self::Raw,
            "xvl" => Self::Xvl,
            _ => return Err(clap::ErrorKind::InvalidValue),
        });
    }
}

impl fmt::Display for AssemblyFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            AssemblyFormat::Raw => write!(f, "raw"),
            AssemblyFormat::Xvl => write!(f, "xvl"),
        };
    }
}
