use std::path::PathBuf;

use clap::Parser;

use crate::writer::OutFormat;

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
pub(crate) struct Args {
    /// Whether to allow mapping on subtopologies.
    #[clap(short, long)]
    pub(crate) subtopologies: bool,

    /// Whether to keep multiple propagators with the same momentum
    #[clap(short, long)]
    pub(crate) keep_duplicate: bool,

    /// Output format.
    #[clap(short, long, value_enum, default_value = "yaml", alias = "outformat")]
    pub(crate) format: OutFormat,

    /// Output file. Print to standard output if absent.
    #[clap(short, long)]
    pub(crate) outfile: Option<PathBuf>,

    /// Topology and diagram files.
    #[clap()]
    pub(crate) infiles: Vec<PathBuf>,

    /// Verbosity level.
    #[clap(
        short,
        long,
        default_value = "info",
        help = "Verbosity level.
Possible values with increasing amount of output are
'off', 'error', 'warn', 'info', 'debug', 'trace'."
    )]
    pub(crate) loglevel: String,
}
