use std::path::PathBuf;

use ahash::AHashMap;
use anyhow::Result;
use clap::Parser;

use crate::symbol::Symbol;
use crate::writer::OutFormat;
use crate::{momentum::Momentum, yaml_dias::NumOrString};

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
pub(crate) struct Args {
    /// Whether to allow mapping onto subtopologies.
    #[clap(short, long)]
    pub(crate) subtopologies: bool,

    /// Sort diagrams before processing them
    ///
    /// Diagrams are sorted by decreasing number of propagators, using
    /// lexicographic ordering by name in case of ties. This option is
    /// mostly useful in combination with `--subtopologies` to ensure
    /// that a minimal set of topologies is selected. Note that in
    /// contrast to standard settings, all diagrams have to be read
    /// into memory, greatly increasing memory usage and delaying the
    /// actual mapping onto topologies.
    #[clap(short, long)]
    pub(crate) sort: bool,

    /// Whether to keep multiple propagators with the same momentum
    #[clap(short, long)]
    pub(crate) keep_duplicate: bool,

    // Ugly workaround:
    // Since clap requires `Display` to allow default values
    // we first privately parse an `Option` and make it available
    // as an actual map
    /// Replace masses
    #[clap(long, value_parser = parse_m_map)]
    replace_masses: Option<AHashMap<NumOrString, NumOrString>>,

    #[clap(skip)]
    replace_masses_actual: AHashMap<NumOrString, NumOrString>,

    /// Replace momenta
    #[clap(long, value_parser = parse_p_map)]
    replace_momenta: Option<AHashMap<Symbol, Momentum>>,

    #[clap(skip)]
    replace_momenta_actual: AHashMap<Symbol, Momentum>,

    /// Output format.
    #[clap(
        short,
        long,
        value_enum,
        default_value = "yaml",
        alias = "outformat"
    )]
    pub(crate) format: OutFormat,

    /// Whether to decompose integrals into one-particle-irreducible factors.
    #[clap(long, alias = "1PI")]
    pub(crate) factors: bool,

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

fn parse_m_map(s: &str) -> Result<AHashMap<NumOrString, NumOrString>> {
    serde_yaml::from_str(s).map_err(|e| e.into())
}

fn parse_p_map(s: &str) -> Result<AHashMap<Symbol, Momentum>> {
    let map: AHashMap<NumOrString, NumOrString> = serde_yaml::from_str(s)?;
    let mut res = AHashMap::with_capacity(map.len());
    for (key, val) in map {
        let key = Symbol::new(&key.to_string());
        let val = Momentum::try_from(val)?;
        res.insert(key, val);
    }
    Ok(res)
}

impl Args {
    pub(crate) fn parse() -> Args {
        let mut args = <Self as clap::Parser>::parse();
        args.replace_masses_actual =
            args.replace_masses.take().unwrap_or_default();
        args.replace_momenta_actual =
            args.replace_momenta.take().unwrap_or_default();
        args
    }

    pub(crate) fn replace_masses(&self) -> &AHashMap<NumOrString, NumOrString> {
        &self.replace_masses_actual
    }

    pub(crate) fn replace_momenta(&self) -> &AHashMap<Symbol, Momentum> {
        &self.replace_momenta_actual
    }
}
