mod conv;
mod diagram;
mod form;
mod momentum;
mod symbol;

use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::io::{BufWriter, Write};
use std::fs::{File, read_to_string};

use ahash::AHashMap;
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use indexmap::IndexMap;
use log::{info, debug, trace};
use petgraph::{
    graph::UnGraph,
};

use crate::conv::{GraphConv, Mass};
use crate::form::FormDiaParser;

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    // Masses
    #[clap(short, long, parse(try_from_str = parse_map))]
    masses: AHashMap<String, Mass>,

    /// Output file
    #[clap(short, long)]
    outfile: Option<PathBuf>,

    /// Topology and diagram files
    #[clap()]
    infiles: Vec<PathBuf>,
}

fn parse_map(input: &str) -> Result<AHashMap<String, i32>, serde_yaml::Error> {
    serde_yaml::from_reader(input.as_bytes())
}

fn write_mappings(
    args: Args,
    mut out: impl Write
) -> Result<()> {
    let mut seen = AHashMap::new();
    let conv = GraphConv::new(args.masses);

    for filename in &args.infiles {
        info!("Reading diagrams from {filename:?}");
        let input = read_to_string(filename).with_context(
            || format!("Failed to read {filename:?}")
        )?;
        let dias = FormDiaParser::new(input.as_str());
        for dia in dias {
            let dia = dia.map_err(|e| anyhow!("{}", e))?;
            let graph = conv.to_petgraph(&dia)?;
            match seen.entry(graph) {
                Entry::Vacant(v) => {
                    writeln!(out, "{0}: {0}", dia.name)?;
                    v.insert(dia);
                },
                Entry::Occupied(o) => {
                    writeln!(out, "{}: {}", dia.name, o.get().name)?
                }
            }
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    env_logger::init();

    if let Some(filename) = &args.outfile {
        let out = BufWriter::new(File::create(filename)?);
        write_mappings(args, out)
    } else {
        write_mappings(args, std::io::stdout())
    }
}
