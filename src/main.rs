mod momentum;
mod symbol;
mod yaml_dias;
mod yaml_doc_iter;

use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::io::{BufReader, BufWriter, Write};
use std::fs::File;
use std::convert::TryFrom;

use ahash::AHashMap;
use anyhow::{Context, Result};
use clap::Parser;
use indexmap::IndexMap;
use log::{info, debug, trace};
use nauty_pet::prelude::*;

use crate::yaml_dias::{NumOrString, Diagram};
use crate::yaml_doc_iter::YamlDocIter;

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    /// Output file
    #[clap(short, long)]
    outfile: Option<PathBuf>,

    /// Topology and diagram files
    #[clap()]
    infiles: Vec<PathBuf>,
}

fn write_mappings(
    args: Args,
    mut out: impl Write
) -> Result<()> {
    let mut seen = AHashMap::new();

    for filename in &args.infiles {
        info!("Reading diagrams from {filename:?}");
        let file = File::open(filename).with_context(
            || format!("Failed to read {filename:?}")
        )?;
        let reader = BufReader::new(file);
        for document in YamlDocIter::new(reader) {
            let document = document?;
            trace!("yaml document:\n{}", std::str::from_utf8(&document).unwrap());
            let dias: Result<IndexMap<NumOrString, Diagram>, _> = serde_yaml::from_slice(&document);
            let dias = match dias {
                Ok(dias) => dias,
                Err(err) => {
                    // TODO: check for error type, but that is not accessible?!
                    if format!("{err:?}") == "EndOfStream" {
                        continue
                    } else {
                        return Err(err).with_context(
                            || format!("Reading from {filename:?}")
                        )
                    }
                }
            };
            for (name, dia) in dias {
                debug!("Read {name}: {dia:#?}");
                let graph = CanonGraph::try_from(
                    dia
                ).with_context(
                    || format!("Parsing diagram {name}")
                )?;
                trace!("Canonical graph {graph:#?}");

                match seen.entry(graph) {
                    Entry::Vacant(v) => {
                        writeln!(out, "{0}: {0}", name)?;
                        v.insert(name);
                    },
                    Entry::Occupied(o) => {
                        writeln!(out, "{}: {}", name, o.get())?
                    }
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
