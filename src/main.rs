mod canon;
mod momentum;
mod momentum_mapping;
mod symbol;
mod yaml_dias;
mod yaml_doc_iter;

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
use petgraph::Graph;

use crate::canon::into_canon;
use crate::momentum_mapping::Mapping;
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
    let mut seen: AHashMap<CanonGraph<_, _, _>, _> = AHashMap::new();

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
                let graph = Graph::try_from(
                    dia
                ).with_context(
                    || format!("Parsing diagram {name}")
                )?;
                trace!("Graph {graph:#?}");

                let canon = into_canon(graph);

                if let Some((known, topname)) = seen.get_key_value(&canon) {
                    let mapping = Mapping::new(canon.get(), known.get())?;
                    writeln!(out, "{name}: [{topname}, {mapping}]")?;
                } else {
                    writeln!(out, "{name}: [{name}, {}]", Mapping::identity(canon.get()))?;
                    seen.insert(canon, name);
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
