mod canon;
mod mapper;
mod momentum;
mod momentum_mapping;
mod symbol;
mod yaml_dias;
mod yaml_doc_iter;

use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;

use ahash::RandomState;
use anyhow::{Context, Result};
use clap::Parser;
use log::{debug, info, trace};

use crate::yaml_dias::{Diagram, NumOrString};
use crate::yaml_doc_iter::YamlDocIter;
use crate::mapper::TopMapper;

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    /// Whether to allow mapping on subtopologies
    #[clap(short, long)]
    subtopologies: bool,

    /// Output file
    #[clap(short, long)]
    outfile: Option<PathBuf>,

    /// Topology and diagram files
    #[clap()]
    infiles: Vec<PathBuf>,
}

fn write_mappings(args: Args, mut out: impl Write) -> Result<()> {
    let mut mapper = TopMapper::new();
    if args.subtopologies {
        mapper.add_subgraphs = true;
    }

    for filename in &args.infiles {
        info!("Reading diagrams from {filename:?}");
        let file = File::open(filename)
            .with_context(|| format!("Failed to read {filename:?}"))?;
        let reader = BufReader::new(file);
        for document in YamlDocIter::new(reader) {
            let document = document?;
            trace!(
                "yaml document:\n{}",
                std::str::from_utf8(&document).unwrap()
            );
            let dias: Result<IndexMap<NumOrString, Diagram>, _> =
                serde_yaml::from_slice(&document);
            let dias = match dias {
                Ok(dias) => dias,
                Err(err) => {
                    // TODO: check for error type, but that is not accessible?!
                    if format!("{err:?}") == "EndOfStream" {
                        continue;
                    } else {
                        return Err(err).with_context(|| {
                            format!("Reading from {filename:?}")
                        });
                    }
                }
            };
            for (name, dia) in dias {
                debug!("Read {name}: {dia:#?}");
                let (topname, map) = mapper.map_dia(name.clone(), dia).with_context(
                    || format!("Mapping diagram {name}")
                )?;
                writeln!(out, "{name}: [{topname}, {map}]")?;
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
