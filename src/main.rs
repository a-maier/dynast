use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::io::BufReader;
use std::fs::File;

use ahash::AHashMap;
use anyhow::{Context, Result};
use clap::Parser;
use indexmap::IndexMap;
use log::{info, debug, trace};
use nauty_pet::prelude::*;
use petgraph::{
    graph::UnGraph,
};

mod diagram;
mod form;
mod momentum;
mod symbol;

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

type Edge = (u32, u32, u32);
type Mass = u32;
type VxColour = ();

// #[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
// struct Mapper {
//     seen: AHashMap<CanonGraph<VxColour,
// }

fn main() -> Result<()> {
    let args = Args::parse();
    env_logger::init();

    let mut seen = AHashMap::new();

    for filename in &args.infiles {
        info!("Reading diagrams from {filename:?}");
        let file = File::open(filename).with_context(
            || format!("Opening {filename:?}")
        )?;
        let file = BufReader::new(file);
        let dias: IndexMap<String, Vec<Edge>>  = serde_yaml::from_reader(
            file
        ).with_context(
            || format!("Reading {filename:?}")
        )?;
        for (name, props) in dias {
            debug!("diagram {name}");
            let dia = UnGraph::<(), _>::from_edges(props);
            trace!("dia: {dia:#?}");
            let canon = CanonGraph::from(dia);
            trace!("canon: {canon:#?}");
            match seen.entry(canon) {
                Entry::Vacant(v) => {
                    println!("{name}: {name}");
                    v.insert(name);
                },
                Entry::Occupied(v) => println!("{name}: {}", v.get())
            };
        }
    }

    Ok(())
}
