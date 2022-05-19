//! dynasty is a program for identifying the topologies (or integral
//! families) of Feynman graphs. It is based on [nauty and
//! Traces](http://pallini.di.uniroma1.it/).
//!
//! # Installation
//!
//! First, ensure that version 2.6 or 2.7 of the nauty and Traces
//! library is installed. Then, if [Rust and Cargo](https://www.rust-lang.org/)
//! are installed on your system, run
//!
//!     cargo install dynasty
//!
//! Precompiled executables are available on
//! [github](https://github.com/a-maier/dynasty).
//!
//! # Usage
//!
//! dynasty reads in Feynman diagrams in a
//! [YAML](https://yaml.org/)-based format and, for each diagram,
//! prints its topology and how loop momenta have to be shifted to
//! obtain a uniform assignment of propagator momenta.
//!
//! Basic usage is
//!
//!   dynasty -o outfile.yml diagrams.yml
//!
//! It is possible to pass more than one input file, for instance
//!
//!   dynasty -o outfile.yml topologies.yml diagrams.yml
//!
//! to ensure the diagrams in `diagrams.yml` are mapped onto the
//! topologies defined in `topologies.yml` as far as possible.
//!
//! ## Important Options
//!
//! * Use the `-s` flag to allow mapping onto subtopologies, i.e. onto
//!   diagrams where one or more propagators have been contracted.
//!
//! ## Input format
//!
//! The input has the form
//! ```yaml
//! diagram0:
//!   - [from0, to0, p0, m0] # first propagator
//!   - [from1, to1, p1, m1] # second propagator
//!   # further propagators ...
//! ---
//! diagram1:
//!   # propagators ...
//! ```
//! `from` and `to` are non-negative integer vertex labels designating
//! the start and end of the propagator line. `p` is the propagator
//! momentum (e.g. `l1 + q`) and `m` its mass. It is allowed to omit
//! both `from` and `to` and the same time to denote a scalar product
//! that is not associated with a graph edge. `dynasty` will ignore
//! such scalar products.
//!
//! The document separator `---` is optional but recommended for large
//! input files to save memory.
//!
//! dynasty includes a
//! [QGRAF](http://cfif.ist.utl.pt/~paulo/qgraf.html) style file
//! `share/qgraf/yaml.sty` that generates the required input. If
//! several fields have the same mass the corresponding masses in the
//! QGRAF output have to be adjusted manually.
//!
//! ## Output formats
//!
//! ### YAML
//!
//! The default YAML output consists of records
//! ```yaml
//! diagram: [topology, {l1: p1, ...}]
//! ```
//! where `diagram` is the diagram name and `topology` the first
//! passed diagram with the same topology. The last part of the entry
//! indicates how loop momenta have to be replaced to arrive at the
//! same momentum assignment as in `topology`.
//!
//! ### [FORM](https://github.com/vermaseren/form)
//!
//! Alternatively `-f form` to produce output for FORM. In this case,
//! mapping information is only written for diagrams that have integer
//! numbers as names. Then, an output file `topologies.frm` can be
//! used as follows:
//! ```FORM
//! cf dia;
//! #include- topologies.frm
//! * example diagram
//! * the argument of `dia` should match the name in the dynasty input file
//! local diagrams =
//! + dia(1)
//! * ... diagram information (propagators, vertices)
//! + dia(2)
//! * ... more diagrams
//! ;
//!
//! id dia(?a) = dia(?a) * topology(?a);
//! if(match(top(?a$a)) && match(top(?b$b)));
//!    print "the topology of diagram %$ is %$" $a $b;
//! endif;
//! * shift momenta to canonical form
//! id replace(?a) = replace_(?a);
//! .end
//! ```
mod canon;
mod graph_util;
mod mapper;
mod momentum;
mod momentum_mapping;
mod symbol;
mod version;
mod writer;
mod yaml_dias;
mod yaml_doc_iter;

use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;

use ahash::RandomState;
use anyhow::{Context, Result};
use clap::Parser;
use env_logger::Env;
use log::{debug, info, trace};

use crate::graph_util::Format;
use crate::mapper::TopMapper;
use crate::version::VERSION_STRING;
use crate::writer::{write, write_header, OutFormat};
use crate::yaml_dias::{Diagram, NumOrString};
use crate::yaml_doc_iter::YamlDocIter;

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

/// Map diagrams onto topologies
#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    /// Whether to allow mapping on subtopologies.
    #[clap(short, long)]
    subtopologies: bool,

    /// Output format.
    #[clap(short, long, arg_enum, default_value = "yaml")]
    format: OutFormat,

    /// Output file. Print to standard output if absent.
    #[clap(short, long)]
    outfile: Option<PathBuf>,

    /// Topology and diagram files.
    #[clap()]
    infiles: Vec<PathBuf>,

    /// Verbosity level.
    #[clap(
        short,
        long,
        default_value = "info",
        help = "Verbosity level.
Possible values with increasing amount of output are
'off', 'error', 'warn', 'info', 'debug', 'trace'."
    )]
    loglevel: String,
}

fn write_mappings(args: Args, mut out: impl Write) -> Result<()> {
    let mut mapper = TopMapper::new();
    if args.subtopologies {
        mapper.add_subgraphs = true;
    }

    write_header(&mut out, args.format)
        .with_context(|| "Failed to write output header")?;

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
                debug!("Read {name}: {}", dia.format());
                let (topname, map) = mapper
                    .map_dia(name.clone(), dia)
                    .with_context(|| format!("Mapping diagram {name}"))?;
                write(&mut out, &name, &topname, &map, args.format)?;
            }
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    let env = Env::default().filter_or("DYNASTY_LOG", &args.loglevel);
    env_logger::init_from_env(env);
    info!("{}", &*VERSION_STRING);

    if let Some(filename) = &args.outfile {
        let out = File::create(filename).with_context(
            || format!("Trying to create {filename:?}")
        )?;
        let out = BufWriter::new(out);
        write_mappings(args, out)
    } else {
        write_mappings(args, std::io::stdout())
    }
}
