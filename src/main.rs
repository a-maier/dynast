//! dynast is a program for identifying the topologies (or integral
//! families) of Feynman graphs. It is based on [nauty and
//! Traces](http://pallini.di.uniroma1.it/).
//!
//! # Installation
//!
//! If [Rust and Cargo](https://www.rust-lang.org/) are installed on your
//! system, run
//!
//!     cargo install dynast
//!
//! Precompiled executables are available on
//! [github](https://github.com/a-maier/dynast).
//!
//! # Usage
//!
//! dynast reads in Feynman diagrams in a [YAML](https://yaml.org/) or
//! [FORM](https://github.com/vermaseren/form)-based format and, for each
//! diagram, prints its topology and how loop momenta have to be shifted
//! to obtain a uniform assignment of propagator momenta.
//!
//! Basic usage is
//!
//!   dynast -o outfile.yml diagrams.yml
//!
//! It is possible to pass more than one input file, for instance
//!
//!   dynast -o outfile.yml topologies.yml diagrams.yml
//!
//! to ensure the diagrams in `diagrams.yml` are mapped onto the
//! topologies defined in `topologies.yml` as far as possible.
//!
//! ## Important Options
//!
//! * Use the `-s` flag to allow mapping onto subtopologies, i.e. onto
//!   diagrams where one or more propagators have been contracted.
//!
//! * Use `-f` to switch between YAML and FORM output.
//!
//! * `--replace-masses` and `--replace-momenta` can be used for kinematic
//!   replacements, e.g. `--replace-momenta='{p1: q, p2: q}'` to set the
//!   momenta `p1` and `p2` to `q`.
//!
//! ## Input format
//!
//! ### YAML
//!
//! The input in YAML format has the form
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
//! that is not associated with a graph edge. `dynast` will ignore
//! such scalar products.
//!
//! The document separator `---` is optional but recommended for large
//! input files to save memory.
//!
//! dynast includes a
//! [QGRAF](http://cfif.ist.utl.pt/~paulo/qgraf.html) style file
//! `share/qgraf/yaml.sty` that generates the required input. If
//! several fields have the same mass the corresponding masses in the
//! QGRAF output have to be adjusted manually.
//!
//! ### FORM
//!
//! FORM input files should start with the following line:
//! ```
//! * dynast-format: FORM
//! ```
//! The remainder of the file should consist of diagram folds:
//! ```
//! *--#[ DIANAME:
//!   diagram specification
//! *--#] DIANAME:
//! ```
//! In the diagram specification, only propagator lines of the form
//! `*prop(from, to, field(fieldnr, ...))` with arbitrary extra
//! whitespace are interpreted. The ellipsis indicates any number of
//! further arguments. Only the `from`, `to`, and `field`
//! specifications are considered for the topology mapping, and
//! `field` is interpreted as a mass. Use the `--replace-masses`
//! option to replace fields by actual masses.
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
//! ### FORM
//!
//! Alternatively `-f form` to produce output for FORM. In this case,
//! *mapping information is only written for diagrams that have integer
//! numbers as names*. Then, an output file `topologies.frm` can be
//! used as follows:
//! ```FORM
//! cf dia;
//! #include- topologies.frm
//! * example diagram
//! * the argument of `dia` should match the name in the dynast input file
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
mod form_input;
mod graph_util;
mod mapper;
mod momentum;
mod momentum_mapping;
mod opt;
mod symbol;
mod version;
mod writer;
mod yaml_dias;
mod yaml_doc_iter;

use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};

use ahash::{AHashMap, RandomState};
use anyhow::{anyhow, Context, Result};
use biconnected_components::SplitIntoBcc;
use env_logger::Env;
use lazy_static::lazy_static;
use log::{debug, info, trace};
use nom::InputIter;
use petgraph::{graph::UnGraph, Graph};
use regex::bytes::Regex;

use crate::form_input::FormDiaReader;
use crate::graph_util::Format;
use crate::mapper::TopMapper;
use crate::momentum::{Momentum, Replace};
use crate::opt::Args;
use crate::symbol::Symbol;
use crate::version::VERSION_STRING;
use crate::writer::{write, write_header, write_factorising};
use crate::yaml_dias::{Diagram, EdgeWeight, NumOrString};
use crate::yaml_doc_iter::YamlDocIter;

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

fn main() -> Result<()> {
    let args = Args::parse();
    let env = Env::default().filter_or("DYNAST_LOG", &args.loglevel);
    env_logger::init_from_env(env);
    info!("{}", &*VERSION_STRING);

    if let Some(filename) = &args.outfile {
        let out = File::create(filename)
            .with_context(|| format!("Trying to create {filename:?}"))?;
        let out = BufWriter::new(out);
        write_mappings(args, out)
    } else {
        write_mappings(args, std::io::stdout())
    }
}

fn write_mappings(args: Args, mut out: impl Write) -> Result<()> {
    const BUF_SIZE: usize = 8192;

    let mut mapper = TopMapper::new();
    mapper.add_subgraphs = args.subtopologies;
    mapper.keep_duplicate = args.keep_duplicate;

    write_header(&mut out, args.format)
        .with_context(|| "Failed to write output header")?;

    for filename in &args.infiles {
        info!("Reading diagrams from {filename:?}");
        let file = File::open(filename)
            .with_context(|| format!("Failed to read {filename:?}"))?;
        let mut reader = BufReader::with_capacity(BUF_SIZE, file);
        let in_format = get_format(&mut reader)
            .with_context(|| format!("Reading from {filename:?}"))?;
        if let Err(err) = match in_format {
            InFormat::Yaml => {
                write_mappings_from_yaml(&mut mapper, reader, &mut out, &args)
            }
            InFormat::Form => {
                write_mappings_from_form(&mut mapper, reader, &mut out, &args)
            }
        } {
            return Err(err)
                .with_context(|| format!("Reading from {filename:?}"));
        }
    }
    Ok(())
}

fn write_mappings_from_yaml(
    mapper: &mut TopMapper<NumOrString>,
    reader: impl BufRead,
    mut out: impl Write,
    args: &Args,
) -> Result<()> {
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
                    return Err(err.into());
                }
            }
        };
        for (name, dia) in dias {
            write_mappings_with(mapper, name, dia, &mut out, args)?;
        }
    }
    Ok(())
}

fn write_mappings_from_form(
    mapper: &mut TopMapper<NumOrString>,
    reader: impl BufRead,
    mut out: impl Write,
    args: &Args,
) -> Result<(), anyhow::Error> {
    for item in FormDiaReader::new(reader) {
        let (name, dia) = item?;
        // TODO: code duplication
        write_mappings_with(mapper, name, dia, &mut out, args)?;
    }
    Ok(())
}

fn write_mappings_with(
    mapper: &mut TopMapper<NumOrString>,
    name: NumOrString,
    dia: Diagram,
    mut out: impl Write,
    args: &Args,
) -> Result<(), anyhow::Error> {
    debug!("Read {name}: {}", dia.format());
    let dia = replace_masses(dia, args.replace_masses());
    trace!("After replacing masses: {}", dia.format());
    let graph = Graph::try_from(dia)?;
    let graph = replace_momenta(graph, args.replace_momenta());
    if args.factors {
        let graphs = graph.split_into_bcc();
        let mut map = Vec::new();
        for (n, graph) in graphs.into_iter().enumerate() {
            let sub_map = mapper
                .map_graph(format!("{name}_subgraph{n}").into(), graph)
                .with_context(|| format!("Mapping diagram {name}, subgraph {n}"))?;
            map.push(sub_map);
        }
        write_factorising(&mut out, &name, &map, args.format)?;
    } else {
        let (topname, map) = mapper
        .map_graph(name.clone(), graph)
        .with_context(|| format!("Mapping diagram {name}"))?;
        write(&mut out, &name, &topname, &map, args.format)?;
    }
    Ok(())
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum InFormat {
    #[default]
    Yaml,
    Form,
}

lazy_static! {
    static ref FORMAT_SPEC: Regex =
        Regex::new(r"dynast-format:\s*(\w+)").unwrap();
}

fn get_format(reader: &mut impl BufRead) -> Result<InFormat> {
    let mut buf = reader.fill_buf()?;
    if let Some(line_end) = buf.position(|b| b == b'\n') {
        buf = &buf[..line_end];
    }
    if let Some(format) = FORMAT_SPEC.captures(buf) {
        let format_str = std::str::from_utf8(&format[1])?;
        let format_str = format_str.to_lowercase();
        match format_str.as_str() {
            "form" => Ok(InFormat::Form),
            "yaml" => Ok(InFormat::Yaml),
            _ => Err(anyhow!("Unknown input format: `{format_str}`")),
        }
    } else {
        Ok(Default::default())
    }
}

fn replace_masses(
    dia: Diagram,
    replace_masses: &AHashMap<NumOrString, NumOrString>,
) -> Diagram {
    use yaml_dias::Denom::*;
    let mut dens = dia.into_denominators();
    for den in &mut dens {
        match den {
            Prop(_, _, _, ref mut m) => {
                if let Some(m_new) = replace_masses.get(m).cloned() {
                    *m = m_new;
                }
            }
            Sp(_, ref mut m) => {
                if let Some(m_new) = replace_masses.get(m).cloned() {
                    *m = m_new;
                }
            }
        }
    }
    Diagram::new(dens)
}

fn replace_momenta(
    mut graph: UnGraph<Momentum, EdgeWeight>,
    replace_momenta: &AHashMap<Symbol, Momentum>,
) -> UnGraph<Momentum, EdgeWeight> {
    for p in graph.node_weights_mut() {
        *p = std::mem::take(p).replace(replace_momenta);
    }
    for p in graph.edge_weights_mut() {
        p.p = std::mem::take(&mut p.p).replace(replace_momenta);
    }
    graph
}
