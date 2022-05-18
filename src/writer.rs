use std::fmt::Display;
use std::io::{Error, Write};

use clap::ArgEnum;
use itertools::Itertools;

use crate::momentum_mapping::Mapping;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, ArgEnum)]
pub(crate) enum OutFormat {
    Form,
    Yaml,
}

pub(crate) fn write(
    mut out: impl Write,
    name: impl Display,
    top: impl Display,
    map: &Mapping,
    format: OutFormat,
) -> Result<(), Error> {
    use OutFormat::*;
    match format {
        Form => writeln!(
            out,
            "fill topology({name}) = top({top})*replace({});",
            map.map.iter().map(|(s, p)| format!("{s}, {p}")).join(", ")
        ),
        Yaml => writeln!(out, "{name}: [{top}, {map}]")
    }
}

pub(crate) fn write_header(
    mut out: impl Write,
    format: OutFormat,
) -> Result<(), Error> {
    use OutFormat::*;
    match format {
        Form => writeln!(
            out,
            "cf top, replace;
table,sparse topology(1);"
        ),
        Yaml => Ok(())
    }
}
