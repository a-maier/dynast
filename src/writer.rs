use std::io::{Error, Write};

use clap::ValueEnum;
use itertools::Itertools;

use crate::momentum_mapping::Mapping;
use crate::version::VERSION_STRING;
use crate::yaml_dias::NumOrString;

#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, ValueEnum,
)]
pub(crate) enum OutFormat {
    Form,
    Yaml,
}

pub(crate) fn write(
    mut out: impl Write,
    name: &NumOrString,
    top: &NumOrString,
    map: &Mapping,
    format: OutFormat,
) -> Result<(), Error> {
    use OutFormat::*;
    match format {
        Form => {
            if let NumOrString::Num(dia) = name {
                writeln!(
                    out,
                    "fill topology({dia}) = top({top})*replace({});",
                    map.0.iter().map(|(s, p)| format!("{s}, {p}")).join(", ")
                )
            } else {
                Ok(())
            }
        }
        Yaml => writeln!(out, "{name}: [{top}, {map}]"),
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
            "* generated by {}
cf top, replace;
table,sparse topology(1);",
            &*VERSION_STRING
        ),
        Yaml => writeln!(out, "# generated by {}", &*VERSION_STRING),
    }
}
