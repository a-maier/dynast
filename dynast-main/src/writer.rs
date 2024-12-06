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

pub(crate) fn write_factorising(
    mut out: impl Write,
    name: &NumOrString,
    map: &[(NumOrString, Mapping)],
    format: OutFormat,
) -> Result<(), Error> {
    use OutFormat::*;
    match format {
        Form => {
            if let NumOrString::Num(dia) = name {
                write!(
                    out,
                    "fill topology({dia}) = top({})",
                    map.iter().map(|(n, _)| n).join(", ")
                )?;
                for (n, map) in map.iter().map(|(_, m)| m).enumerate() {
                    if map.0.is_empty() {
                        write!(out, "*replace({n})")?;
                    } else {
                        write!(
                            out,
                            "*replace({n}, {})",
                            map.0
                                .iter()
                                .map(|(s, p)| format!("{s}, {p}"))
                                .join(", ")
                        )?;
                    }
                }
                writeln!(out, ";")
            } else {
                Ok(())
            }
        }
        Yaml => {
            writeln!(out, "{name}:")?;
            for (top, map) in map {
                writeln!(out, " - [{top}, {map}]")?
            }
            Ok(())
        }
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