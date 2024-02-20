use std::{io::BufRead, num::ParseIntError};

use lazy_static::lazy_static;
use log::trace;
use regex::Regex;
use thiserror::Error;

use crate::yaml_dias::{Denom::Prop, Diagram, NumOrString};

#[derive(Clone, Debug, Default)]
pub(crate) struct FormDiaReader<R> {
    reader: R,
}

impl<R: BufRead> FormDiaReader<R> {
    pub(crate) fn new(reader: R) -> Self {
        Self { reader }
    }

    fn read_dia(
        &mut self,
        name: String,
        line: &mut String,
    ) -> Result<(NumOrString, Diagram), FormReadError> {
        line.clear();
        let mut props = Vec::new();
        loop {
            let read = self.reader.read_line(line);
            match read {
                Err(err) => return Err(err.into()),
                Ok(0) => return Err(FormReadError::UnfinishedDia(name)),
                _ => {}
            };
            if FORM_FOLD_END.is_match(line) {
                let name = if let Ok(num) = name.parse() {
                    NumOrString::Num(num)
                } else {
                    NumOrString::String(name)
                };
                let dia = Diagram::new(props);
                return Ok((name, dia));
            }
            if !FORM_COMMENT_LINE.is_match(line) {
                if let Some(caps) = FORM_PROP.captures(line) {
                    let from = caps[1].parse()?;
                    let to = caps[2].parse()?;
                    let p = NumOrString::String(caps[4].to_string());
                    let m = NumOrString::String(caps[3].to_string());
                    props.push(Prop(from, to, p, m));
                }
            }
            line.clear()
        }
    }
}

lazy_static! {
    static ref FORM_FOLD_START: Regex = Regex::new(r"^\*--#\[\s+(\w+):").unwrap();
    static ref FORM_FOLD_END: Regex = Regex::new(r"^\*--#\]\s+(\w+):").unwrap();
    static ref FORM_COMMENT_LINE: Regex = Regex::new(r"^\*").unwrap();
    static ref FORM_PROP: Regex = Regex::new(r"^\s*\*\s*prop\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\w+)\(\s*\d+\s*,\s*([^,\)]*)\s*\)\s*\)").unwrap();
}

impl<R: BufRead> Iterator for FormDiaReader<R> {
    type Item = Result<(NumOrString, Diagram), FormReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut line = String::new();
        loop {
            let read = self.reader.read_line(&mut line);
            match read {
                Err(err) => return Some(Err(err.into())),
                Ok(0) => return None,
                _ => {}
            };
            if let Some(caps) = FORM_FOLD_START.captures(&line) {
                let name = caps[1].to_string();
                trace!("Reading FORM diagram {name}");
                return Some(self.read_dia(name, &mut line));
            }
            line.clear();
        }
    }
}

#[derive(Debug, Error)]
pub enum FormReadError {
    #[error("Failed to read FORM diagram: {0}")]
    IOErr(#[from] std::io::Error),
    #[error("Failed to parse vertex number in FORM propagator: {0}")]
    ParseVxNumErr(#[from] ParseIntError),
    #[error("Reached end of file while reading FORM diagram `{0}`")]
    UnfinishedDia(String),
}
