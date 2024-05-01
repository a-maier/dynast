// TODO: proper error types
use ahash::RandomState;
use anyhow::{anyhow, Context, Result};
use lazy_static::lazy_static;
use log::{info, trace};
use nom::InputIter;
use regex::bytes::Regex;

use std::{fs::File, io::{BufRead, BufReader}, path::{Path, PathBuf}};

use crate::{form_input::FormDiaReader, yaml_dias::{Diagram, NumOrString}, yaml_doc_iter::YamlDocIter};

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

#[derive(Debug, Default)]
pub(crate) struct DiaFileIter {
    files: Vec<PathBuf>,
    current: Option<DiaReader<BufReader<File>>>,
}

impl DiaFileIter {
    pub(crate) fn try_from_files(mut files: Vec<PathBuf>) -> Result<Self> {
        files.reverse();
        let current = files.last().map(DiaReader::try_new).transpose()?;
        Ok(Self {
            files,
            current,
        })
    }

    fn next_file(&mut self) -> Result<()> {
        self.files.pop();
        self.current = self.files.last().map(DiaReader::try_new).transpose()?;
        Ok(())
    }
}

impl Iterator for DiaFileIter {
    type Item = Result<(NumOrString, Diagram)>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(inner) = self.current.as_mut() else {
            return None
        };
        let next = inner.next();
        if next.is_some() {
            return next.map(|n| n.with_context(
                || format!("Reading from {:?}", self.files.last().unwrap())
            ));
        };
        if let Err(err) = self.next_file() {
            return Some(Err(err))
        }
        self.next()
    }
}

#[derive(Debug)]
enum DiaReader<R> {
    Form(FormDiaReader<R>),
    Yaml(YamlReader<R>),
}

impl<R: BufRead> Iterator for DiaReader<R> {
    type Item = Result<(NumOrString, Diagram)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            DiaReader::Form(r) => r.next().map(|n| n.map_err(|e| e.into())),
            DiaReader::Yaml(r) => r.next(),
        }
    }
}

impl DiaReader<BufReader<File>> {
    fn try_new<P: AsRef<Path>>(from: P) -> Result<Self> {
        const BUF_SIZE: usize = 8192;

        let filename = from.as_ref();
        info!("Reading diagrams from {filename:?}");
        let file = File::open(filename)
            .with_context(|| format!("Failed to read {filename:?}"))?;
        let mut reader = BufReader::with_capacity(BUF_SIZE, file);
        let in_format = get_format(&mut reader)
            .with_context(|| format!("Reading from {filename:?}"))?;
        let reader = match in_format {
            InFormat::Yaml => Self::Yaml(YamlReader::new(reader)),
            InFormat::Form => Self::Form(FormDiaReader::new(reader)),
        };
        Ok(reader)
    }
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

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct YamlReader<R> {
    reader: YamlDocIter<R>,
    dia_cached: Vec<(NumOrString, Diagram)>,
}

impl<R: BufRead> Iterator for YamlReader<R> {
    type Item = Result<(NumOrString, Diagram)>;

    fn next(&mut self) -> Option<Self::Item> {
        let from_cache = self.dia_cached.pop();
        if from_cache.is_some() {
            return from_cache.map(Ok);
        }
        let next_document = self.reader.next()?;
        let document = match next_document {
            Ok(doc) => doc,
            Err(err) => return Some(Err(err.into())),
        };
        trace!(
            "yaml document:\n{}",
            std::str::from_utf8(&document).unwrap()
        );
        let dias: Result<IndexMap<NumOrString, Diagram>, _> =
            serde_yaml::from_slice(&document);
        let dias = match dias {
            Ok(dias) => dias,
            Err(err) => return Some(Err(err.into())),
        };
        self.dia_cached = dias.into_iter().collect();
        self.dia_cached.reverse();
        self.next()
    }
}

impl YamlReader<BufReader<File>> {
    fn new(reader: BufReader<File>) -> Self {
        let reader = YamlDocIter::new(reader);
        let dia_cached = Vec::new();
        Self {
            reader,
            dia_cached,
        }
    }
}
