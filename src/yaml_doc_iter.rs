use std::io::{BufRead, Read, Result};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct YamlDocIter<T> {
    reader: T
}

impl<T: BufRead> YamlDocIter<T> {
    pub(crate) fn new(reader: T) -> Self {
        Self { reader }
    }
}

impl<T: BufRead> Iterator for YamlDocIter<T> {
    type Item = Result<Vec<u8>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = Vec::new();
        match read_yaml_document(&mut self.reader, &mut buf) {
            Ok(0) => None,
            Ok(_) => Some(Ok(buf)),
            Err(err) => Some(Err(err))
        }
    }
}

const YAML_DOC_SEP: &[u8] = b"---";

fn read_yaml_document(
    reader: &mut impl BufRead,
    buf: &mut Vec<u8>
) -> std::io::Result<usize> {
    let mut nread = 0;
    loop {
        nread += reader.read_until(b'-', buf)?;
        if buf.last() != Some(&b'-') {
            return Ok(nread);
        }
        nread += reader.take(2).read_to_end(buf)?;
        if buf.ends_with(YAML_DOC_SEP) && !ends_inside_yaml_comment(buf) {
            buf.truncate(buf.len() - YAML_DOC_SEP.len());
            return Ok(nread)
        }
    }
}

fn ends_inside_yaml_comment(buf: &[u8]) -> bool {
    let last_comment_or_newline = buf.iter()
        .rposition(|&c| c == b'\n' || c == b'#');
    if let Some(idx) = last_comment_or_newline {
        buf[idx] == b'#'
    } else {
        false
    }
}
