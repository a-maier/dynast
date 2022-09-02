use std::collections::HashMap;
use std::fmt::{self, Display};
use std::sync::RwLock;

use lazy_static::lazy_static;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct SymbolRegister {
    names: Vec<String>,
    indices: HashMap<String, usize>,
}

impl SymbolRegister {
    fn name(&self, idx: usize) -> &str {
        &self.names[idx]
    }

    fn try_idx(&self, name: &str) -> Option<usize> {
        self.indices.get(name).copied()
    }

    fn idx(&mut self, name: &str) -> usize {
        if let Some(idx) = self.try_idx(name) {
            return idx;
        }
        let new_idx = self.names.len();
        self.indices.insert(String::from(name), new_idx);
        self.names.push(String::from(name));
        new_idx
    }
}

lazy_static! {
    static ref SYMBOL_REGISTER: RwLock<SymbolRegister> =
        RwLock::new(SymbolRegister::default());
}

#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Deserialize,
    Serialize,
)]
#[serde(transparent)]
pub struct Symbol {
    #[serde(
        serialize_with = "serialize_sym",
        deserialize_with = "deserialize_sym"
    )]
    idx: usize,
}

impl Symbol {
    pub fn new_unchecked(name: &str) -> Self {
        if let Some(idx) = SYMBOL_REGISTER.read().unwrap().try_idx(name) {
            return Self { idx };
        }
        let idx = SYMBOL_REGISTER.write().unwrap().idx(name);
        Self { idx }
    }

    pub fn name(&self) -> String {
        SYMBOL_REGISTER.read().unwrap().name(self.idx).to_owned()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", SYMBOL_REGISTER.read().unwrap().name(self.idx))
    }
}

fn serialize_sym<S: Serializer>(sym: &usize, s: S) -> Result<S::Ok, S::Error> {
    let sym = Symbol { idx: *sym };
    let name = sym.name();
    String::serialize(&name, s)
}

fn deserialize_sym<'de, D: Deserializer<'de>>(d: D) -> Result<usize, D::Error> {
    let name = String::deserialize(d)?;
    let s = Symbol::new_unchecked(&name);
    Ok(s.idx)
}

#[macro_export]
macro_rules! symbols {
    ( $( $x:ident ),* ) => {
        $(
            let $x = Symbol::new_unchecked(stringify!($x));
        )*
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol() {
        symbols!(x, y, z);
        assert_eq!(x.name(), "x");
        assert_eq!(y.name(), "y");
        assert_eq!(z.name(), "z");
        let xx = Symbol::new_unchecked("x");
        assert_eq!(xx.name(), "x");
    }
}
