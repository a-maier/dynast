use std::fmt::{self, Display};

use serde::{Deserialize, Serialize};

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
pub struct Symbol(math_symbols::Symbol);

impl Symbol {
    pub fn new(name: &str) -> Self {
        Self(math_symbols::Symbol::new(name))
    }

    pub fn name(&self) -> String {
        self.0.name()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[macro_export]
macro_rules! symbols {
    ( $( $x:ident ),* ) => {
        $(
            let $x = Symbol::new(stringify!($x));
        )*
    };
}
