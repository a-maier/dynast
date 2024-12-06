use std::fmt::{self, Display};

#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde-1", serde(transparent))]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
            let $x = $crate::symbol::Symbol::new(stringify!($x));
        )*
    };
}
