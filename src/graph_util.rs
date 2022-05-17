use std::fmt::{self, Display};

use num_traits::Zero;
use petgraph::{
    graph::UnGraph,
    visit::{EdgeRef, NodeIndexable},
};

use crate::momentum::Momentum;
use crate::yaml_dias::EdgeWeight;

pub(crate) trait Format<'a> {
    type Output: Display;

    fn format(&'a self) -> Self::Output;
}

impl<'a> Format<'a> for UnGraph<Momentum, EdgeWeight> {
    type Output = FormatUnGraph<'a>;

    fn format(&'a self) -> Self::Output {
        FormatUnGraph(&self)
    }
}

pub(crate) struct FormatUnGraph<'a> (
    &'a UnGraph<Momentum, EdgeWeight>
);

impl<'a> Display for FormatUnGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Graph {{
   external momenta: {{")?;
        for (n, p) in self.0.node_weights().enumerate() {
            if !p.is_zero() {
                write!(f, "{n}: {p}, ")?;
            }
        }
        writeln!(f, "}},
   propagators: [")?;
        for e in self.0.edge_references() {
            let from = self.0.to_index(e.source());
            let to = self.0.to_index(e.target());
            writeln!(f, "      [({from}, {to}), {}, {}],", e.weight().p, e.weight().m)?
        }
        writeln!(f, "   ],\n}}")
    }
}
