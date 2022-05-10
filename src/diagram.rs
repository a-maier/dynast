use num_rational::Rational32;
use serde::{Deserialize, Serialize};

use crate::momentum::Momentum;

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize)]
pub(crate) struct Diagram {
    pub(crate) vertices: Vec<Vertex>,
    pub(crate) propagators: Vec<Propagator>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize)]
pub(crate) struct Vertex {
    pub(crate) id: u32,
    pub(crate) momentum: Momentum,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize)]
pub(crate) struct Propagator {
    pub(crate) from: u32,
    pub(crate) to: u32,
    pub(crate) momentum: Momentum,
    pub(crate) mass: String,
}
