use num_rational::Rational32;
use serde::{Deserialize, Serialize};
use typed_builder::TypedBuilder;

use crate::momentum::Momentum;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize, TypedBuilder)]
pub(crate) struct Diagram {
    pub(crate) prefactor: Rational32,
    pub(crate) name: String,
    pub(crate) vertices: Vec<Vertex>,
    pub(crate) propagators: Vec<Propagator>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize, TypedBuilder)]
pub(crate) struct Vertex {
    pub(crate) id: u32,
    pub(crate) fields: Vec<Field>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize, TypedBuilder)]
pub(crate) struct Propagator {
    pub(crate) from: u32,
    pub(crate) to: u32,
    pub(crate) field: Field,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize, TypedBuilder)]
pub(crate) struct Field {
    pub(crate) name: String,
    pub(crate) id: i32,
    pub(crate) momentum: Momentum,
}
