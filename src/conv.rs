use ahash::AHashMap;
use nauty_pet::prelude::*;
use petgraph::{
    graph::UnGraph,
    Undirected
};
use thiserror::Error;

use crate::diagram::Diagram;
use crate::momentum::Momentum;

pub(crate) type Mass = i32;

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub(crate) struct GraphConv {
    masses: AHashMap<String, Mass>,
}

#[derive(Debug, Error)]
pub(crate) enum ConversionError {
    #[error("Field {0} has unknown mass")]
    UnknownMass(String)
}

impl GraphConv {
    pub(crate) fn new(masses: AHashMap<String, i32>) -> Self {
        Self { masses }
    }

    pub(crate) fn to_petgraph(
        &self,
        dia: &Diagram
    ) -> Result<CanonGraph<Momentum, Mass, Undirected>, ConversionError> {
        use ConversionError::*;
        use petgraph::visit::NodeIndexable;

        let mut res = UnGraph::with_capacity(
            dia.vertices.len(),
            dia.propagators.len(),
        );
        let highest_vx_id = dia.vertices.iter()
            .map(|vx| vx.id)
            .max()
            .unwrap_or(0);
        for _ in 0..=highest_vx_id {
            res.add_node(Momentum::zero());
        }
        // TODO: filter out duplicate propagators (same momentum)
        for prop in &dia.propagators {
            let name = &prop.field.name;
            if let Some(&mass) = self.masses.get(name) {
                let from = res.from_index(prop.from as usize);
                let to = res.from_index(prop.to as usize);
                res.add_edge(from, to, mass);
            } else {
                return Err(UnknownMass(name.to_owned()));
            }
        }
        for vx in &dia.vertices {
            let p_in = vx.fields.iter()
                .filter(|field| field.id < 0)
                .fold(
                    Momentum::zero(),
                    |p_in, f| p_in + &f.momentum
                );
            let v = res.from_index(vx.id as usize);
            *res.node_weight_mut(v).unwrap() = p_in;
        }
        Ok(res.into())
    }
}
