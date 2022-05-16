use std::convert::TryFrom;

use ahash::RandomState;
use log::trace;
use nauty_pet::prelude::*;
use petgraph::{Graph, Undirected};
use thiserror::Error;

use crate::canon::into_canon;
use crate::momentum::Momentum;
use crate::momentum_mapping::{Mapping, MappingError};
use crate::yaml_dias::{Diagram, EdgeWeight, ImportError, NumOrString};

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct TopMapper {
    seen: IndexMap<CanonGraph<Momentum, EdgeWeight, Undirected>, NumOrString>,
}

impl TopMapper {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    // TODO: borrow checker complains if we return a `&NumOrString`
    pub(crate) fn map_dia(
        &mut self,
        name: NumOrString,
        dia: Diagram,
    ) -> Result<(NumOrString, Mapping), TopMapError> {
        let graph = Graph::try_from(dia)?;
        trace!("Graph {graph:#?}");

        let canon = into_canon(graph);

        if let Some((target, topname)) = self.seen.get_key_value(&canon) {
            let map = Mapping::new(canon.get(), target.get())?;
            return Ok((topname.clone(), map));
        };

        let map = Mapping::identity(canon.get());
        self.seen.insert(canon, name.clone());
        Ok((name, map))
    }
}

#[derive(Debug, Error)]
pub(crate) enum TopMapError{
    #[error("Failed to convert to graph: {0}")]
    ConvError(#[from] ImportError),
    #[error("Failed to map onto topology: {0}")]
    MapError(#[from] MappingError)

}
