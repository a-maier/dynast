use std::convert::TryFrom;

use ahash::RandomState;
use log::{debug, trace};
use nauty_pet::prelude::*;
use petgraph::{graph::UnGraph, Graph, Undirected, visit::EdgeRef};
use thiserror::Error;

use crate::canon::{contract_edge, into_canon};
use crate::graph_util::{Format};
use crate::momentum::Momentum;
use crate::momentum_mapping::{Mapping, MappingError};
use crate::yaml_dias::{Diagram, EdgeWeight, ImportError, NumOrString};

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct TopMapper {
    seen: IndexMap<CanonGraph<Momentum, EdgeWeight, Undirected>, NumOrString>,
    pub(crate) add_subgraphs: bool
}

impl TopMapper {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn map_dia(
        &mut self,
        name: NumOrString,
        dia: Diagram,
    ) -> Result<(NumOrString, Mapping), TopMapError> {
        debug!("Mapping diagram {name}: {}", dia.format());
        let graph = Graph::try_from(dia)?;

        self.map_graph(name, graph)
    }

    // TODO: borrow checker complains if we return a `&NumOrString`
    pub(crate) fn map_graph(
        &mut self,
        name: NumOrString,
        graph: UnGraph<Momentum, EdgeWeight>,
    ) -> Result<(NumOrString, Mapping), TopMapError> {
        debug!("Mapping graph {name}: {}", graph.format());
        let canon = into_canon(graph);

        if let Some((target, topname)) = self.seen.get_key_value(&canon) {
            debug!("{name} is {topname}");
            let map = Mapping::new(canon.get(), target.get())?;
            return Ok((topname.clone(), map));
        };

        debug!("{name} is a new topology");
        let map = Mapping::identity(canon.get());
        if self.add_subgraphs {
            self.insert_subgraphs(canon, name.clone())
        } else {
            self.seen.insert(canon, name.clone());
        }
        Ok((name, map))
    }

    fn insert_subgraphs(
        &mut self,
        graph: CanonGraph<Momentum, EdgeWeight, Undirected>,
        name: NumOrString,
    ) {
        trace!("Inserting {}", graph.get().format());
        let contractible_edges = graph.edge_references().enumerate()
            .filter_map(
                |(e, edge)| if edge.source() != edge.target() {
                    Some(e)
                } else {
                    None
                }
            );
        for edge in contractible_edges {
            trace!("Contracting edge {edge} of {}", graph.get().format());
            let subgraph = contract_edge(graph.clone(), edge);
            if !self.seen.contains_key(&subgraph) {
                self.insert_subgraphs(subgraph, name.clone())
            }
        }
        self.seen.insert(graph, name);
    }
}


#[derive(Debug, Error)]
pub(crate) enum TopMapError{
    #[error("Failed to convert to graph: {0}")]
    ConvError(#[from] ImportError),
    #[error("Failed to map onto topology: {0}")]
    MapError(#[from] MappingError)

}

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn map_simple() {
        use NumOrString::*;
        log_init();

        let mut mapper = TopMapper::default();

        let propagators = vec![
            (2, 1, String("l1".to_owned()), Num(0)),
            (1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));

        let propagators = vec![
            (1, 2, String("-l1".to_owned()), Num(0)),
            (1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(2), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));

        let propagators = vec![
            (1, 1, String("l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(3), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(3));
    }

    #[test]
    fn map_sub() {
        use NumOrString::*;
        log_init();

        let mut mapper = TopMapper::default();
        mapper.add_subgraphs = true;
        let propagators = vec![
            (2, 1, String("l1".to_owned()), Num(0)),
            (1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));

        let propagators = vec![
            (1, 1, String("-l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(2), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));
    }

    #[test]
    fn map_2l() {
        use NumOrString::*;
        log_init();

        let mut mapper = TopMapper::default();
        mapper.add_subgraphs = true;
        let propagators = vec![
            (1, 2, String("l1+q".to_owned()), Num(0)),
            (2, 3, String("l2+q".to_owned()), Num(0)),
            (3, 4, String("l2".to_owned()), Num(0)),
            (4, 1, String("l1".to_owned()), Num(0)),
            (2, 4, String("l1-l2".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));
        mapper.add_subgraphs = false;

        let propagators = vec![
            (1, 2, String("l1+q".to_owned()), Num(0)),
            (1, 3, String("l2".to_owned()), Num(0)),
            (3, 1, String("l1+l2".to_owned()), Num(0)),
            (2, 3, String("l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(2), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));

        let propagators = vec![
            (1, 2, String("l1+q".to_owned()), Num(0)),
            (2, 3, String("l1".to_owned()), Num(0)),
            (3, 4, String("l1+l2".to_owned()), Num(0)),
            (4, 3, String("l2".to_owned()), Num(0)),
            (4, 1, String("l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(3), Diagram { propagators }).unwrap();
        assert_eq!(res.0, Num(1));
    }

}
