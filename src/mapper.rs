use std::convert::TryFrom;
use std::hash::Hash;

use ahash::RandomState;
use log::{debug, trace};
use nauty_pet::prelude::*;
use petgraph::{graph::UnGraph, visit::EdgeRef, Graph, Undirected};
use thiserror::Error;

use crate::canon::{contract_edge, into_canon};
use crate::graph_util::{contract_duplicate, Format};
use crate::momentum::Momentum;
use crate::momentum_mapping::{Mapping, MappingError};
use crate::symbol::Symbol;
use crate::yaml_dias::{Diagram, EdgeWeight, ImportError, NumOrString};

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;
type IndexSet<T> = indexmap::IndexSet<T, RandomState>;

pub type Topology = CanonGraph<Momentum, EdgeWeight, Undirected>;

#[derive(Clone, Debug)]
pub(crate) struct TopologyWithExtMom {
    pub(crate) external_momenta: IndexSet<Symbol>,
    pub(crate) graph: Topology
}

impl Hash for TopologyWithExtMom {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.graph.hash(state);
    }
}

impl PartialEq for TopologyWithExtMom {
    fn eq(&self, other: &Self) -> bool {
        self.graph == other.graph
    }
}

impl Eq for TopologyWithExtMom { }

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TopMapper {
    seen: IndexMap<TopologyWithExtMom, NumOrString>,
    pub add_subgraphs: bool,
    pub keep_duplicate: bool,
}

impl TopMapper {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_topologies(self) -> impl Iterator<Item = (Topology, NumOrString)> {
        self.seen.into_iter().map(|(t, n)| (t.graph, n))
    }

    pub fn map_dia(
        &mut self,
        name: NumOrString,
        dia: Diagram,
    ) -> Result<(NumOrString, Mapping), TopMapError> {
        debug!("Mapping diagram {name}: {}", dia.format());
        let graph = Graph::try_from(dia)?;

        self.map_graph(name, graph)
    }

    pub fn try_map_dia(
        &self,
        dia: Diagram,
    ) -> Result<Option<(NumOrString, Mapping)>, TopMapError> {
        debug!("Trying to map diagram {}", dia.format());
        let graph = Graph::try_from(dia)?;

        self.try_map_graph(graph)
    }

    // TODO: borrow checker complains if we return a `&NumOrString`
    pub fn map_graph(
        &mut self,
        name: NumOrString,
        graph: UnGraph<Momentum, EdgeWeight>,
    ) -> Result<(NumOrString, Mapping), TopMapError> {
        debug!("Mapping graph {name}: {}", graph.format());
        let graph = if self.keep_duplicate {
            graph
        } else {
            contract_duplicate(graph)
        };
        let graph = into_canon(graph);
        let external_momenta = extract_external_momenta(graph.get());
        let canon = TopologyWithExtMom{ graph, external_momenta };
        trace!("Canonical form of {name}: {}", canon.graph.get().format());

        if let Some((target, topname)) = self.seen.get_key_value(&canon) {
            debug!("{name} is {topname}");
            let map = Mapping::new(&canon, target)?;
            return Ok((topname.clone(), map));
        };

        debug!("{name} is a new topology");
        let map = Mapping::identity(&canon);
        if self.add_subgraphs {
            self.insert_subgraphs(canon, name.clone())
        } else {
            self.seen.insert(canon, name.clone());
        }
        Ok((name, map))
    }


    pub fn try_map_graph(
        &self,
        graph: UnGraph<Momentum, EdgeWeight>
    ) -> Result<Option<(NumOrString, Mapping)>, TopMapError> {
        debug!("Trying to map graph {}", graph.format());
        let graph = contract_duplicate(graph);
        let graph = into_canon(graph);
        let external_momenta = extract_external_momenta(graph.get());
        let canon = TopologyWithExtMom{ graph, external_momenta };
        if let Some((target, topname)) = self.seen.get_key_value(&canon) {
            debug!("graph is {topname}");
            let map = Mapping::new(&canon, target)?;
            Ok(Some((topname.clone(), map)))
        } else {
            Ok(None)
        }
    }

    fn insert_subgraphs(
        &mut self,
        top: TopologyWithExtMom,
        name: NumOrString,
    ) {
        trace!("Inserting {}", top.graph.get().format());
        let contractible_edges =
            top.graph.edge_references().enumerate().filter_map(|(e, edge)| {
                if edge.source() != edge.target() {
                    Some(e)
                } else {
                    None
                }
            });
        for edge in contractible_edges.rev() {
            trace!("Contracting edge {edge} of {}", top.graph.get().format());
            let subgraph = contract_edge(top.graph.clone(), edge);
            let subgraph = TopologyWithExtMom {
                external_momenta: top.external_momenta.clone(),
                graph: subgraph
            };
            if !self.seen.contains_key(&subgraph) {
                self.insert_subgraphs(subgraph, name.clone())
            }
        }
        self.seen.insert(top, name);
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum TopMapError {
    #[error("Failed to convert to graph: {0}")]
    ConvError(#[from] ImportError),
    #[error("Failed to map onto topology: {0}")]
    MapError(#[from] MappingError),
}

fn extract_external_momenta<E>(g: &UnGraph<Momentum, E>) -> IndexSet<Symbol> {
    let mut res = IndexSet::default();
    for p in g.node_weights() {
        for term in p.terms() {
            res.insert(term.symbol());
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml_dias::Denom::Prop;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn map_simple() {
        use NumOrString::*;

        log_init();

        let mut mapper = TopMapper::default();

        let denominators = vec![
            Prop(2, 1, String("l1".to_owned()), Num(0)),
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));

        let denominators = vec![
            Prop(1, 2, String("-l1".to_owned()), Num(0)),
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(2), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));

        let denominators = vec![Prop(1, 1, String("l1".to_owned()), Num(0))];
        let res = mapper.map_dia(Num(3), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(3));
    }

    #[test]
    fn map_sub() {
        use NumOrString::*;
        log_init();

        let mut mapper = TopMapper::default();
        mapper.add_subgraphs = true;
        let denominators = vec![
            Prop(2, 1, String("l1".to_owned()), Num(0)),
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));

        let denominators = vec![Prop(1, 1, String("-l1".to_owned()), Num(0))];
        let res = mapper.map_dia(Num(2), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));

        let denominators = vec![Prop(1, 1, String("l1-q".to_owned()), Num(0))];
        let res = mapper.map_dia(Num(3), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));
    }

    #[test]
    fn map_2l() {
        use NumOrString::*;
        log_init();

        let mut mapper = TopMapper::default();
        mapper.add_subgraphs = true;
        let denominators = vec![
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
            Prop(2, 3, String("l2+q".to_owned()), Num(0)),
            Prop(3, 4, String("l2".to_owned()), Num(0)),
            Prop(4, 1, String("l1".to_owned()), Num(0)),
            Prop(2, 4, String("l1-l2".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(1), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));
        mapper.add_subgraphs = false;

        let denominators = vec![
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
            Prop(1, 3, String("l2".to_owned()), Num(0)),
            Prop(3, 1, String("l1+l2".to_owned()), Num(0)),
            Prop(2, 3, String("l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(2), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));

        let denominators = vec![
            Prop(1, 2, String("l1+q".to_owned()), Num(0)),
            Prop(2, 3, String("l1".to_owned()), Num(0)),
            Prop(3, 4, String("l1+l2".to_owned()), Num(0)),
            Prop(4, 3, String("l2".to_owned()), Num(0)),
            Prop(4, 1, String("l1".to_owned()), Num(0)),
        ];
        let res = mapper.map_dia(Num(3), Diagram::new(denominators)).unwrap();
        assert_eq!(res.0, Num(1));
    }
}
