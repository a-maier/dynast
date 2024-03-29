use std::convert::TryFrom;
use std::fmt::Display;
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
use crate::yaml_dias::{Diagram, EdgeWeight, ImportError};

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;
type IndexSet<T> = indexmap::IndexSet<T, RandomState>;

pub type Topology = CanonGraph<Momentum, EdgeWeight, Undirected>;

#[derive(Clone, Debug)]
pub(crate) struct TopologyWithExtMom {
    pub(crate) external_momenta: IndexSet<Symbol>,
    pub(crate) graph: Topology,
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

impl Eq for TopologyWithExtMom {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TopMapper<ID> {
    seen: IndexMap<TopologyWithExtMom, ID>,
    pub add_subgraphs: bool,
    pub keep_duplicate: bool,
}

impl<ID> Default for TopMapper<ID> {
    fn default() -> Self {
        Self {
            seen: Default::default(),
            add_subgraphs: Default::default(),
            keep_duplicate: Default::default(),
        }
    }
}

impl<ID: Clone + Display> TopMapper<ID> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_topologies(self) -> impl Iterator<Item = (Topology, ID)> {
        self.seen.into_iter().map(|(t, n)| (t.graph, n))
    }

    pub fn map_dia(
        &mut self,
        name: ID,
        dia: Diagram,
    ) -> Result<(ID, Mapping), TopMapError> {
        debug!("Mapping diagram {name}: {}", dia.format());
        let graph = Graph::try_from(dia)?;

        self.map_graph(name, graph)
    }

    pub fn try_map_dia(
        &self,
        dia: Diagram,
    ) -> Result<Option<(ID, Mapping)>, TopMapError> {
        debug!("Trying to map diagram {}", dia.format());
        let graph = Graph::try_from(dia)?;

        self.try_map_graph(graph)
    }

    // TODO: borrow checker complains if we return a `&NumOrString`
    pub fn map_graph(
        &mut self,
        name: ID,
        graph: UnGraph<Momentum, EdgeWeight>,
    ) -> Result<(ID, Mapping), TopMapError> {
        let ext_momenta = extract_external_momenta(&graph);
        self.map_graph_with_ext(name, graph, ext_momenta)
    }

    pub fn map_graph_with_ext(
        &mut self,
        name: ID,
        graph: UnGraph<Momentum, EdgeWeight>,
        external_momenta: IndexSet<Symbol>,
    ) -> Result<(ID, Mapping), TopMapError> {
        let (res, canon) =
            self.try_map_graph_with_ext_helper(graph, external_momenta)?;
        if let Some(res) = res {
            Ok(res)
        } else {
            debug!("{name} is a new topology");
            let map = Mapping::identity(&canon);
            if self.add_subgraphs {
                self.insert_subgraphs(canon, name.clone())
            } else {
                self.seen.insert(canon, name.clone());
            }
            Ok((name, map))
        }
    }

    pub fn try_map_graph(
        &self,
        graph: UnGraph<Momentum, EdgeWeight>,
    ) -> Result<Option<(ID, Mapping)>, TopMapError> {
        let ext_momenta = extract_external_momenta(&graph);
        self.try_map_graph_with_ext(graph, ext_momenta)
    }

    pub fn try_map_graph_with_ext(
        &self,
        graph: UnGraph<Momentum, EdgeWeight>,
        external_momenta: IndexSet<Symbol>,
    ) -> Result<Option<(ID, Mapping)>, TopMapError> {
        let (res, _) =
            self.try_map_graph_with_ext_helper(graph, external_momenta)?;
        Ok(res)
    }

    // like `try_map_graph_with_ext`, but also returns the canonised graph
    fn try_map_graph_with_ext_helper(
        &self,
        graph: UnGraph<Momentum, EdgeWeight>,
        external_momenta: IndexSet<Symbol>,
    ) -> Result<(Option<(ID, Mapping)>, TopologyWithExtMom), TopMapError> {
        debug!("Trying to map graph {}", graph.format());
        let graph = if self.keep_duplicate {
            graph
        } else {
            contract_duplicate(graph)
        };
        let graph = into_canon(graph);
        let canon = TopologyWithExtMom {
            graph,
            external_momenta,
        };
        trace!("Canonical form of graph: {}", canon.graph.format());

        if let Some((target, topname)) = self.seen.get_key_value(&canon) {
            debug!("graph is {topname}");
            let map = Mapping::new(&canon, target)?;
            return Ok((Some((topname.clone(), map)), canon));
        }

        // try again with reversed external momenta
        let TopologyWithExtMom {
            external_momenta,
            graph,
        } = canon.clone();
        let mut graph = UnGraph::from(graph);
        for p in graph.node_weights_mut() {
            *p = -std::mem::take(p);
        }
        graph.reverse();
        let graph = into_canon(graph);
        let canon2 = TopologyWithExtMom {
            graph,
            external_momenta,
        };
        if let Some((target, topname)) = self.seen.get_key_value(&canon2) {
            debug!("graph is {topname}");
            let map = Mapping::new(&canon2, target)?;
            Ok((Some((topname.clone(), map)), canon))
        } else {
            Ok((None, canon))
        }
    }

    fn insert_subgraphs(&mut self, top: TopologyWithExtMom, name: ID) {
        trace!("Inserting {}", top.graph.format());
        let contractible_edges = top
            .graph
            .edge_references()
            .enumerate()
            .filter_map(|(e, edge)| {
                if edge.source() != edge.target() {
                    Some(e)
                } else {
                    None
                }
            });
        for edge in contractible_edges.rev() {
            trace!("Contracting edge {edge} of {}", top.graph.format());
            let subgraph = contract_edge(top.graph.clone(), edge);
            let subgraph = TopologyWithExtMom {
                external_momenta: top.external_momenta.clone(),
                graph: subgraph,
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

pub fn extract_external_momenta<E>(
    g: &UnGraph<Momentum, E>,
) -> IndexSet<Symbol> {
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
    use crate::yaml_dias::NumOrString::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn map_simple() {
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
