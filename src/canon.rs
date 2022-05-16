use std::convert::identity;

use derivative::Derivative;
use log::trace;
use nauty_pet::prelude::*;
use petgraph::{
    graph::{IndexType, UnGraph},
    visit::EdgeRef,
    EdgeType, Graph, Undirected,
};

use crate::momentum::Momentum;
use crate::yaml_dias::EdgeWeight;

// Similar to nauty_pet's canonisation, but keep track of momentum directions
pub(crate) fn into_canon(
    g: UnGraph<Momentum, EdgeWeight>,
) -> CanonGraph<Momentum, EdgeWeight, Undirected> {
    let mut node_nr = 0;
    let canon = CanonGraph::from(transform_nodes(g.clone(), |p| {
        let n = NodeWeight { id: node_nr, p };
        node_nr += 1;
        n
    }));
    trace!("Canonically labelled: {canon:#?}");

    let mut relabel = vec![0; canon.node_count()];
    for (new_id, n) in canon.node_weights().enumerate() {
        relabel[n.id as usize] = new_id;
    }
    trace!("Relabelling: {relabel:?}");
    let g = relabel_nodes(g, relabel);
    trace!("Relabelled {g:#?}");
    debug_assert!(g.is_identical(CanonGraph::from(g.clone()).get()));
    g.into()
}

#[derive(Clone, Debug, Default, Derivative)]
#[derivative(Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct NodeWeight {
    #[derivative(
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore",
        Hash = "ignore"
    )]
    pub(crate) id: u32,
    pub(crate) p: Momentum,
}

fn apply_perm<T>(slice: &mut [T], mut new_pos: Vec<usize>) {
    const CORRECT_POS: usize = usize::MAX;
    for idx in 0..slice.len() {
        let mut next_idx = new_pos[idx];
        if next_idx == CORRECT_POS {
            continue;
        }
        while next_idx != idx {
            slice.swap(idx, next_idx);
            next_idx = std::mem::replace(&mut new_pos[next_idx], CORRECT_POS);
        }
    }
}

fn relabel_nodes(
    g: UnGraph<Momentum, EdgeWeight>,
    relabel: Vec<usize>,
) -> UnGraph<Momentum, EdgeWeight> {
    let (mut nodes, mut edges) = into_nodes_edges(g);
    for edge in &mut edges {
        edge.0 = relabel[edge.0];
        edge.1 = relabel[edge.1];
        if edge.0 > edge.1 {
            std::mem::swap(&mut edge.0, &mut edge.1);
            edge.2.p *= -1;
        }
    }
    edges.sort();
    apply_perm(&mut nodes, relabel);
    from_nodes_edges(nodes, edges)
}

fn into_nodes_edges<N, E, Ty, Ix>(
    g: Graph<N, E, Ty, Ix>,
) -> (Vec<N>, Vec<(usize, usize, E)>)
where
    Ty: EdgeType,
    Ix: IndexType,
{
    into_transformed_nodes_edges(g, identity, identity)
}

fn from_nodes_edges<N, E, Ty, Ix>(
    nodes: Vec<N>,
    edges: Vec<(usize, usize, E)>,
) -> Graph<N, E, Ty, Ix>
where
    Ty: EdgeType,
    Ix: IndexType,
{
    let mut res = Graph::with_capacity(nodes.len(), edges.len());
    for node in nodes {
        res.add_node(node);
    }
    for edge in edges {
        use petgraph::visit::NodeIndexable;
        let from = res.from_index(edge.0);
        let to = res.from_index(edge.1);
        res.add_edge(from, to, edge.2);
    }
    res
}

fn into_transformed_nodes_edges<N, E, Ty, Ix, F, G, NN, EE>(
    g: Graph<N, E, Ty, Ix>,
    mut node_transform: F,
    mut edge_transform: G,
) -> (Vec<NN>, Vec<(usize, usize, EE)>)
where
    Ty: EdgeType,
    Ix: IndexType,
    F: FnMut(N) -> NN,
    G: FnMut(E) -> EE,
{
    use petgraph::visit::NodeIndexable;
    let edges = Vec::from_iter(
        g.edge_references()
            .map(|e| (g.to_index(e.source()), g.to_index(e.target()))),
    );
    let (nodes, e) = g.into_nodes_edges();
    let nodes = nodes
        .into_iter()
        .map(|n| node_transform(n.weight))
        .collect();
    let edges = edges
        .into_iter()
        .zip(e.into_iter().map(|e| e.weight))
        .map(|((from, to), wt)| (from, to, edge_transform(wt)))
        .collect();
    (nodes, edges)
}

fn transform_nodes_edges<N, NN, E, EE, Ty, Ix, F, G>(
    g: Graph<N, E, Ty, Ix>,
    node_transform: F,
    edge_transform: G,
) -> Graph<NN, EE, Ty, Ix>
where
    Ty: EdgeType,
    Ix: IndexType,
    F: FnMut(N) -> NN,
    G: FnMut(E) -> EE,
{
    let (nodes, edges) =
        into_transformed_nodes_edges(g, node_transform, edge_transform);
    from_nodes_edges(nodes, edges)
}

fn transform_nodes<N, NN, E, Ty, Ix, F>(
    g: Graph<N, E, Ty, Ix>,
    node_transform: F,
) -> Graph<NN, E, Ty, Ix>
where
    Ty: EdgeType,
    Ix: IndexType,
    F: FnMut(N) -> NN,
{
    transform_nodes_edges(g, node_transform, identity)
}
