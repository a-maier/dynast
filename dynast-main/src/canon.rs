use crate::Momentum;
use derivative::Derivative;
use log::trace;
use nauty_pet::prelude::*;
use petgraph::{graph::UnGraph, Undirected};

use crate::graph_util::{
    from_nodes_edges, into_nodes_edges, transform_nodes, Format,
};
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
    trace!("Relabelled {}", g.format());
    debug_assert!(g.is_identical(&CanonGraph::from(g.clone())));
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
