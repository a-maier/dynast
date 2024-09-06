use std::borrow::Cow;
use std::cmp::Ordering;
use std::convert::identity;
use std::fmt::{self, Display};
use std::ops::AddAssign;

use ahash::{AHashSet, AHashMap};
use num_traits::Zero;
use petgraph::prelude::NodeIndex;
use petgraph::{
    graph::{IndexType, UnGraph},
    visit::{EdgeRef, NodeIndexable},
    EdgeType, Graph,
};

use crate::momentum::Momentum;
use crate::yaml_dias::EdgeWeight;

pub trait Format<'a> {
    type Output: Display;

    fn format(&'a self) -> Self::Output;
}

impl<'a> Format<'a> for UnGraph<Momentum, EdgeWeight> {
    type Output = FormatUnGraph<'a>;

    fn format(&'a self) -> Self::Output {
        FormatUnGraph(self)
    }
}

pub struct FormatUnGraph<'a>(&'a UnGraph<Momentum, EdgeWeight>);

impl<'a> Display for FormatUnGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Graph {{
   external momenta: {{"
        )?;
        for (n, p) in self.0.node_weights().enumerate() {
            if !p.is_zero() {
                write!(f, "{n}: {p}, ")?;
            }
        }
        writeln!(
            f,
            "}},
   propagators: ["
        )?;
        for e in self.0.edge_references() {
            let from = self.0.to_index(e.source());
            let to = self.0.to_index(e.target());
            writeln!(
                f,
                "      [({from}, {to}), {}, {}],",
                e.weight().p,
                e.weight().m
            )?
        }
        writeln!(f, "   ],\n}}")
    }
}

pub fn into_nodes_edges<N, E, Ty, Ix>(
    g: Graph<N, E, Ty, Ix>,
) -> (Vec<N>, Vec<(usize, usize, E)>)
where
    Ty: EdgeType,
    Ix: IndexType,
{
    into_transformed_nodes_edges(g, identity, identity)
}

pub fn from_nodes_edges<N, E, Ty, Ix>(
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
        let from = res.from_index(edge.0);
        let to = res.from_index(edge.1);
        res.add_edge(from, to, edge.2);
    }
    res
}

pub(crate) fn into_transformed_nodes_edges<N, E, Ty, Ix, F, G, NN, EE>(
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

pub(crate) fn transform_nodes_edges<N, NN, E, EE, Ty, Ix, F, G>(
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

pub(crate) fn transform_nodes<N, NN, E, Ty, Ix, F>(
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

pub fn contract_graph_edge<N, E, Ty, Ix>(
    g: Graph<N, E, Ty, Ix>,
    idx: usize,
) -> Graph<N, E, Ty, Ix>
where
    N: AddAssign,
    Ty: EdgeType,
    Ix: IndexType,
{
    let (mut nodes, mut edges) = into_nodes_edges(g);
    let contracted = edges.remove(idx);
    let (merged, removed) = minmax(contracted.0, contracted.1);
    if merged != removed {
        let removed_node = nodes.remove(removed);
        nodes[merged] += removed_node;
        for edge in &mut edges {
            for vx in [&mut edge.0, &mut edge.1] {
                match removed.cmp(vx) {
                    Ordering::Less => *vx -= 1,
                    Ordering::Equal => *vx = merged,
                    Ordering::Greater => {}
                }
            }
        }
    }
    from_nodes_edges(nodes, edges)
}

pub(crate) fn contract_duplicate<N, Ty, Ix>(
    mut g: Graph<N, EdgeWeight, Ty, Ix>,
) -> Graph<N, EdgeWeight, Ty, Ix>
where
    N: AddAssign,
    Ty: EdgeType,
    Ix: IndexType,
{
    let mut seen = AHashSet::new();
    let mut e = 0;
    while e < g.edge_count() {
        let idx = petgraph::visit::EdgeIndexable::from_index(&g, e);
        let p = norm_sign(&g.edge_weight(idx).unwrap().p);
        if seen.contains(p.as_ref()) {
            g = contract_graph_edge(g, e);
        } else {
            seen.insert(p.into_owned());
            e += 1;
        }
    }
    g
}

fn minmax<T: Ord>(s: T, t: T) -> (T, T) {
    if s < t {
        (s, t)
    } else {
        (t, s)
    }
}

fn norm_sign(p: &Momentum) -> Cow<'_, Momentum> {
    match p.terms().first() {
        Some(t) if t.coeff() < 0 => Cow::Owned(-p.clone()),
        _ => Cow::Borrowed(p),
    }
}

pub fn contains_cycle<N, E, Ix: IndexType>(g: &UnGraph<N, E, Ix>) -> bool {
    // need to do a manual DFS because the petgraph one uses edges
    // twice in an undirected graph

    fn insert_edge<Ix: IndexType>(
        adjacent_to: &mut AHashMap<NodeIndex<Ix>, AHashSet<NodeIndex<Ix>>>,
        source: NodeIndex<Ix>,
        target: NodeIndex<Ix>,
    ) -> bool {
        adjacent_to.entry(source)
            .or_default()
            .insert(target)
            && adjacent_to.entry(target)
            .or_default()
            .insert(source)
    }

    fn remove_edge<Ix: IndexType>(
        adjacent_to: &mut AHashMap<NodeIndex<Ix>, AHashSet<NodeIndex<Ix>>>,
        source: &NodeIndex<Ix>,
        target: &NodeIndex<Ix>,
    ) -> bool {
        adjacent_to.get_mut(source).unwrap().remove(target)
            && adjacent_to.get_mut(target).unwrap().remove(source)
    }

    let mut adjacent_to: AHashMap<_, AHashSet<_>> = AHashMap::new();
    for edge in g.edge_references() {
        if !insert_edge(&mut adjacent_to, edge.source(), edge.target()) {
            // we have seen an edge source <-> target before,
            // so there is a cycle
            return true;
        }
    }
    let mut remaining_nodes: AHashSet<_> = g.node_indices().collect();
    let mut visited_nodes = AHashSet::new();
    let mut edge_stack = Vec::new();

    while let Some(start) = remaining_nodes.iter().copied().next() {
        let removed = remaining_nodes.remove(&start);
        debug_assert!(removed);
        let new_visited = visited_nodes.insert(start);
        debug_assert!(new_visited);
        if let Some(adjacent) = adjacent_to.get(&start) {
            edge_stack.extend(adjacent.iter().map(|t| (start, *t)));
        }
        while let Some((current, next)) = edge_stack.pop() {
            let new_visited = visited_nodes.insert(next);
            if !new_visited {
                return true;
            }
            let removed = remove_edge(&mut adjacent_to, &current, &next);
            debug_assert!(removed);
            let removed = remaining_nodes.remove(&next);
            debug_assert!(removed);
            if let Some(adjacent) = adjacent_to.get(&next) {
                edge_stack.extend(adjacent.iter().map(|t| (current, *t)));
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn cycle() {
        log_init();

        let g = UnGraph::<(), i32>::from_edges(&[(0, 1)]);
        assert!(!contains_cycle(&g));

        let g = UnGraph::<(), i32>::from_edges(&[(0, 1), (1, 0)]);
        assert!(contains_cycle(&g));

        let g = UnGraph::<(), i32>::from_edges(&[(0, 1), (2, 3)]);
        assert!(!contains_cycle(&g));

        let g = UnGraph::<(), i32>::from_edges(&[(0, 1), (2, 3), (3, 4), (4, 2)]);
        assert!(contains_cycle(&g));
    }
}
