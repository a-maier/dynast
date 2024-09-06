use std::cmp::Ordering;
use std::fmt::{self, Display};

use ahash::RandomState;
use itertools::{izip, join, Itertools};
use log::{debug, trace};
use nalgebra::{DMatrix, Dim, Matrix, MatrixViewMut, RawStorage, U1};
use nauty_pet::graph::CanonGraph;
use num_traits::Zero;
use petgraph::Undirected;
use petgraph::{graph::UnGraph, visit::EdgeRef};
use thiserror::Error;

use crate::graph_util::Format;
use crate::mapper::TopologyWithExtMom;
use crate::momentum::{Momentum, Term};
use crate::symbol::Symbol;
use crate::yaml_dias::EdgeWeight;

type IndexMap<K, V> = indexmap::IndexMap<K, V, RandomState>;
type IndexSet<T> = indexmap::IndexSet<T, RandomState>;

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct Mapping(pub IndexMap<Symbol, Momentum>);

impl Display for Mapping {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sorted = Vec::from_iter(self.0.iter());
        sorted.sort();
        let output = sorted.iter().map(|(s, p)| format!("{s}: {p}")).join(", ");
        write!(f, "{{{output}}}")
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum MappingError {
    MomentumMismatch(Box<(IndexSet<Symbol>, IndexSet<Symbol>)>),
    SubgraphMismatch(usize, usize),
}

impl Display for MappingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MappingError::MomentumMismatch(err) => write!(
                f,
                "Sets of loop momenta differ: {{{}}} != {{{}}}",
                join(&err.0, ", "),
                join(&err.1, ", "),
            ),
            MappingError::SubgraphMismatch(from, to) => write!(
                f,
                "Number of subgraphs differ: {from} != {to}"
            ),
        }
    }
}

impl Mapping {
    pub(crate) fn identity(g: &TopologyWithExtMom) -> Self {
        let map = g.subgraphs().iter()
            .flat_map(|g| extract_momenta(g))
            .filter_map(|p| if g.external_momenta.contains(&p) {
                None
            } else {
                Some((p, p.into()))
            })
            .collect();
        Self(map)
    }

    pub(crate) fn new(
        from: &TopologyWithExtMom,
        to: &TopologyWithExtMom,
    ) -> Result<Self, MappingError> {
        if from.subgraphs().len() != to.subgraphs().len() {
            return Err(MappingError::SubgraphMismatch(from.subgraphs().len(), to.subgraphs().len()));
        }

        let mut ext_momenta = Vec::from_iter(
            from.external_momenta.union(&to.external_momenta).copied(),
        );
        ext_momenta.sort();

        let mut res = Self::default();
        for (from, to) in from.subgraphs().iter().zip(to.subgraphs()) {
            let mut subgraph_mapping = Self::new_single(
                from, to,
                &ext_momenta
            )?;
            res.0.append(&mut subgraph_mapping.0);
        }
        Ok(res)
    }

    pub(crate) fn new_single(
        from: &CanonGraph<Momentum, EdgeWeight, Undirected>,
        to: &CanonGraph<Momentum, EdgeWeight, Undirected>,
        ext_momenta: &[Symbol],
    ) -> Result<Self, MappingError> {
        debug_assert!(ext_momenta.windows(2).all(|q| q[0] < q[1]));

        debug!("Mapping {} onto {}", from.format(), to.format());
        if !shift_needed(&from, &to) {
            // TODO: code duplication with Self::identity
            let map = extract_momenta(from)
                .into_iter()
                .filter_map(|p| if ext_momenta.contains(&p) {
                None
            } else {
                Some((p, p.into()))
            });
            return Ok(Self(map.collect()));
        }
        debug_assert_eq!(from.edge_count(), to.edge_count());
        let mut from_loop_momenta = extract_momenta(from);
        let mut to_loop_momenta = extract_momenta(to);
        for q in ext_momenta {
            from_loop_momenta.swap_remove(q);
            to_loop_momenta.swap_remove(q);
        }
        if from_loop_momenta.len() != to_loop_momenta.len() {
            return Err(MappingError::MomentumMismatch(Box::new((
                from_loop_momenta,
                to_loop_momenta,
            ))));
        }
        let mut from_loop_momenta = Vec::from_iter(from_loop_momenta);
        from_loop_momenta.sort();
        let mut to_loop_momenta = Vec::from_iter(to_loop_momenta);
        to_loop_momenta.sort();
        let from_loop_momentum_pos = IndexMap::from_iter(
            from_loop_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );
        let to_loop_momentum_pos = IndexMap::from_iter(
            to_loop_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );
        let ext_momentum_pos = IndexMap::from_iter(
            ext_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );

        let mut shifts = Vec::new();
        for (from, to) in
            from.edge_references().zip(to.edge_references())
        {
            debug_assert_eq!(from.source(), to.source());
            debug_assert_eq!(from.target(), to.target());
            debug_assert_eq!(from.weight().m, to.weight().m);
            shifts.push(Shift::new(&from.weight().p, &to.weight().p));
        }
        shifts.sort();
        for shift in &shifts {
            debug!("{shift}");
        }

        let (l, q, lp, qp) = to_matrices(
            &shifts,
            &from_loop_momentum_pos,
            &to_loop_momentum_pos,
            &ext_momentum_pos,
        );

        trace!("{l} * l + {q} * q -> {lp} * l + {qp} * q");

        let linv = l.clone().try_inverse().unwrap();
        let o = &linv * &lp;
        let s = &linv * (&qp - &q);

        debug_assert_eq!(lp, &l * &o);
        debug_assert_eq!(qp, &l * &s + &q);

        Ok(Self::from_matrices(
            &o,
            &s,
            &from_loop_momenta,
            &to_loop_momenta,
            &ext_momenta,
        ))
    }

    pub fn from_matrices<R, C, S>(
        l: &Matrix<f64, R, C, S>,
        q: &Matrix<f64, R, C, S>,
        from_loop_momenta: &[Symbol],
        to_loop_momenta: &[Symbol],
        ext_momenta: &[Symbol],
    ) -> Self
    where
        C: Dim,
        R: Dim,
        S: RawStorage<f64, R, C>,
    {
        debug_assert_eq!(l.ncols(), to_loop_momenta.len());
        debug_assert_eq!(l.nrows(), from_loop_momenta.len());
        debug_assert_eq!(l.nrows(), q.nrows());
        debug_assert_eq!(q.ncols(), ext_momenta.len());
        let mut map = IndexMap::default();
        let rows = izip!(from_loop_momenta, l.row_iter(), q.row_iter());
        for (lhs, lrow, qrow) in rows {
            let mut rhs = Momentum::zero();
            for (coeff, p) in lrow.iter().zip(to_loop_momenta.iter()) {
                let coeff = *coeff as i32;
                if coeff != 0 {
                    rhs += Term::new(coeff, *p);
                }
            }
            for (coeff, p) in qrow.iter().zip(ext_momenta.iter()) {
                let coeff = *coeff as i32;
                if coeff != 0 {
                    rhs += Term::new(coeff, *p);
                }
            }
            map.insert(*lhs, rhs);
        }
        Self(map)
    }
}

impl IntoIterator for Mapping {
    type Item = (Symbol, Momentum);
    type IntoIter = indexmap::map::IntoIter<Symbol, Momentum>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn to_matrices(
    shifts: &[Shift],
    lpos: &IndexMap<Symbol, usize>,
    lppos: &IndexMap<Symbol, usize>,
    qpos: &IndexMap<Symbol, usize>,
) -> (DMatrix<f64>, DMatrix<f64>, DMatrix<f64>, DMatrix<f64>) {
    debug_assert_eq!(lpos.len(), lppos.len());
    let nloops = lpos.len();
    let next = qpos.len();
    let mut l = DMatrix::zeros(nloops, nloops);
    let mut q = DMatrix::zeros(nloops, next);
    let mut lp = DMatrix::zeros(nloops, nloops);
    let mut qp = DMatrix::zeros(nloops, next);
    let extr = CoeffExtract { lpos, qpos };
    let extrp = CoeffExtract { lpos: lppos, qpos };
    let mut row = 0;
    for shift in shifts.iter() {
        extr.extract_into(shift.lhs, l.row_mut(row), q.row_mut(row));
        extrp.extract_into(shift.rhs, lp.row_mut(row), qp.row_mut(row));
        row += 1;
        let rank = l.rows(0, row).rank(1e-10);
        if rank < row {
            row -= 1;
            l.row_mut(row).fill(0.);
            lp.row_mut(row).fill(0.);
            q.row_mut(row).fill(0.);
            qp.row_mut(row).fill(0.);
        }
        if row >= nloops {
            return (l, q, lp, qp);
        }
    }
    unreachable!(
        "Number of independent shifts cannot be smaller than number of loops"
    )
}

struct CoeffExtract<'a> {
    lpos: &'a IndexMap<Symbol, usize>,
    qpos: &'a IndexMap<Symbol, usize>,
}

impl<'a> CoeffExtract<'a> {
    fn extract_into<C, R>(
        &self,
        p: &Momentum,
        mut l: MatrixViewMut<'_, f64, U1, C, R, C>,
        mut q: MatrixViewMut<'_, f64, U1, C, R, C>,
    ) where
        C: Dim,
        R: Dim,
    {
        for term in p.terms() {
            if let Some(&col) = self.lpos.get(&term.symbol()) {
                l[col] = term.coeff() as f64;
            } else {
                let col = *self.qpos.get(&term.symbol()).unwrap();
                q[col] = term.coeff() as f64;
            };
        }
    }
}

fn shift_needed(
    from: &UnGraph<Momentum, EdgeWeight>,
    to: &UnGraph<Momentum, EdgeWeight>,
) -> bool {
    from.edge_weights()
        .zip(to.edge_weights())
        .any(|(from, to)| from.p != to.p && from.p != -to.p.clone())
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Shift<'a> {
    lhs: &'a Momentum,
    rhs: &'a Momentum,
}

impl<'a> Shift<'a> {
    fn new(lhs: &'a Momentum, rhs: &'a Momentum) -> Self {
        Self { lhs, rhs }
    }
}

impl<'a> std::fmt::Display for Shift<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} -> {}", self.lhs, self.rhs)
    }
}

impl<'a> PartialOrd for Shift<'a> {
    fn partial_cmp(&self, other: &Shift) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Shift<'a> {
    fn cmp(&self, other: &Shift) -> Ordering {
        (self.lhs.terms().len(), &self.lhs, &self.rhs).cmp(&(
            other.lhs.terms().len(),
            &other.lhs,
            &other.rhs,
        ))
    }
}

fn extract_momenta(g: &UnGraph<Momentum, EdgeWeight>) -> IndexSet<Symbol> {
    g.edge_weights()
        .flat_map(|e| e.p.terms().iter().map(|t| t.symbol()))
        .collect()
}
