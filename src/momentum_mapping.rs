use std::cmp::Ordering;
use std::fmt::{self, Display};

use ahash::RandomState;
use itertools::{izip, join, Itertools};
use log::{debug, trace};
use nalgebra::{DMatrix, Dim, Matrix, MatrixViewMut, RawStorage, U1};
use num_traits::Zero;
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
pub struct Mapping (
    pub IndexMap<Symbol, Momentum>
);

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
}

impl Display for MappingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MappingError::MomentumMismatch(err) => write!(
                f,
                "Sets of loop momenta differ: {{{}}} != {{{}}}",
                join(&err.0, ", "),
                join(&err.1, ", "),
            )
        }
    }
}

impl Mapping {
    pub(crate) fn identity(g: &TopologyWithExtMom) -> Self {
        let map = extract_loop_momenta(g)
            .into_iter()
            .map(|p| (p, p.into()))
            .collect();
        Self ( map )
    }

    pub(crate) fn new(
        from: &TopologyWithExtMom,
        to: &TopologyWithExtMom,
    ) -> Result<Self, MappingError> {
        debug!("Mapping {} onto {}", from.graph.format(), to.graph.format());
        if !shift_needed(&from.graph, &to.graph) {
            return Ok(Self::identity(from));
        }
        debug_assert_eq!(from.graph.edge_count(), to.graph.edge_count());
        let mut ext_momenta = Vec::from_iter(
            from.external_momenta.union(&to.external_momenta).copied()
        );
        ext_momenta.sort();
        let mut loop_momenta = extract_loop_momenta(from);
        let mut to_loop_momenta = extract_loop_momenta(to);
        for q in &ext_momenta {
            loop_momenta.swap_remove(q);
            to_loop_momenta.swap_remove(q);
        }
        let loop_momenta = loop_momenta;
        let to_loop_momenta = to_loop_momenta;
        if loop_momenta != to_loop_momenta {
            return Err(MappingError::MomentumMismatch(Box::new((
                loop_momenta,
                to_loop_momenta,
            ))));
        }
        let mut loop_momenta = Vec::from_iter(loop_momenta);
        loop_momenta.sort();
        let loop_momentum_pos = IndexMap::from_iter(
            loop_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );
        let ext_momentum_pos = IndexMap::from_iter(
            ext_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );

        let mut shifts = Vec::new();
        for (from, to) in from.graph.edge_references().zip(to.graph.edge_references()) {
            debug_assert_eq!(from.source(), to.source());
            debug_assert_eq!(from.target(), to.target());
            debug_assert_eq!(from.weight().m, to.weight().m);
            shifts.push(Shift::new(&from.weight().p, &to.weight().p));
        }
        shifts.sort();
        for shift in &shifts {
            debug!("{shift}");
        }

        let (l, q, lp, qp) =
            to_matrices(&shifts, &loop_momentum_pos, &ext_momentum_pos);

        trace!("{l} * l + {q} * q -> {lp} * l + {qp} * q");

        let linv = l.clone().try_inverse().unwrap();
        let o = &linv * &lp;
        let s = &linv * (&qp - &q);

        debug_assert_eq!(lp, &l * &o);
        debug_assert_eq!(qp, &l * &s + &q);

        Ok(Self::from_matrices(&o, &s, &loop_momenta, &ext_momenta))
    }

    pub fn from_matrices<R, C, S>(
        l: &Matrix<f64, R, C, S>,
        q: &Matrix<f64, R, C, S>,
        loop_momenta: &[Symbol],
        ext_momenta: &[Symbol],
    ) -> Self
    where
        C: Dim,
        R: Dim,
        S: RawStorage<f64, R, C>,
    {
        debug_assert_eq!(l.ncols(), loop_momenta.len());
        debug_assert_eq!(l.nrows(), l.ncols());
        debug_assert_eq!(l.nrows(), q.nrows());
        debug_assert_eq!(q.ncols(), ext_momenta.len());
        let mut map = IndexMap::default();
        let rows = izip!(loop_momenta, l.row_iter(), q.row_iter());
        for (lhs, lrow, qrow) in rows {
            let mut rhs = Momentum::zero();
            for (coeff, p) in lrow.iter().zip(loop_momenta.iter()) {
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
        Self ( map )
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
    qpos: &IndexMap<Symbol, usize>,
) -> (DMatrix<f64>, DMatrix<f64>, DMatrix<f64>, DMatrix<f64>) {
    let nloops = lpos.len();
    let next = qpos.len();
    let mut l = DMatrix::zeros(nloops, nloops);
    let mut q = DMatrix::zeros(nloops, next);
    let mut lp = DMatrix::zeros(nloops, nloops);
    let mut qp = DMatrix::zeros(nloops, next);
    let extr = CoeffExtract { lpos, qpos };
    let mut row = 0;
    for shift in shifts.iter() {
        extr.extract_into(shift.lhs, l.row_mut(row), q.row_mut(row));
        extr.extract_into(shift.rhs, lp.row_mut(row), qp.row_mut(row));
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
    unreachable!("Number of independent shifts cannot be smaller than number of loops")
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

fn extract_loop_momenta(
    g: &TopologyWithExtMom
) -> IndexSet<Symbol> {
    let mut res = IndexSet::default();
    for w in g.graph.edge_weights() {
        for term in w.p.terms() {
            if !g.external_momenta.contains(&term.symbol()) {
                res.insert(term.symbol());
            }
        }
    }
    res
}
