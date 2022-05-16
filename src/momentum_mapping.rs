use std::cmp::Ordering;
use std::fmt::{self, Display};

use ahash::{AHashMap, AHashSet};
use itertools::{izip, join, Itertools};
use log::{debug, trace};
use nalgebra::{
    DMatrix, Dim, Matrix, MatrixSliceMut, RawStorage, RawStorageMut, U1,
};
use num_traits::Zero;
use petgraph::{graph::UnGraph, visit::EdgeRef};
use thiserror::Error;

use crate::momentum::{Momentum, Term};
use crate::symbol::Symbol;
use crate::yaml_dias::EdgeWeight;

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub(crate) struct Mapping {
    map: AHashMap<Symbol, Momentum>,
}

impl Display for Mapping {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sorted = Vec::from_iter(self.map.iter());
        sorted.sort_unstable();
        let output = sorted.iter().map(|(s, p)| format!("{s}: {p}")).join(", ");
        write!(f, "{{{output}}}")
    }
}

#[derive(Debug, Error)]
pub(crate) enum MappingError {
    #[error("Sets of loop momenta differ: {{{}}} != {{{}}}", join(.0, ", "), join(.1, ", "))]
    MomentumMismatch(AHashSet<Symbol>, AHashSet<Symbol>),
}

impl Mapping {
    pub(crate) fn identity(g: &UnGraph<Momentum, EdgeWeight>) -> Self {
        let external_momenta = extract_external_momenta(g);
        let map = extract_loop_momenta(g, &external_momenta)
            .into_iter()
            .map(|p| (p, p.into()))
            .collect();
        Self { map }
    }

    pub(crate) fn new(
        from: &UnGraph<Momentum, EdgeWeight>,
        to: &UnGraph<Momentum, EdgeWeight>,
    ) -> Result<Self, MappingError> {
        if !shift_needed(from, to) {
            return Ok(Self::identity(from));
        }
        debug_assert_eq!(from.edge_count(), to.edge_count());
        let ext_momenta = extract_external_momenta(from);
        debug_assert_eq!(ext_momenta, extract_external_momenta(to));
        let loop_momenta = extract_loop_momenta(from, &ext_momenta);
        let to_loop_momenta = extract_loop_momenta(to, &ext_momenta);
        if loop_momenta != to_loop_momenta {
            return Err(MappingError::MomentumMismatch(
                loop_momenta,
                to_loop_momenta,
            ));
        }
        let mut loop_momenta = Vec::from_iter(loop_momenta);
        loop_momenta.sort_unstable();
        let loop_momentum_pos = AHashMap::from_iter(
            loop_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );
        let mut ext_momenta = Vec::from_iter(ext_momenta);
        ext_momenta.sort_unstable();
        let ext_momentum_pos = AHashMap::from_iter(
            ext_momenta.iter().enumerate().map(|(n, p)| (*p, n)),
        );

        let mut shifts = Vec::new();
        for (from, to) in from.edge_references().zip(to.edge_references()) {
            debug_assert_eq!(from.source(), to.source());
            debug_assert_eq!(from.target(), to.target());
            debug_assert_eq!(from.weight().m, to.weight().m);
            shifts.push(Shift::new(&from.weight().p, &to.weight().p));
        }
        shifts.sort_unstable();
        for shift in &shifts {
            debug!("{shift}");
        }

        let (l, q, lp, qp) =
            to_matrices(&shifts, &loop_momentum_pos, &ext_momentum_pos);

        let (lred, qred, mut lpred, mut qpred) =
            trunc_independent(l.clone(), q.clone(), lp.clone(), qp.clone());

        trace!("{lred} * l + {qred} * q -> {lpred} * l + {qpred} * q");
        let linv = lred.try_inverse().unwrap();
        for signs in 0..2usize.pow(lpred.ncols() as u32) {
            apply_signs(&mut lpred, signs);
            apply_signs(&mut qpred, signs);

            let o = &linv * &lpred;
            let s = &linv * (&qpred - &qred);
            let (l_new, q_new) = normalise_row_signs(&l * &o, &l * &s + &q);
            trace!("Shifted with sign {signs}: {l_new} * l + {q_new} * q");
            if l_new == lp && q_new == qp {
                return Ok(Self::from_matrices(
                    &o,
                    &s,
                    &loop_momenta,
                    &ext_momenta,
                ));
            }

            apply_signs(&mut qpred, signs);
            apply_signs(&mut lpred, signs);
        }
        unreachable!()
    }

    fn from_matrices<R, C, S>(
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
        let mut map = AHashMap::new();
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
        Self { map }
    }
}

fn normalise_row_signs<R, C, S>(
    mut l: Matrix<f64, R, C, S>,
    mut q: Matrix<f64, R, C, S>,
) -> (Matrix<f64, R, C, S>, Matrix<f64, R, C, S>)
where
    C: Dim,
    R: Dim,
    S: RawStorageMut<f64, R, C>,
{
    for (mut lrow, mut qrow) in l.row_iter_mut().zip(q.row_iter_mut()) {
        let sign_flip = lrow
            .iter()
            .chain(qrow.iter())
            .find(|&&e| e != 0.)
            .map(|&e| e < 0.)
            .unwrap_or(false);
        if sign_flip {
            lrow *= -1.;
            qrow *= -1.;
        }
    }
    (l, q)
}

fn to_matrices(
    shifts: &[Shift],
    lpos: &AHashMap<Symbol, usize>,
    qpos: &AHashMap<Symbol, usize>,
) -> (DMatrix<f64>, DMatrix<f64>, DMatrix<f64>, DMatrix<f64>) {
    let nshifts = shifts.len();
    let nloops = lpos.len();
    let next = qpos.len();
    let mut l = DMatrix::zeros(nshifts, nloops);
    let mut q = DMatrix::zeros(nshifts, next);
    let mut lp = DMatrix::zeros(nshifts, nloops);
    let mut qp = DMatrix::zeros(nshifts, next);
    let extr = CoeffExtract { lpos, qpos };
    for (row, shift) in shifts.iter().enumerate() {
        extr.extract_into(shift.lhs, l.row_mut(row), q.row_mut(row));
        extr.extract_into(shift.rhs, lp.row_mut(row), qp.row_mut(row));
    }
    (l, q, lp, qp)
}

struct CoeffExtract<'a> {
    lpos: &'a AHashMap<Symbol, usize>,
    qpos: &'a AHashMap<Symbol, usize>,
}

impl<'a> CoeffExtract<'a> {
    fn extract_into<C, R>(
        &self,
        p: &Momentum,
        mut l: MatrixSliceMut<'_, f64, U1, C, R, C>,
        mut q: MatrixSliceMut<'_, f64, U1, C, R, C>,
    ) where
        C: Dim,
        R: Dim,
    {
        let sign = match p.terms().first() {
            Some(term) if term.coeff() < 0 => -1,
            _ => 1,
        } as f64;
        for term in p.terms() {
            if let Some(&col) = self.lpos.get(&term.symbol()) {
                l[col] = sign * term.coeff() as f64;
            } else {
                let col = *self.qpos.get(&term.symbol()).unwrap();
                q[col] = sign * term.coeff() as f64;
            };
        }
    }
}

pub fn apply_signs<T, R, C, S>(m: &mut Matrix<T, R, C, S>, signs: usize)
where
    T: std::ops::MulAssign<f64>,
    R: Dim,
    C: Dim,
    S: RawStorage<T, R, C> + RawStorageMut<T, R, C>,
{
    for i in 0..m.nrows() {
        if ((signs >> i) & 1) == 1 {
            for e in m.row_mut(i).iter_mut() {
                *e *= -1.
            }
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

fn trunc_independent(
    mut l: DMatrix<f64>,
    mut q: DMatrix<f64>,
    mut lp: DMatrix<f64>,
    mut qp: DMatrix<f64>,
) -> (DMatrix<f64>, DMatrix<f64>, DMatrix<f64>, DMatrix<f64>) {
    let nloops = l.ncols();
    for dim in 2..=nloops {
        loop {
            let rank = l.rows(0, dim).rank(1e-10);
            if rank == dim {
                break;
            }
            l = l.remove_row(dim);
            q = q.remove_row(dim);
            lp = lp.remove_row(dim);
            qp = qp.remove_row(dim);
        }
    }
    if l.nrows() > nloops {
        let ndelete = l.nrows() - nloops;
        l = l.remove_rows(nloops, ndelete);
        q = q.remove_rows(nloops, ndelete);
        lp = lp.remove_rows(nloops, ndelete);
        qp = qp.remove_rows(nloops, ndelete);
    }
    (l, q, lp, qp)
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
        (self.lhs.terms().len(), &self.lhs, &self.rhs).partial_cmp(&(
            other.lhs.terms().len(),
            &other.lhs,
            &other.rhs,
        ))
    }
}

impl<'a> Ord for Shift<'a> {
    fn cmp(&self, other: &Shift) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn extract_external_momenta<E>(g: &UnGraph<Momentum, E>) -> AHashSet<Symbol> {
    let mut res = AHashSet::new();
    for p in g.node_weights() {
        for term in p.terms() {
            res.insert(term.symbol());
        }
    }
    res
}

fn extract_loop_momenta<N>(
    g: &UnGraph<N, EdgeWeight>,
    external_momenta: &AHashSet<Symbol>,
) -> AHashSet<Symbol> {
    let mut res = AHashSet::new();
    for w in g.edge_weights() {
        for term in w.p.terms() {
            if !external_momenta.contains(&term.symbol()) {
                res.insert(term.symbol());
            }
        }
    }
    res
}
