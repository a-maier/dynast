use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::From;
use std::fmt::{self, Display};
use std::hash::BuildHasher;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use crate::symbol::Symbol;

#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Term {
    /// Momentum symbol
    pub symbol: Symbol,
    /// Numeric prefactor
    pub coeff: i32,
}

impl Term {
    pub fn new(coeff: i32, symbol: Symbol) -> Term {
        Term { symbol, coeff }
    }
}

impl From<Symbol> for Term {
    fn from(symbol: Symbol) -> Self {
        Self { symbol, coeff: 1 }
    }
}

impl Neg for Term {
    type Output = Self;

    fn neg(mut self) -> Self {
        self.coeff *= -1;
        self
    }
}

impl MulAssign<i32> for Term {
    fn mul_assign(&mut self, c: i32) {
        self.coeff *= c;
    }
}

impl Mul<i32> for Term {
    type Output = Term;

    fn mul(mut self, c: i32) -> Self::Output {
        self *= c;
        self
    }
}

impl Mul<Term> for i32 {
    type Output = Term;

    fn mul(self, t: Term) -> Self::Output {
        t * self
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.coeff {
            0 => '0'.fmt(f),
            1 => self.symbol.fmt(f),
            -1 => write!(f, "-{}", self.symbol),
            coeff => write!(f, "{coeff}*{}", self.symbol),
        }
    }
}

/// A four-momentum, represented
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde-1", serde(transparent))]
pub struct Momentum {
    #[cfg_attr(
        feature = "serde-1",
        serde(deserialize_with = "deserialize_terms")
    )]
    terms: Vec<Term>,
}

#[cfg(feature = "serde-1")]
fn deserialize_terms<'de, D: serde::Deserializer<'de>>(
    d: D,
) -> Result<Vec<Term>, D::Error> {
    use serde::Deserialize;
    let mut terms = <Vec<Term>>::deserialize(d)?;
    canonise_terms(&mut terms);
    Ok(terms)
}

fn canonise_terms(terms: &mut Vec<Term>) {
    terms.sort_unstable();
    for mut j in 1..terms.len() {
        let i = j - 1;
        while j < terms.len() && terms[i].symbol == terms[j].symbol {
            terms[i].coeff += std::mem::replace(&mut terms[j].coeff, 0);
            j += 1;
        }
    }
    terms.retain(|t| t.coeff != 0);
}

impl Momentum {
    /// Construct a momentum as a sum of terms
    ///
    /// # Safety
    ///
    /// * The terms should be sorted
    /// * No two terms should have the same symbol.
    /// * All coefficients should be non-zero.
    pub unsafe fn from_terms_unchecked(terms: Vec<Term>) -> Self {
        debug_assert!(terms.windows(2).all(|t| t[0].symbol < t[1].symbol));
        debug_assert!(terms.iter().all(|t| t.coeff != 0));
        Self { terms }
    }

    pub fn from_terms(mut terms: Vec<Term>) -> Self {
        canonise_terms(&mut terms);
        Self { terms }
    }

    pub fn terms(&self) -> &[Term] {
        &self.terms
    }

    pub fn into_terms(self) -> Vec<Term> {
        self.terms
    }

    pub fn pop(&mut self) -> Option<Term> {
        self.terms.pop()
    }

    pub fn len(&self) -> usize {
        self.terms.len()
    }

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn is_zero(&self) -> bool {
        self.is_empty()
    }

    pub fn retain<F>(&mut self, f: F)
    where
        for<'a> F: FnMut(&'a Term) -> bool,
    {
        self.terms.retain(f)
    }

    pub fn zero() -> Self {
        Self { terms: vec![] }
    }

    fn add_assign_iter<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = Term>,
    {
        let mut res = Vec::new();
        let mut it1 = self.terms.iter();
        let mut it2 = iter.into_iter();
        let mut t1 = it1.next();
        let mut t2 = it2.next();
        while let (Some(tt1), Some(tt2)) = (t1, t2) {
            match tt1.symbol.cmp(&tt2.symbol) {
                Ordering::Less => {
                    res.push(*tt1);
                    t1 = it1.next();
                }
                Ordering::Greater => {
                    res.push(tt2);
                    t2 = it2.next();
                }
                Ordering::Equal => {
                    let coeff = tt1.coeff + tt2.coeff;
                    if coeff != 0 {
                        let symbol = tt1.symbol;
                        res.push(Term { coeff, symbol });
                    }
                    t1 = it1.next();
                    t2 = it2.next();
                }
            }
        }
        if let Some(t1) = t1 {
            res.push(*t1);
        }
        if let Some(t2) = t2 {
            res.push(t2);
        }
        res.extend(it1);
        res.extend(it2);
        self.terms = res;
    }
}

impl FromIterator<Term> for Momentum {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Term>,
    {
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            let mut p = first.into();
            for term in iter {
                p += term
            }
            p
        } else {
            Momentum::zero()
        }
    }
}

impl Neg for Momentum {
    type Output = Self;

    fn neg(mut self) -> Self {
        for t in &mut self.terms {
            t.coeff *= -1;
        }
        self
    }
}

impl From<Term> for Momentum {
    fn from(t: Term) -> Self {
        if t.coeff == 0 {
            Momentum::zero()
        } else {
            Momentum { terms: vec![t] }
        }
    }
}

impl From<Symbol> for Momentum {
    fn from(s: Symbol) -> Self {
        Momentum {
            terms: vec![s.into()],
        }
    }
}

impl Display for Momentum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((first, rest)) = self.terms.split_first() {
            first.fmt(f)?;
            for term in rest {
                if term.coeff > 0 {
                    write!(f, "+{term}")?;
                } else {
                    term.fmt(f)?;
                }
            }
            Ok(())
        } else {
            '0'.fmt(f)
        }
    }
}

impl Add for Momentum {
    type Output = Self;

    fn add(mut self, other: Momentum) -> Self::Output {
        self += other;
        self
    }
}

impl Add<&Momentum> for Momentum {
    type Output = Self;

    fn add(mut self, other: &Momentum) -> Self::Output {
        self += other;
        self
    }
}

impl Add<Momentum> for &Momentum {
    type Output = Momentum;

    fn add(self, mut other: Momentum) -> Self::Output {
        other += self;
        other
    }
}

impl<'a> Add<&'a Momentum> for &'_ Momentum {
    type Output = Momentum;

    fn add(self, other: &'a Momentum) -> Self::Output {
        let mut res = self.clone();
        res += other;
        res
    }
}

impl Sub for Momentum {
    type Output = Self;

    fn sub(mut self, other: Momentum) -> Self::Output {
        self -= other;
        self
    }
}

impl Sub<&Momentum> for Momentum {
    type Output = Self;

    fn sub(mut self, other: &Momentum) -> Self::Output {
        self -= other;
        self
    }
}

impl Sub<Momentum> for &Momentum {
    type Output = Momentum;

    fn sub(self, mut other: Momentum) -> Self::Output {
        other -= self;
        other
    }
}

impl<'a> Sub<&'a Momentum> for &'_ Momentum {
    type Output = Momentum;

    fn sub(self, other: &'a Momentum) -> Self::Output {
        let mut res = self.clone();
        res -= other;
        res
    }
}

impl AddAssign<&Momentum> for Momentum {
    fn add_assign(&mut self, other: &Momentum) {
        self.add_assign_iter(other.terms.iter().copied());
    }
}

impl AddAssign<Momentum> for Momentum {
    fn add_assign(&mut self, other: Momentum) {
        self.add_assign(&other)
    }
}

impl AddAssign<Term> for Momentum {
    fn add_assign(&mut self, rhs: Term) {
        let sym = &rhs.symbol;
        let pos = self.terms.binary_search_by_key(sym, |t| t.symbol);
        match pos {
            Ok(pos) => {
                self.terms[pos].coeff += rhs.coeff;
                if self.terms[pos].coeff == 0 {
                    self.terms.remove(pos);
                }
            }
            Err(pos) => self.terms.insert(pos, rhs),
        }
    }
}

impl SubAssign<&Momentum> for Momentum {
    fn sub_assign(&mut self, other: &Momentum) {
        self.add_assign_iter(other.terms.iter().map(|t| -*t));
    }
}

impl SubAssign<Momentum> for Momentum {
    fn sub_assign(&mut self, other: Momentum) {
        self.sub_assign(&other)
    }
}

impl SubAssign<Term> for Momentum {
    fn sub_assign(&mut self, rhs: Term) {
        let sym = &rhs.symbol;
        let pos = self.terms.binary_search_by_key(sym, |t| t.symbol);
        match pos {
            Ok(pos) => {
                self.terms[pos].coeff -= rhs.coeff;
                if self.terms[pos].coeff == 0 {
                    self.terms.remove(pos);
                }
            }
            Err(pos) => self.terms.insert(pos, -rhs),
        }
    }
}

impl MulAssign<i32> for Momentum {
    fn mul_assign(&mut self, c: i32) {
        if c == 0 {
            self.terms.clear()
        } else {
            for t in &mut self.terms {
                *t *= c;
            }
        }
    }
}

impl Mul<i32> for Momentum {
    type Output = Self;

    fn mul(mut self, c: i32) -> Self::Output {
        self *= c;
        self
    }
}

impl Mul<i32> for &Momentum {
    type Output = Momentum;

    fn mul(self, c: i32) -> Self::Output {
        self.clone() * c
    }
}

impl Mul<Momentum> for i32 {
    type Output = Momentum;

    fn mul(self, p: Momentum) -> Self::Output {
        p * self
    }
}

impl Mul<&Momentum> for i32 {
    type Output = Momentum;

    fn mul(self, p: &Momentum) -> Self::Output {
        p * self
    }
}

#[cfg(feature = "num-traits")]
impl num_traits::Zero for Momentum {
    fn zero() -> Self {
        Momentum { terms: vec![] }
    }

    fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }
}

impl Mul<i32> for Symbol {
    type Output = Term;

    fn mul(self, c: i32) -> Self::Output {
        Term::new(c, self)
    }
}

impl Mul<Symbol> for i32 {
    type Output = Term;

    fn mul(self, s: Symbol) -> Self::Output {
        Term::new(self, s)
    }
}

pub trait Replace<M> {
    type Output;

    fn replace(self, map: M) -> Self::Output;
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Momentum, S>> for Symbol {
    type Output = Momentum;

    fn replace(self, map: &'a HashMap<Symbol, Momentum, S>) -> Self::Output {
        map.get(&self).cloned().unwrap_or(Momentum::from(self))
    }
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Symbol, S>> for Symbol {
    type Output = Symbol;

    fn replace(self, map: &'a HashMap<Symbol, Symbol, S>) -> Self::Output {
        map.get(&self).copied().unwrap_or(self)
    }
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Symbol, S>> for Term {
    type Output = Term;

    fn replace(mut self, map: &'a HashMap<Symbol, Symbol, S>) -> Self::Output {
        self.symbol = self.symbol.replace(map);
        self
    }
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Symbol, S>> for Momentum {
    type Output = Momentum;

    fn replace(self, map: &'a HashMap<Symbol, Symbol, S>) -> Self::Output {
        self.into_terms()
            .into_iter()
            .map(|t| t.replace(map))
            .collect()
    }
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Momentum, S>> for Term {
    type Output = Momentum;

    fn replace(self, map: &'a HashMap<Symbol, Momentum, S>) -> Self::Output {
        let Term { symbol, coeff } = self;
        coeff * symbol.replace(map)
    }
}

impl<'a, S: BuildHasher> Replace<&'a HashMap<Symbol, Momentum, S>>
    for Momentum
{
    type Output = Momentum;

    fn replace(self, map: &'a HashMap<Symbol, Momentum, S>) -> Self::Output {
        self.into_terms().into_iter().map(|t| t.replace(map)).sum()
    }
}

impl Replace<(Symbol, Momentum)> for Momentum {
    type Output = Momentum;

    fn replace(mut self, repl: (Symbol, Momentum)) -> Self::Output {
        let (lhs, mut rhs) = repl;
        let pos = self.terms().binary_search_by_key(&lhs, |t| t.symbol);
        if let Ok(pos) = pos {
            let coeff = self.terms.remove(pos).coeff;
            // TODO: use fused multiply add?
            rhs *= coeff;
            self += rhs;
        }
        self
    }
}

impl Replace<(Symbol, Symbol)> for Momentum {
    type Output = Momentum;

    fn replace(mut self, repl: (Symbol, Symbol)) -> Self::Output {
        let (lhs, rhs) = repl;
        let pos = self.terms().binary_search_by_key(&lhs, |t| t.symbol);
        if let Ok(pos) = pos {
            let mut t = self.terms.remove(pos);
            t.symbol = rhs;
            self += t;
        }
        self
    }
}

impl Sum for Momentum {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, e| acc + e).unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols;

    #[test]
    fn momentum() {
        symbols!(x, y);
        let x = Momentum::from(x);
        let y = Momentum::from(y);
        assert_eq!(Momentum::zero(), &x - &x);
        assert_eq!(2 * &x, &x + &x);
        assert_eq!(Momentum::zero(), &x + &y - &x - &y);
    }
}
