use std::hash::BuildHasher;

use indexmap::IndexMap;

use crate::{
    momentum::{Momentum, Replace, Term},
    symbol::Symbol,
};

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Momentum, S>> for Symbol {
    type Output = Momentum;

    fn replace(self, map: &'a IndexMap<Symbol, Momentum, S>) -> Self::Output {
        map.get(&self).cloned().unwrap_or(Momentum::from(self))
    }
}

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Momentum, S>> for Term {
    type Output = Momentum;

    fn replace(self, map: &'a IndexMap<Symbol, Momentum, S>) -> Self::Output {
        let Term { symbol, coeff } = self;
        coeff * symbol.replace(map)
    }
}

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Momentum, S>>
    for Momentum
{
    type Output = Momentum;

    fn replace(self, map: &'a IndexMap<Symbol, Momentum, S>) -> Self::Output {
        self.into_terms().into_iter().map(|t| t.replace(map)).sum()
    }
}

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Symbol, S>> for Symbol {
    type Output = Symbol;

    fn replace(self, map: &'a IndexMap<Symbol, Symbol, S>) -> Self::Output {
        map.get(&self).copied().unwrap_or(self)
    }
}

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Symbol, S>> for Term {
    type Output = Term;

    fn replace(mut self, map: &'a IndexMap<Symbol, Symbol, S>) -> Self::Output {
        self.symbol = self.symbol.replace(map);
        self
    }
}

impl<'a, S: BuildHasher> Replace<&'a IndexMap<Symbol, Symbol, S>> for Momentum {
    type Output = Momentum;

    fn replace(self, map: &'a IndexMap<Symbol, Symbol, S>) -> Self::Output {
        self.into_terms()
            .into_iter()
            .map(|t| t.replace(map))
            .collect()
    }
}
