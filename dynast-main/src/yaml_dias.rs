use std::cmp::max;
use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::iter::once;

use crate::{Momentum, Symbol, Term};
use derivative::Derivative;
use nom::{
    character::complete::{
        alpha1, alphanumeric0, char, digit1, multispace0, one_of,
    },
    combinator::{opt, recognize},
    multi::many0,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use petgraph::graph::UnGraph;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::graph_util::Format;

#[derive(
    Clone,
    Default,
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Deserialize,
    Serialize,
)]
#[serde(transparent)]
pub struct Diagram {
    denominators: Vec<Denom>,
}

impl Diagram {
    pub fn denominators(&self) -> &Vec<Denom> {
        &self.denominators
    }

    pub fn denominator(&self, n: usize) -> &Denom {
        &self.denominators[n]
    }

    pub fn into_denominators(self) -> Vec<Denom> {
        self.denominators
    }

    pub fn new(denominators: Vec<Denom>) -> Self {
        Self { denominators }
    }
}

#[derive(
    Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize,
)]
#[serde(untagged)]
pub enum Denom {
    Prop(u32, u32, NumOrString, NumOrString),
    Sp(NumOrString, NumOrString),
}

#[derive(
    Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Deserialize, Serialize,
)]
#[serde(untagged)]
pub enum NumOrString {
    Num(i64),
    String(String),
}

impl From<NumOrString> for String {
    fn from(s: NumOrString) -> Self {
        match s {
            NumOrString::Num(num) => num.to_string(),
            NumOrString::String(s) => s,
        }
    }
}

impl From<String> for NumOrString {
    fn from(source: String) -> Self {
        Self::String(source)
    }
}

impl From<i64> for NumOrString {
    fn from(source: i64) -> Self {
        Self::Num(source)
    }
}

impl Display for NumOrString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumOrString::Num(num) => num.fmt(f),
            NumOrString::String(s) => s.fmt(f),
        }
    }
}

impl TryFrom<NumOrString> for Momentum {
    type Error = ImportError;

    fn try_from(s: NumOrString) -> Result<Self, Self::Error> {
        let s: String = s.into();
        let (rest, p) = momentum(&s)?;
        if rest.trim_start().is_empty() {
            Ok(p)
        } else {
            Err(ImportError::MomentumParseError(s))
        }
    }
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ImportError {
    #[error("Failed to parse momentum: {0}")]
    MomentumParseError(String),
}

impl<'a> std::convert::From<nom::Err<nom::error::Error<&'a str>>>
    for ImportError
{
    fn from(e: nom::Err<nom::error::Error<&'a str>>) -> Self {
        ImportError::MomentumParseError(e.to_string())
    }
}

impl TryFrom<Diagram> for UnGraph<Momentum, EdgeWeight> {
    type Error = ImportError;

    fn try_from(dia: Diagram) -> Result<Self, Self::Error> {
        use petgraph::visit::NodeIndexable;
        use Denom::*;

        let propagators = Vec::from_iter(
            dia.into_denominators().into_iter().filter_map(|d| {
                if let Prop(v1, v2, p, m) = d {
                    Some((v1, v2, p, m))
                } else {
                    None
                }
            }),
        );

        let nprops = propagators.len();
        let nvertices = propagators
            .iter()
            .map(|(from, to, _, _)| max(*from, *to) + 1)
            .max()
            .unwrap_or(0);
        let mut res = UnGraph::with_capacity(nvertices as usize, nprops);
        for _ in 0..nvertices {
            res.add_node(Momentum::zero());
        }

        for (from, to, p, m) in propagators {
            let from = res.from_index(from as usize);
            let to = res.from_index(to as usize);
            let p = p.try_into()?;
            let m = m.into();
            *res.node_weight_mut(from).unwrap() -= &p;
            *res.node_weight_mut(to).unwrap() += &p;
            res.add_edge(from, to, EdgeWeight { p, m });
        }

        Ok(res)
    }
}

#[derive(Clone, Debug, Default, Derivative)]
#[derivative(Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct EdgeWeight {
    #[derivative(
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore",
        Hash = "ignore"
    )]
    pub p: Momentum,
    pub m: String,
}

fn momentum(input: &str) -> IResult<&str, Momentum> {
    use Sign::*;
    use TermOrZero::*;
    let (rest, (sign, first_term, terms)) = tuple((
        opt(sign),
        preceded(multispace0, term_or_zero),
        many0(pair(
            preceded(multispace0, sign),
            preceded(multispace0, term_or_zero),
        )),
    ))(input)?;
    let sign = sign.unwrap_or(Plus);
    let p =
        Momentum::from_iter(once((sign, first_term)).chain(terms).filter_map(
            |(s, term)| {
                if let Term(term) = term {
                    Some(if s == Minus { -term } else { term })
                } else {
                    None
                }
            },
        ));
    Ok((rest, p))
}

fn sign(input: &str) -> IResult<&str, Sign> {
    use Sign::*;
    let (rest, sign) = one_of("+-")(input)?;
    if sign == '+' {
        Ok((rest, Plus))
    } else {
        Ok((rest, Minus))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum Sign {
    Plus,
    Minus,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum TermOrZero {
    Zero,
    Term(Term),
}

fn term_or_zero(input: &str) -> IResult<&str, TermOrZero> {
    use TermOrZero::*;
    if let Ok((rest, _)) = char::<_, nom::error::Error<_>>('0')(input) {
        Ok((rest, Zero))
    } else {
        let (rest, term) = term(input)?;
        Ok((rest, Term(term)))
    }
}

fn term(input: &str) -> IResult<&str, Term> {
    let (rest, (coeff, sym)) = separated_pair(
        opt(terminated(u32, pair(multispace0, char('*')))),
        multispace0,
        var,
    )(input)?;
    let sym = Symbol::new(sym);
    let term = if let Some(coeff) = coeff {
        Term::new(coeff as i32, sym)
    } else {
        sym.into()
    };
    Ok((rest, term))
}

// TODO: allow [name]
fn var(input: &str) -> IResult<&str, &str> {
    recognize(tuple((alpha1, alphanumeric0)))(input)
}

fn u32(input: &str) -> IResult<&str, u32> {
    let (rest, num) = digit1(input)?;
    Ok((rest, num.parse().unwrap()))
}

impl<'a> Format<'a> for Diagram {
    type Output = FormatDia<'a>;

    fn format(&'a self) -> Self::Output {
        FormatDia(self)
    }
}

pub struct FormatDia<'a>(&'a Diagram);

impl Display for FormatDia<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Denom::*;
        writeln!(
            f,
            "Graph {{
   ["
        )?;
        for den in &self.0.denominators {
            match den {
                Prop(from, to, p, m) => {
                    writeln!(f, "      [({from}, {to}), {p}, {m}],")?
                }
                Sp(p, m) => writeln!(f, "      [{p}, {m}],")?,
            }
        }
        writeln!(f, "   ],\n}}")
    }
}

#[cfg(test)]
mod tests {
    use dynast_momentum::symbols;

    use super::*;

    #[test]
    fn parse_var() {
        let (_rest, v) = var("a").unwrap();
        assert_eq!(v, "a");

        let v = var("0");
        assert!(v.is_err());
    }

    #[test]
    fn parse_term() {
        symbols!(p);
        let (_rest, t) = term("p").unwrap();
        assert_eq!(t, Term::from(p));

        let (_rest, t) = term("2*p").unwrap();
        assert_eq!(t, Term::new(2, p));

        let (_rest, t) = term("2 *p").unwrap();
        assert_eq!(t, Term::new(2, p));

        let (_rest, t) = term("2* p").unwrap();
        assert_eq!(t, Term::new(2, p));

        let (_rest, t) = term("2 * p").unwrap();
        assert_eq!(t, Term::new(2, p));
    }

    #[test]
    fn parse_momentum() {
        let (_rest, p) = momentum("0").unwrap();
        assert_eq!(p, Momentum::zero());

        let (_rest, p) = momentum("p - p").unwrap();
        assert_eq!(p, Momentum::zero());

        let (_rest, p) = momentum("- p + p").unwrap();
        assert_eq!(p, Momentum::zero());

        let (_rest, p) = momentum("- p + 2*p - p").unwrap();
        assert_eq!(p, Momentum::zero());

        let _ = momentum("p").unwrap();
        let _ = momentum("p1").unwrap();
    }
}
