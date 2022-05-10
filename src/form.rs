use std::iter::once;

use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric0, char, digit1, line_ending, one_of, space1},
    combinator::{opt, recognize},
    multi::many0,
    sequence::{delimited, preceded, separated_pair, pair, terminated, tuple},
};
use num_rational::Rational32;
use thiserror::Error;

use crate::momentum::{Term, Momentum};
use crate::symbol::Symbol;
use crate::diagram::{Diagram, Field, Propagator, Vertex};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct FormDiaParser<'a> {
    input: &'a str
}

impl<'a> FormDiaParser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        if let Ok((input, _)) = comment_at_start_of_input(input) {
            Self { input }
        } else {
            Self { input }
        }
    }
}

impl<'a> Iterator for FormDiaParser<'a> {
    type Item = Result<Diagram, ImportError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (input, _) = space_or_comment(self.input).unwrap();
        if input.is_empty() {
            return None
        };
        match dia(input) {
            Ok((input, dia)) => {
                self.input = input;
                Some(Ok(dia))
            },
            Err(err) => Some(Err(err.into()))
        }
    }
}

impl<'a> std::convert::From<nom::Err<nom::error::Error<&'a str>>> for ImportError<'a> {
    fn from(e: nom::Err<nom::error::Error<&'a str>>) -> Self {
        ImportError::ParseError(e)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ImportError<'a> {
    #[error("{0}")]
    ParseError(nom::Err<nom::error::Error<&'a str>>)
}

const PROP_TAG: &str = "prop";
const VX_TAG: &str = "vx";
const DIA_NAME_TAG: &str = "dia";

fn dia(input: &str) -> IResult<&str, Diagram> {
    use DiaFact::*;

    let (rest, (sign, pref, first_el, elements)) = tuple((
        terminated(opt(sign), space_or_comment),
        opt(terminated(rational32, tuple((space_or_comment, char('*'), space_or_comment)))),
        dia_fact,
        many0(preceded(tuple((space_or_comment, char('*'), space_or_comment)), dia_fact))
    ))(input)?;
    let mut prefactor = pref.unwrap_or_else(|| 1.into());
    if sign == Some(Sign::Minus) {
        prefactor *= 1;
    }
    let mut name = String::new();
    let mut vertices = Vec::new();
    let mut propagators = Vec::new();
    for f in once(first_el).chain(elements.into_iter()) {
        match f {
            Prop(p) => propagators.push(p),
            Vx(v) => vertices.push(v),
            Name(n) => name = n
        };
    }
    let dia = Diagram::builder()
        .prefactor(prefactor)
        .name(name)
        .vertices(vertices)
        .propagators(propagators)
        .build();
    Ok((rest, dia))
}

fn dia_fact(input: &str) -> IResult<&str, DiaFact> {
    use DiaFact::*;
    if let Ok((rest, prop)) = prop(input) {
        Ok((rest, Prop(prop)))
    } else if let Ok((rest, vx)) = vx(input) {
        Ok((rest, Vx(vx)))
    } else {
        let (rest, dia) = dia_name(input)?;
        Ok((rest, Name(dia.to_owned())))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum DiaFact {
    Prop(Propagator), Vx(Vertex), Name(String),
}

fn rational32(input: &str) -> IResult<&str, Rational32> {
    let (rest, (num, den)) = pair(
        i32,
        opt(
            preceded(tuple((space_or_comment, char('/'), space_or_comment)), i32)
        )
    )(input)?;
    let den = den.unwrap_or(1);
    Ok((rest, Rational32::new(num, den)))
}

fn dia_name(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = pair(tag(DIA_NAME_TAG), char('('))(input)?;
    let mut n_open_brackets = 1;
    let mut len = 0;
    while n_open_brackets > 0 {
        if let Some(next) = rest.find(|c| c == '(' || c == ')') {
            len += next + 1;
            if rest.as_bytes()[next] == b'(' {
                n_open_brackets += 1;
            } else {
                n_open_brackets -= 1;
            }
            rest = &rest[next+1..];
        } else {
            let input = String::from_iter(input.chars().take(20));
            panic!("Unmatched open bracket here: {input}");
        }
    }
    let start = DIA_NAME_TAG.len() + 1;
    let end = start + len - 1;
    Ok((rest, &input[start..end]))
}

fn prop(input: &str) -> IResult<&str, Propagator> {
    let (rest, (from, to, field)) = tuple((
        delimited(tuple((tag(PROP_TAG), char('('), space_or_comment)), u32, space_or_comment),
        delimited(pair(char(','), space_or_comment), u32, space_or_comment),
        delimited(pair(char(','), space_or_comment), field, pair(space_or_comment, char(')')))
    ))(input)?;

    let prop = Propagator::builder()
        .from(from)
        .to(to)
        .field(field)
        .build();
    Ok((rest, prop))
}

fn vx(input: &str) -> IResult<&str, Vertex> {
    let (rest, (id, fields, _)) = tuple((
        delimited(tuple((tag(VX_TAG), char('('), space_or_comment)), u32, space_or_comment),
        many0(delimited(pair(char(','), space_or_comment), field, space_or_comment)),
        char(')')
    ))(input)?;
    let vx = Vertex::builder()
        .id(id)
        .fields(fields)
        .build();

    Ok((rest, vx))
}

fn field(input: &str) -> IResult<&str, Field> {
    let (rest, name) = var(input)?;
    let (rest, (id, momentum)) = delimited(
        preceded(char('('), space_or_comment),
        separated_pair(
            i32,
            delimited(space_or_comment, char(','), space_or_comment),
            momentum
        ),
        preceded(space_or_comment, char(')'))
    )(rest)?;
    let field = Field::builder()
        .name(name.to_owned())
        .momentum(momentum.to_owned())
        .id(id)
        .build();
    Ok((rest, field))
}

// TODO: allow [name]
fn var(input: &str) -> IResult<&str, &str> {
    recognize(tuple((alpha1, alphanumeric0)))(input)
}

fn u32(input: &str) -> IResult<&str, u32> {
    let (rest, num) = digit1(input)?;
    Ok((rest, num.parse().unwrap()))
}

fn i32(input: &str) -> IResult<&str, i32> {
    let (rest, (sign, num)) = pair(opt(terminated(sign, space_or_comment)), digit1)(input)?;
    let mut num = num.parse().unwrap();
    if sign == Some(Sign::Minus) {
        num *= -1;
    }
    Ok((rest, num))
}

fn momentum(input: &str) -> IResult<&str, Momentum> {
    use Sign::*;
    use TermOrZero::*;
    let (rest, (sign, first_term, terms)) = tuple((
        opt(sign), preceded(space_or_comment, term_or_zero),
        many0(pair(
            preceded(space_or_comment, sign),
            preceded(space_or_comment, term_or_zero),
        ))
    ))(input)?;
    let sign = sign.unwrap_or(Plus);
    let p = Momentum::from_iter(
        once((sign, first_term))
            .chain(terms.into_iter())
            .filter_map(
                |(s, term)| if let Term(term) = term {
                    Some(
                        if s == Minus { -term } else { term }
                    )
                } else {
                    None
                }
            )
    );
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
    Plus, Minus
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum TermOrZero {
    Zero,
    Term(Term)
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
        opt(terminated(u32, pair(space_or_comment, char('*')))),
        space_or_comment,
        var
    )(input)?;
    let sym = Symbol::new_unchecked(sym);
    let term = if let Some(coeff) = coeff {
        Term::new(coeff as i32, sym)
    } else {
        Term::new_from_sym(sym)
    };
    Ok((rest, term))
}

fn space_or_comment(input: &str) -> IResult<&str, &str> {
    recognize(many0(alt((space1, comment, line_ending))))(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(tuple((line_ending, char('*'), take_until("\n"))))(input)
}

fn comment_at_start_of_input(input: &str) -> IResult<&str, &str> {
    recognize(tuple((char('*'), take_until("\n"))))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols;

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
        assert_eq!(t, Term::new_from_sym(p));

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

    #[test]
    fn parse_field() {
        let _ = field("phi(1, p1)").unwrap();

        let _ = field("Psi(2,l1-q1)").unwrap();

        let _ = field("Psi(-2,-q)").unwrap();
    }

    #[test]
    fn parse_prop() {
        let _ = prop("prop(2,1,psi(1,-l1))").unwrap();
    }

    #[test]
    fn parse_vx() {
        let _ = vx("vx(3,Psi(-2,-q),phi(4,l1+q),psi(2,-l1+q1))").unwrap();
    }

    #[test]
    fn parse_name() {
        let (_rest, dia) = dia_name("dia(1)").unwrap();
        assert_eq!(dia, "1");

        let (_rest, dia) = dia_name("dia(1*(2+3))").unwrap();
        assert_eq!(dia, "1*(2+3)");
    }

    #[test]
    fn parse_dias_form() {
        let mut dias = FormDiaParser::new(DIAS_FORM);
        for i in 1..=11 {
            let dia = dias.next().unwrap().unwrap();
            assert_eq!(dia.name, i.to_string());
        }
        assert!(dias.next().is_none())
    }

    #[test]
    fn parse_dias_qgraf() {
        let mut dias = FormDiaParser::new(DIAS_QGRAF);
        for i in 1..=11 {
            let dia = dias.next().unwrap().unwrap();
            assert_eq!(dia.name, i.to_string());
        }
        assert!(dias.next().is_none())
    }

    const DIAS_FORM: &str = "
       +
         dia(1)
         *vx(1,Psi(1,-l1),phi(2, - q + l1),psi(-3,q))
         *vx(2,Psi(-2,-q),phi(4,q + l2),psi(3,-l2))
         *vx(3,Psi(3,l2),phi(5, - l1 - l2),psi(1,l1))
         *vx(4,phi(-1,0),phi(2,q - l1),phi(4, - q - l2),phi(5,l1 + l2))
         *prop(1,3,psi(1,l1))
         *prop(3,2,psi(3,-l2))
         *prop(4,1,phi(2, - q + l1))
         *prop(4,2,phi(4,q + l2))
         *prop(4,3,phi(5, - l1 - l2))
       +
         1/2
         *dia(2)
         *vx(1,Psi(2,l1),phi(-1,0),psi(1,-l1))
         *vx(2,Psi(1,l1),phi(3, - q - l1),psi(-3,q))
         *vx(3,Psi(-2,-q),phi(4,q + l1),psi(2,-l1))
         *vx(4,phi(3,q + l1),phi(4, - q - l1),phi(5,l2),phi(5,-l2))
         *prop(1,3,psi(2,-l1))
         *prop(2,1,psi(1,-l1))
         *prop(4,2,phi(3, - q - l1))
         *prop(4,3,phi(4,q + l1))
         *prop(4,4,phi(5,l2))
       +
         dia(3)
         *vx(1,Psi(2,l1),phi(-1,0),psi(1,-l1))
         *vx(2,Psi(3,-l2),phi(4, - q + l2),psi(-3,q))
         *vx(3,Psi(-2,-q),phi(5,l1 + l2),psi(6,q - l1 - l2))
         *vx(4,Psi(1,l1),phi(5, - l1 - l2),psi(3,l2))
         *vx(5,Psi(6, - q + l1 + l2),phi(4,q - l2),psi(2,-l1))
         *prop(1,5,psi(2,-l1))
         *prop(2,4,psi(3,l2))
         *prop(4,1,psi(1,-l1))
         *prop(4,3,phi(5,l1 + l2))
         *prop(5,2,phi(4, - q + l2))
         *prop(5,3,psi(6,q - l1 - l2))
       +
         dia(4)
         *vx(1,Psi(2,l1),phi(-1,0),psi(1,-l1))
         *vx(2,Psi(1,l1),phi(3, - q - l1),psi(-3,q))
         *vx(3,Psi(-2,-q),phi(4,-l2),psi(5,q + l2))
         *vx(4,Psi(6,l1 - l2),phi(4,l2),psi(2,-l1))
         *vx(5,Psi(5, - q - l2),phi(3,q + l1),psi(6, - l1 + l2))
         *prop(1,4,psi(2,-l1))
         *prop(2,1,psi(1,-l1))
         *prop(4,3,phi(4,-l2))
         *prop(4,5,psi(6, - l1 + l2))
         *prop(5,2,phi(3, - q - l1))
         *prop(5,3,psi(5,q + l2))
       +
         dia(5)
         *vx(1,Psi(1,-l1),phi(-1,0),psi(2,l1))
         *vx(2,Psi(-2,-q),phi(3,q - l1),psi(1,l1))
         *vx(3,Psi(5, - q + l2),phi(4,-l2),psi(-3,q))
         *vx(4,Psi(2,-l1),phi(4,l2),psi(6,l1 - l2))
         *vx(5,Psi(6, - l1 + l2),phi(3, - q + l1),psi(5,q - l2))
         *prop(1,2,psi(1,l1))
         *prop(3,5,psi(5,q - l2))
         *prop(4,1,psi(2,l1))
         *prop(4,3,phi(4,-l2))
         *prop(5,2,phi(3,q - l1))
         *prop(5,4,psi(6,l1 - l2))
       +
         dia(6)
         *vx(1,Psi(2, - q + l1),phi(1,-l1),psi(-3,q))
         *vx(2,Psi(-2,-q),phi(1,l1),psi(3,q - l1))
         *vx(3,Psi(5,l2),phi(-1,0),psi(4,-l2))
         *vx(4,Psi(4,l2),phi(6, - q + l1 - l2),psi(2,q - l1))
         *vx(5,Psi(3, - q + l1),phi(6,q - l1 + l2),psi(5,-l2))
         *prop(1,4,psi(2,q - l1))
         *prop(2,1,phi(1,-l1))
         *prop(3,5,psi(5,-l2))
         *prop(4,3,psi(4,-l2))
         *prop(5,2,psi(3,q - l1))
         *prop(5,4,phi(6, - q + l1 - l2))
       -
         dia(7)
         *vx(1,Psi(1,-l1),phi(2, - q + l1),psi(-3,q))
         *vx(2,Psi(-2,-q),phi(3,q - l1),psi(1,l1))
         *vx(3,Psi(5,l2),phi(-1,0),psi(4,-l2))
         *vx(4,Psi(4,l2),phi(2,q - l1),psi(6, - q + l1 - l2))
         *vx(5,Psi(6,q - l1 + l2),phi(3, - q + l1),psi(5,-l2))
         *prop(1,2,psi(1,l1))
         *prop(3,5,psi(5,-l2))
         *prop(4,1,phi(2, - q + l1))
         *prop(4,3,psi(4,-l2))
         *prop(5,2,phi(3,q - l1))
         *prop(5,4,psi(6, - q + l1 - l2))
       -
         dia(8)
         *vx(1,Psi(1,-l1),phi(2, - q + l1),psi(-3,q))
         *vx(2,Psi(-2,-q),phi(3,q - l1),psi(1,l1))
         *vx(3,Psi(4,-l2),phi(-1,0),psi(5,l2))
         *vx(4,Psi(6, - q + l1 - l2),phi(2,q - l1),psi(4,l2))
         *vx(5,Psi(5,-l2),phi(3, - q + l1),psi(6,q - l1 + l2))
         *prop(1,2,psi(1,l1))
         *prop(3,4,psi(4,l2))
         *prop(4,1,phi(2, - q + l1))
         *prop(4,5,psi(6,q - l1 + l2))
         *prop(5,2,phi(3,q - l1))
         *prop(5,3,psi(5,l2))
       -
         dia(9)
         *vx(1,Psi(2,l1),phi(-1,0),psi(1,-l1))
         *vx(2,Psi(1,l1),phi(3, - q - l1),psi(-3,q))
         *vx(3,Psi(-2,-q),phi(4,q + l1),psi(2,-l1))
         *vx(4,Psi(6,-l2),phi(3,q + l1),psi(5, - q - l1 + l2))
         *vx(5,Psi(5,q + l1 - l2),phi(4, - q - l1),psi(6,l2))
         *prop(1,3,psi(2,-l1))
         *prop(2,1,psi(1,-l1))
         *prop(4,2,phi(3, - q - l1))
         *prop(4,5,psi(6,l2))
         *prop(5,3,phi(4,q + l1))
         *prop(5,4,psi(5, - q - l1 + l2))
       +
         dia(10)
         *vx(1,Psi(1,-l1),phi(2, - q + l1),psi(-3,q))
         *vx(2,Psi(3,-l1),phi(-1,0),psi(1,l1))
         *vx(3,Psi(-2,-q),phi(2,q - l1),psi(4,l1))
         *vx(4,Psi(6,-l2),phi(5, - l1 + l2),psi(3,l1))
         *vx(5,Psi(4,-l1),phi(5,l1 - l2),psi(6,l2))
         *prop(1,2,psi(1,l1))
         *prop(2,4,psi(3,l1))
         *prop(3,1,phi(2, - q + l1))
         *prop(4,5,psi(6,l2))
         *prop(5,3,psi(4,l1))
         *prop(5,4,phi(5, - l1 + l2))
       +
         dia(11)
         *vx(1,Psi(-2,-q),phi(2,q + l1),psi(1,-l1))
         *vx(2,Psi(1,l1),phi(-1,0),psi(3,-l1))
         *vx(3,Psi(4,l1),phi(2, - q - l1),psi(-3,q))
         *vx(4,Psi(3,l1),phi(5, - l1 + l2),psi(6,-l2))
         *vx(5,Psi(6,l2),phi(5,l1 - l2),psi(4,-l1))
         *prop(2,1,psi(1,-l1))
         *prop(3,1,phi(2,q + l1))
         *prop(3,5,psi(4,-l1))
         *prop(4,2,psi(3,-l1))
         *prop(5,4,psi(6,-l2))
         *prop(5,4,phi(5, - l1 + l2))
";
    const DIAS_QGRAF: &str = "
*--#[ d1:
*
     +1*dia(1)
    *vx(1,Psi(1,-l1),phi(2,l1-q),psi(-3,q))
    *vx(2,Psi(-2,-q),phi(4,l2+q),psi(3,-l2))
    *vx(3,Psi(3,l2),phi(5,-l1-l2),psi(1,l1))
    *vx(4,phi(-1,q1),phi(2,-l1+q),phi(4,-l2-q),phi(5,l1+l2))

     *prop(1,3,psi(1,l1))
     *prop(4,1,phi(2,l1-q))
     *prop(3,2,psi(3,-l2))
     *prop(4,2,phi(4,l2+q))
     *prop(4,3,phi(5,-l1-l2))

*
*--#] d1:
*--#[ d2:
*
     +1/2*dia(2)
    *vx(1,Psi(2,l1-q1),phi(-1,q1),psi(1,-l1))
    *vx(2,Psi(1,l1),phi(3,-l1-q),psi(-3,q))
    *vx(3,Psi(-2,-q),phi(4,l1+q),psi(2,-l1+q1))
    *vx(4,phi(3,l1+q),phi(4,-l1-q),phi(5,l2),phi(5,-l2))

     *prop(2,1,psi(1,-l1))
     *prop(1,3,psi(2,-l1+q1))
     *prop(4,2,phi(3,-l1-q))
     *prop(4,3,phi(4,l1+q))
     *prop(4,4,phi(5,l2))

*
*--#] d2:
*--#[ d3:
*
     +1*dia(3)
    *vx(1,Psi(2,l1-q1),phi(-1,q1),psi(1,-l1))
    *vx(2,Psi(3,-l2),phi(4,l2-q),psi(-3,q))
    *vx(3,Psi(-2,-q),phi(5,l1+l2),psi(6,-l1-l2+q))
    *vx(4,Psi(1,l1),phi(5,-l1-l2),psi(3,l2))
    *vx(5,Psi(6,l1+l2-q),phi(4,-l2+q),psi(2,-l1+q1))

     *prop(4,1,psi(1,-l1))
     *prop(1,5,psi(2,-l1+q1))
     *prop(2,4,psi(3,l2))
     *prop(5,2,phi(4,l2-q))
     *prop(4,3,phi(5,l1+l2))
     *prop(5,3,psi(6,-l1-l2+q))

*
*--#] d3:
*--#[ d4:
*
     +1*dia(4)
    *vx(1,Psi(2,l1-q1),phi(-1,q1),psi(1,-l1))
    *vx(2,Psi(1,l1),phi(3,-l1-q),psi(-3,q))
    *vx(3,Psi(-2,-q),phi(4,-l2),psi(5,l2+q))
    *vx(4,Psi(6,l1-l2-q1),phi(4,l2),psi(2,-l1+q1))
    *vx(5,Psi(5,-l2-q),phi(3,l1+q),psi(6,-l1+l2+q1))

     *prop(2,1,psi(1,-l1))
     *prop(1,4,psi(2,-l1+q1))
     *prop(5,2,phi(3,-l1-q))
     *prop(4,3,phi(4,-l2))
     *prop(5,3,psi(5,l2+q))
     *prop(4,5,psi(6,-l1+l2+q1))

*
*--#] d4:
*--#[ d5:
*
     +1*dia(5)
    *vx(1,Psi(1,-l1),phi(-1,q1),psi(2,l1-q1))
    *vx(2,Psi(-2,-q),phi(3,-l1+q),psi(1,l1))
    *vx(3,Psi(5,l2-q),phi(4,-l2),psi(-3,q))
    *vx(4,Psi(2,-l1+q1),phi(4,l2),psi(6,l1-l2-q1))
    *vx(5,Psi(6,-l1+l2+q1),phi(3,l1-q),psi(5,-l2+q))

     *prop(1,2,psi(1,l1))
     *prop(4,1,psi(2,l1-q1))
     *prop(5,2,phi(3,-l1+q))
     *prop(4,3,phi(4,-l2))
     *prop(3,5,psi(5,-l2+q))
     *prop(5,4,psi(6,l1-l2-q1))

*
*--#] d5:
*--#[ d6:
*
     +1*dia(6)
    *vx(1,Psi(2,l1-q),phi(1,-l1),psi(-3,q))
    *vx(2,Psi(-2,-q),phi(1,l1),psi(3,-l1+q))
    *vx(3,Psi(5,l2-q1),phi(-1,q1),psi(4,-l2))
    *vx(4,Psi(4,l2),phi(6,l1-l2-q),psi(2,-l1+q))
    *vx(5,Psi(3,l1-q),phi(6,-l1+l2+q),psi(5,-l2+q1))

     *prop(2,1,phi(1,-l1))
     *prop(1,4,psi(2,-l1+q))
     *prop(5,2,psi(3,-l1+q))
     *prop(4,3,psi(4,-l2))
     *prop(3,5,psi(5,-l2+q1))
     *prop(5,4,phi(6,l1-l2-q))

*
*--#] d6:
*--#[ d7:
*
     -1*dia(7)
    *vx(1,Psi(1,-l1),phi(2,l1-q),psi(-3,q))
    *vx(2,Psi(-2,-q),phi(3,-l1+q),psi(1,l1))
    *vx(3,Psi(5,l2-q1),phi(-1,q1),psi(4,-l2))
    *vx(4,Psi(4,l2),phi(2,-l1+q),psi(6,l1-l2-q))
    *vx(5,Psi(6,-l1+l2+q),phi(3,l1-q),psi(5,-l2+q1))

     *prop(1,2,psi(1,l1))
     *prop(4,1,phi(2,l1-q))
     *prop(5,2,phi(3,-l1+q))
     *prop(4,3,psi(4,-l2))
     *prop(3,5,psi(5,-l2+q1))
     *prop(5,4,psi(6,l1-l2-q))

*
*--#] d7:
*--#[ d8:
*
     -1*dia(8)
    *vx(1,Psi(1,-l1),phi(2,l1-q),psi(-3,q))
    *vx(2,Psi(-2,-q),phi(3,-l1+q),psi(1,l1))
    *vx(3,Psi(4,-l2),phi(-1,q1),psi(5,l2-q1))
    *vx(4,Psi(6,l1-l2-q),phi(2,-l1+q),psi(4,l2))
    *vx(5,Psi(5,-l2+q1),phi(3,l1-q),psi(6,-l1+l2+q))

     *prop(1,2,psi(1,l1))
     *prop(4,1,phi(2,l1-q))
     *prop(5,2,phi(3,-l1+q))
     *prop(3,4,psi(4,l2))
     *prop(5,3,psi(5,l2-q1))
     *prop(4,5,psi(6,-l1+l2+q))

*
*--#] d8:
*--#[ d9:
*
     -1*dia(9)
    *vx(1,Psi(2,l1-q1),phi(-1,q1),psi(1,-l1))
    *vx(2,Psi(1,l1),phi(3,-l1-q),psi(-3,q))
    *vx(3,Psi(-2,-q),phi(4,l1+q),psi(2,-l1+q1))
    *vx(4,Psi(6,-l2),phi(3,l1+q),psi(5,-l1+l2-q))
    *vx(5,Psi(5,l1-l2+q),phi(4,-l1-q),psi(6,l2))

     *prop(2,1,psi(1,-l1))
     *prop(1,3,psi(2,-l1+q1))
     *prop(4,2,phi(3,-l1-q))
     *prop(5,3,phi(4,l1+q))
     *prop(5,4,psi(5,-l1+l2-q))
     *prop(4,5,psi(6,l2))

*
*--#] d9:
*--#[ d10:
*
     +1*dia(10)
    *vx(1,Psi(1,-l1),phi(2,l1-q),psi(-3,q))
    *vx(2,Psi(3,-l1-q1),phi(-1,q1),psi(1,l1))
    *vx(3,Psi(-2,-q),phi(2,-l1+q),psi(4,l1+q1))
    *vx(4,Psi(6,-l2),phi(5,-l1+l2-q1),psi(3,l1+q1))
    *vx(5,Psi(4,-l1-q1),phi(5,l1-l2+q1),psi(6,l2))

     *prop(1,2,psi(1,l1))
     *prop(3,1,phi(2,l1-q))
     *prop(2,4,psi(3,l1+q1))
     *prop(5,3,psi(4,l1+q1))
     *prop(5,4,phi(5,-l1+l2-q1))
     *prop(4,5,psi(6,l2))

*
*--#] d10:
*--#[ d11:
*
     +1*dia(11)
    *vx(1,Psi(-2,-q),phi(2,l1+q),psi(1,-l1))
    *vx(2,Psi(1,l1),phi(-1,q1),psi(3,-l1-q1))
    *vx(3,Psi(4,l1+q1),phi(2,-l1-q),psi(-3,q))
    *vx(4,Psi(3,l1+q1),phi(5,-l1+l2-q1),psi(6,-l2))
    *vx(5,Psi(6,l2),phi(5,l1-l2+q1),psi(4,-l1-q1))

     *prop(2,1,psi(1,-l1))
     *prop(3,1,phi(2,l1+q))
     *prop(4,2,psi(3,-l1-q1))
     *prop(3,5,psi(4,-l1-q1))
     *prop(5,4,phi(5,-l1+l2-q1))
     *prop(5,4,psi(6,-l2))

*
*--#] d11:
*
* end
*
";
}
