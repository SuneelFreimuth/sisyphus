use nom::bytes::complete::{tag};
use nom::branch::{alt};
use nom::combinator::{value, map_res};
use nom::multi::{many0};
use nom::number::complete::float;
use nom::{IResult};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Operator(Operator),
    Number(f64)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
}

pub fn lex(input: &str) -> Result<Vec<Token>, nom::Err<nom::error::Error<&str>>> {
    let (_, tokens) = many0(lex_token)(input)?;
    Ok(tokens)
}

fn lex_token(input: &str) -> IResult<&str, Token> {
    alt((
        lex_operator,
        lex_number,
    ))(input)
}

fn lex_operator(input: &str) -> IResult<&str, Token> {
    let (input, op) = alt((
        value(Operator::Plus, tag("+")),
        value(Operator::Minus, tag("-")),
        value(Operator::Star, tag("*")),
        value(Operator::Slash, tag("/")),
        value(Operator::Percent, tag("%")),
    ))(input)?;
    Ok((input, Token::Operator(op)))
}

fn lex_number(input: &str) -> IResult<&str, Token> {
    map_res(
        float(input),
        |n| Ok(Token::Number(n.parse::<f64>().unwrap()))
    )
}

mod tests {
    use super::*;

    #[test]
    fn lex_nothing() {
        assert_eq!(lex(""), Ok(vec![]));
    }

    #[test]
    fn lex_operator() {
        assert_eq!(lex("+"), Ok(vec![Token::Operator(Operator::Plus)]));
        assert_eq!(lex("-"), Ok(vec![Token::Operator(Operator::Minus)]));
        assert_eq!(lex("*"), Ok(vec![Token::Operator(Operator::Star)]));
        assert_eq!(lex("/"), Ok(vec![Token::Operator(Operator::Slash)]));
        assert_eq!(lex("%"), Ok(vec![Token::Operator(Operator::Percent)]));
    }

    #[test]
    fn lex_number() {
        assert_eq!(lex("0"), Ok(vec![Token::Number(0)]));
    }
}