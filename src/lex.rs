use crate::util::*;
use crate::FLOAT_TOLERANCE;

use nom::bytes::complete::{tag};
use nom::branch::{alt};
use nom::character::complete::{one_of, satisfy, space0};
use nom::combinator::{value, map_res, recognize, opt};
use nom::error::ErrorKind;
use nom::multi::separated_list1;
use nom::multi::{many1};
use nom::number::complete::float;
use nom::sequence::preceded;
use nom::{IResult};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Operator(Operator),
    Function(Function),
    Number(f64)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Function {
    Sqrt,
}

pub fn lex(input: &str) -> Result<Vec<Token>, nom::Err<nom::error::Error<&str>>> {
    let (_, tokens) = lex_tokens(input)?;
    Ok(tokens)
}

pub fn is_valid_expression(input: &str) -> bool {
    todo!()
}

fn lex_tokens(mut input: &str) -> IResult<&str, Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    loop {
        if let Ok((inp, _)) = space0::<_, nom::error::Error<&str>>(input) {
            input = inp;
        } else {
            break;
        }
        if let Ok((inp, token)) = lex_token(input) {
            input = inp;
            tokens.push(token);
        } else {
            break;
        }
    }
    if tokens.len() > 0 {
        Ok((input, tokens))
    } else {
        Err(nom::Err::Error(nom::error::make_error(input, ErrorKind::SeparatedList)))
    }
}

fn lex_token(input: &str) -> IResult<&str, Token> {
    alt((
        lex_operator,
        lex_paren,
        // lex_builtin,
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
        value(Operator::Caret, tag("^")),
    ))(input)?;
    Ok((input, Token::Operator(op)))
}

fn lex_paren(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::LParen, tag("(")),
        value(Token::RParen, tag(")")),
    ))(input)
}

fn lex_builtin(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Function(Function::Sqrt), tag("sqrt")),
    ))(input)
}

fn lex_number(input: &str) -> IResult<&str, Token> {
    alt((
        lex_constant,
        lex_binary_literal,
        lex_hex_literal,
        lex_float_literal,
    ))(input)
}

fn lex_constant(input: &str) -> IResult<&str, Token> {
    let (input, n) = alt((
        value(std::f64::consts::PI, alt((tag("pi"), tag("π")))),
        value(std::f64::consts::E, tag("e")),
    ))(input)?;
    Ok((input, Token::Number(n)))
}

fn lex_binary_literal(input: &str) -> IResult<&str, Token> {
    let (input, digits) = preceded(
        tag("0b"),
        recognize(many1(one_of("01")))
    )(input)?;
    let mut n: u64 = 0;
    for d in digits.chars() {
        n <<= 1;
        if d == '1' {
            n |= 1;
        }
    }
    Ok((input, Token::Number(n as f64)))
}

fn lex_hex_literal(input: &str) -> IResult<&str, Token> {
    let (input, digits) = preceded(
        tag("0x"),
        recognize(many1(satisfy(is_hex_digit)))
    )(input)?;
    let n = u64::from_str_radix(digits, 16).unwrap();
    Ok((input, Token::Number(n as f64)))
}

fn is_hex_digit(c: char) -> bool {
    (c >= '0' && c <= '9') ||
    (c >= 'A' && c <= 'F') ||
    (c >= 'a' && c <= 'f')
}

fn lex_float_literal(input: &str) -> IResult<&str, Token> {
    map_res(
        float,
        |n| Ok::<Token, ()>(Token::Number(n as f64))
    )(input)
}

mod tests {
    use super::*;

    fn assert_lexes_to(input: &str, expected: Vec<Token>) {
        let result = lex(input).expect(format!("Failed to parse input \"{input}\"").as_str());
        if result.len() != expected.len() {
            panic!("Incorrectly lexed input.\nActual: {result:?}\nExpected: {expected:?}");
        }
        for pair in std::iter::zip(result, expected) {
            if let (Token::Number(result), Token::Number(expected)) = pair {
                assert!(equal_within(result, expected, FLOAT_TOLERANCE));
            } else {
                assert_eq!(pair.0, pair.1);
            }
        }
    }

    #[test]
    fn lex_operator() {
        assert_lexes_to("+", vec![Token::Operator(Operator::Plus)]);
        assert_lexes_to("-", vec![Token::Operator(Operator::Minus)]);
        assert_lexes_to("*", vec![Token::Operator(Operator::Star)]);
        assert_lexes_to("/", vec![Token::Operator(Operator::Slash)]);
        assert_lexes_to("%", vec![Token::Operator(Operator::Percent)]);
        assert_lexes_to("^", vec![Token::Operator(Operator::Caret)]);
    }

    #[test]
    fn lex_number() {
        assert_lexes_to("0", vec![Token::Number(0.)]);
        assert_lexes_to("1", vec![Token::Number(1.)]);
        assert_lexes_to("11", vec![Token::Number(11.)]);
        assert_lexes_to("11e-1", vec![Token::Number(1.1)]);
        assert_lexes_to("123E-02", vec![Token::Number(1.23)]);
        assert_lexes_to("123K-01", vec![Token::Number(123.0)]);

        assert_lexes_to("0b0", vec![Token::Number(0.)]);
        assert_lexes_to("0b1", vec![Token::Number(1.)]);
        assert_lexes_to("0b1001100101", vec![Token::Number(613.)]);

        assert_lexes_to("pi", vec![Token::Number(std::f64::consts::PI)]);
        assert_lexes_to("π", vec![Token::Number(std::f64::consts::PI)]);
        assert_lexes_to("e", vec![Token::Number(std::f64::consts::E)]);
    }

    #[test]
    fn lex_flat_expressions() {
        assert_lexes_to("9+10", vec![
            Token::Number(9.),
            Token::Operator(Operator::Plus),
            Token::Number(10.),
        ]);
        assert_lexes_to(" 2-1 ", vec![
            Token::Number(2.),
            Token::Operator(Operator::Minus),
            Token::Number(1.),
        ]);
        assert_lexes_to("1+2-3*4/5^6", vec![
            Token::Number(1.),
            Token::Operator(Operator::Plus),
            Token::Number(2.),
            Token::Operator(Operator::Minus),
            Token::Number(3.),
            Token::Operator(Operator::Star),
            Token::Number(4.),
            Token::Operator(Operator::Slash),
            Token::Number(5.),
            Token::Operator(Operator::Caret),
            Token::Number(6.),
        ]);
        assert_lexes_to(" 1    + 2  -   3*4  / 5  ^6", vec![
            Token::Number(1.),
            Token::Operator(Operator::Plus),
            Token::Number(2.),
            Token::Operator(Operator::Minus),
            Token::Number(3.),
            Token::Operator(Operator::Star),
            Token::Number(4.),
            Token::Operator(Operator::Slash),
            Token::Number(5.),
            Token::Operator(Operator::Caret),
            Token::Number(6.),
        ]);
    }

    // fn lex_nested_expressions() {
    //     assert_lexes_to("sqrt(3*3 + 4*4)", vec![
    //         Token::Number(1.),
    //         Token::Operator(Operator::Plus),
    //         Token::Number(2.),
    //         Token::Operator(Operator::Minus),
    //         Token::Number(3.),
    //         Token::Operator(Operator::Star),
    //         Token::Number(4.),
    //         Token::Operator(Operator::Slash),
    //         Token::Number(5.),
    //     ]);
    // }
}