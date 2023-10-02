use crate::lex::{Operator, Token};

#[derive(Clone, Debug, PartialEq)]
pub struct PostfixExpr {
    pub queue: Vec<Token>,
}

// https://en.wikipedia.org/wiki/Shunting_yard_algorithm
pub fn parse(tokens: Vec<Token>) -> PostfixExpr {
    debug_assert!(is_valid_infix_expr(&tokens));

    let mut output_queue: Vec<Token> = Vec::new();
    let mut op_stack: Vec<Operator> = Vec::new();

    for token in tokens {
        match &token {
            Token::Number(_) => {
                output_queue.push(token);
            },
            Token::Operator(op) => {
                while let Some(last_op) = op_stack.last() {
                    if !(precedence(last_op) > precedence(op)
                        || (precedence(last_op) == precedence(op) && associativity(op) == Associativity::Left)) {
                        break;
                    }
                    output_queue.push(Token::Operator(op_stack.pop().unwrap()));
                }
                op_stack.push(*op);
            },
            _ => unreachable!("All tokens in a valid infix expression must be either a number or an operator."),
        }
    }

    // Iterate in LIFO order
    let remaining_ops = op_stack.into_iter().rev().map(Token::Operator);
    output_queue.extend(remaining_ops);

    PostfixExpr {
        queue: output_queue,
    }
}

fn is_valid_infix_expr(tokens: &Vec<Token>) -> bool {
    for (i, token) in tokens.iter().enumerate() {
        if (i % 2 == 0 && !matches!(token, Token::Number(_)))
            || (i % 2 == 1 && !matches!(token, Token::Operator(_op)))
        {
            return false;
        }
    }
    true
}

fn precedence(op: &Operator) -> usize {
    match op {
        Operator::Plus => 0,
        Operator::Minus => 0,
        Operator::Star => 1,
        Operator::Slash => 1,
        Operator::Percent => 1,
        Operator::Caret => 2,
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Associativity {
    Left,
    Right
}

fn associativity(op: &Operator) -> Associativity {
    match op {
        Operator::Plus => Associativity::Left,
        Operator::Minus => Associativity::Left,
        Operator::Star => Associativity::Left,
        Operator::Slash => Associativity::Left,
        Operator::Percent => Associativity::Left,
        Operator::Caret => Associativity::Right,
    }
}

mod tests {
    use crate::lex;

    use super::*;

    #[test]
    fn parse_expression() {
        assert_eq!(
            parse(vec![
                Token::Number(1.),
                Token::Operator(Operator::Plus),
                Token::Number(1.),
            ]),
            PostfixExpr {
                queue: vec![
                    Token::Number(1.),
                    Token::Number(1.),
                    Token::Operator(Operator::Plus),
                ]
            }
        );
        assert_eq!(
            parse(vec![
                Token::Number(1.),
                Token::Operator(Operator::Plus),
                Token::Number(2.),
                Token::Operator(Operator::Star),
                Token::Number(3.),
                Token::Operator(Operator::Minus),
                Token::Number(4.),
            ]),
            PostfixExpr {
                queue: vec![
                    Token::Number(1.),
                    Token::Number(2.),
                    Token::Number(3.),
                    Token::Operator(Operator::Star),
                    Token::Operator(Operator::Plus),
                    Token::Number(4.),
                    Token::Operator(Operator::Minus),
                ]
            }
        );
    }
}
