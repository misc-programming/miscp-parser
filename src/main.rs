#[derive(Debug, PartialEq, Clone, Copy)]
enum Token {
    Digit(u32),
    Plus,
    Minus,
    Times,
    Division,
    Lparen,
    Rparen,
}

#[derive(Debug, PartialEq)]
enum AST {
    Digit(u32),
    Sum(Box<AST>, Box<AST>),
    Diff(Box<AST>, Box<AST>),
    Prod(Box<AST>, Box<AST>),
    Div(Box<AST>, Box<AST>),
    Negative(Box<AST>),
}

impl AST {
    fn value(self) -> i32 {
        match self {
            AST::Digit(d) => d as i32,
            AST::Sum(l, r) => l.value() + r.value(),
            AST::Diff(l, r) => l.value() - r.value(),
            AST::Prod(l, r) => l.value() * r.value(),
            AST::Div(l, r) => l.value() / r.value(),
            AST::Negative(d) => -d.value()
        }
    }
}

fn lex(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();

    for c in input.chars() {
        match c {
            d @ '0'..='9' => {
                let value = d.to_string().parse::<u32>().map_err(|_| format!("{} is not a digit", d))?;
                tokens.push(Token::Digit(value))
            }
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Times),
            '/' => tokens.push(Token::Division),
            '(' => tokens.push(Token::Lparen),
            ')' => tokens.push(Token::Rparen),
            ' ' => continue,
            _ => return Err(format!("Unexpected char {}", c))
        }
    }
    Ok(tokens)
}


fn read_token(input: &Vec<Token>, position: usize) -> Result<&Token, String> {
    match input.get(position) {
        None => Err(format!("Cannot read token at position {}", position)),
        Some(token) => Ok(token)
    }
}

fn read_h(input: &Vec<Token>, position: usize) -> Result<(AST, usize), String> {
    let token = read_token(input, position)?;
    match token {
        Token::Digit(value) => {
            Ok((AST::Digit(*value), position + 1))
        }
        Token::Lparen => {
            let node_and_position = read_e(input, position + 1)?;
            let rparen = read_token(input, node_and_position.1)?;
            match rparen {
                Token::Rparen => Ok((node_and_position.0, node_and_position.1 + 1)),
                _ => return Err("Missing left parenthesis".to_string())
            }
        }
        _ => return Err(format!("Unexpected token at position {}", position))
    }
}

fn read_g(input: &Vec<Token>, position: usize) -> Result<(AST, usize), String> {
    let operators = vec![Token::Minus];

    let token = read_token(input, position)?;

    if operators.contains(&token) {
        let node = read_h(input, position + 1)?;
        Ok((AST::Negative(Box::from(node.0)), node.1))
    } else {
        read_h(input, position)
    }
}

fn read_f(input: &Vec<Token>, position: usize) -> Result<(AST, usize), String> {
    let mut node_and_position = read_g(input, position)?;

    let operators = vec![Token::Times, Token::Division];

    while let Some(current) = input.get(node_and_position.1) {
        if operators.contains(&current) {
            node_and_position = match current {
                Token::Times => {
                    let right = read_h(input, node_and_position.1 + 1)?;
                    (AST::Prod(Box::from(node_and_position.0), Box::from(right.0)), right.1)
                }
                Token::Division => {
                    let right = read_h(input, node_and_position.1 + 1)?;
                    (AST::Div(Box::from(node_and_position.0), Box::from(right.0)), right.1)
                }
                _ => return Err("Unexpected operator".to_string())
            }
        } else {
            break;
        }
    }
    Ok(node_and_position)
}

fn read_e(input: &Vec<Token>, position: usize) -> Result<(AST, usize), String> {
    let mut node_and_position = read_f(input, position)?;

    let operators = vec![Token::Plus, Token::Minus];

    while let Some(current) = input.get(node_and_position.1) {
        if operators.contains(&current) {
            node_and_position = match current {
                Token::Plus => {
                    let right = read_f(input, node_and_position.1 + 1)?;
                    (AST::Sum(Box::from(node_and_position.0), Box::from(right.0)), right.1)
                }
                Token::Minus => {
                    let right = read_f(input, node_and_position.1 + 1)?;
                    (AST::Diff(Box::from(node_and_position.0), Box::from(right.0)), right.1)
                }
                _ => return Err("Unexpected operator".to_string())
            }
        } else {
            break;
        }
    }
    Ok(node_and_position)
}

fn parse(input: &Vec<Token>) -> Result<AST, String> {
    read_e(input, 0).map(|r| r.0)
}

fn interpret(operation: &str) -> Result<i32, String> {
    let tokens = lex(operation)?;
    let parser = parse(&tokens)?;
    Ok(parser.value())
}

fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let operation = "-3 + 4 * 5 / 2 - (3 + 2)";

        let expected = vec![Token::Minus, Token::Digit(3), Token::Plus, Token::Digit(4),
                            Token::Times, Token::Digit(5), Token::Division, Token::Digit(2),
                            Token::Minus, Token::Lparen, Token::Digit(3), Token::Plus,
                            Token::Digit(2), Token::Rparen
        ];

        let tokens = lex(operation).unwrap();

        assert_eq!(tokens, expected);
    }


    #[test]
    fn test_parser() {
        let operation = "-3 + 4 * 5 / 2 - (3 + 2)";

        let expected =
            AST::Diff(
                Box::from(
                    AST::Sum(
                        Box::from(AST::Negative(Box::from(AST::Digit(3)))),
                        Box::from(
                            AST::Div(Box::from(
                                AST::Prod(
                                    Box::from(AST::Digit(4)),
                                    Box::from(AST::Digit(5)),
                                )),
                                     Box::from(AST::Digit(2))),
                        )),
                ),
                Box::from(
                    AST::Sum(
                        Box::from(AST::Digit(3)),
                        Box::from(AST::Digit(2)),
                    )
                ),
            );

        let tokens = lex(operation).unwrap();
        let parser = parse(&tokens).unwrap();

        assert_eq!(parser, expected);
    }

    #[test]
    fn test_interpreter() {
        let operation = "-3 + 4 * 5 - (3 + 2) * ( 7 - 5 )";
        assert_eq!(interpret(operation).unwrap(), 7);
    }
}