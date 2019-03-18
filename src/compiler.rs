use crate::lambda::*;

pub fn compile(input: &str) -> Result<Exp<String>, String> {
    // TEMP: one char tokens, ignore whitespace
    let mut tokens = lex(input);
    preprocess(&mut tokens);
    // make tokens immutable
    let tokens = tokens.iter().map(String::as_str).collect::<Vec<_>>();
    parse(&tokens)
}

// TODO: maybe use multi-char tokens
pub fn lex(input: &str) -> Vec<String> {
    // one character tokens initially
    input
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| c.to_string())
        .collect::<Vec<_>>()
}

// TODO: abstraction multi-var shorthand, maybe verify syntax
pub fn preprocess(tokens: &mut Vec<String>) {
    // insert parens around every lambda expression
    // where we search from
    let mut begin = 0;
    while let Some(mut i) = tokens[begin..].iter().position(|t| *t == r"\") {
        i += begin;
        // skip the "(\"
        begin = i + 2;
        // insert an opening brace
        tokens.insert(i, String::from("("));
        tokens.insert(
            find_matching_brace(tokens[i..].iter().map(String::as_str), ("(", ")"))
                .map_or_else(|| tokens.len(), |v| v + i),
            String::from(")"),
        );
    }
}

// TODO: alias expressions with assignments to names
pub fn parse(tokens: &[&str]) -> Result<Exp<String>, String> {
    let mut ast = generate_ast(tokens)?;
    ast.make_identifiers_unique(&mut Default::default(), &mut Default::default());
    Ok(ast)
}

pub fn generate_ast(tokens: &[&str]) -> Result<Exp<String>, String> {
    // TODO: in the future, use bnf crate
    match tokens.len() {
        0 => Err(String::from("Can't parse zero-length list of tokens")),
        // create variable expression
        1 => Ok(Exp::Var(Ident::without_id(String::from(tokens[0])))),
        _ => {
            if tokens.len() > 2
                && tokens[0] == "("
                && find_matching_brace(tokens.iter().cloned(), ("(", ")"))
                    .map_or(false, |i| i == tokens.len() - 1)
            {
                // remove outer layer of parens
                generate_ast(&tokens[1..(tokens.len() - 1)])
            } else if tokens.len() > 3 && tokens[0] == r"\" && tokens[2] == "." {
                // identified abstraction
                Ok(Exp::Abs(
                    Ident::without_id(String::from(tokens[1])),
                    Box::new(generate_ast(&tokens[3..])?),
                ))
            } else if tokens[tokens.len() - 1] == ")"
                || tokens[tokens.len() - 1].chars().all(char::is_alphanumeric)
            {
                // identified abstraction
                // ensure that an opening brace has been found
                let sep = tokens.len()
                    - 1
                    - find_matching_brace(tokens.iter().rev().cloned(), (")", "("))
                        .ok_or_else(|| format!("No opening brace found: {:?}", tokens))?;
                let split = tokens.split_at(sep);
                Ok(Exp::App(
                    Box::new(generate_ast(split.0)?),
                    Box::new(generate_ast(split.1)?),
                ))
            } else {
                Err(format!("Could not parse: {:?}", tokens))
            }
        }
    }
}

/// Finds the index of the closing parenthesis, given that `v[0]` is `(`.
/// If it isn't `(`, `Some(0)` is returned.
/// If the end of the vector is reached without finding a closing brace,
/// `None` is returned.
pub fn find_matching_brace<'a>(
    v: impl Iterator<Item = &'a str>,
    symbols: (&str, &str),
) -> Option<usize> {
    let mut depth = 0;
    for (i, s) in v.enumerate() {
        if s == symbols.0 {
            depth += 1;
        } else if s == symbols.1 {
            depth -= 1;
        }
        if depth == 0 {
            return Some(i);
        }
    }
    None
}
