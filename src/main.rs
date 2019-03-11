use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
enum Exp<T> {
    // variable
    Var(T),
    // abstraction
    Abs(T, Box<Exp<T>>),
    // application
    App(Box<Exp<T>>, Box<Exp<T>>),
}

impl Exp<String> {
    fn make_identifiers_unique(
        &mut self,
        symbols: &mut HashMap<String, usize>, // just use numbered ids for now
        scope: &mut HashMap<String, Vec<String>>, // pop and push ids
    ) -> Result<(), String> {
        // scope maps original variable name to unique id
        match self {
            Exp::Var(v) => {
                // try to replace with the new identifier at the top of the stack
                if let Some(identifiers) = scope.get(v) {
                    if let Some(id) = identifiers.last() {
                        *v = id.clone();
                    }
                }
            }
            Exp::Abs(v, b) => {
                // construct an empty vec if it is missing
                if !scope.contains_key(v) {
                    scope.insert(v.clone(), Default::default());
                }
                // scope in
                let id = get_unique_identifier(&v, symbols);
                scope.get_mut(v).unwrap().push(id.clone());
                // convert the body
                b.make_identifiers_unique(symbols, scope)?;
                // remove our identifier, it can never be referenced again
                let idents = scope.get_mut(v).unwrap();
                idents.pop();
                // shrink the vec if it's too big (premature optimization?)
                if idents.len() <= idents.capacity() / 2 {
                    idents.shrink_to_fit();
                }
                *v = id;
            }
            Exp::App(l, r) => {
                l.make_identifiers_unique(symbols, scope)?;
                r.make_identifiers_unique(symbols, scope)?;
            }
        };
        Ok(())
    }
}

impl<T> fmt::Display for Exp<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exp::Var(v) => write!(f, "{}", v),
            Exp::Abs(v, b) => {
                if let Exp::App(l, r) = b.as_ref() {
                    // don't wrap the application in parentheses
                    write!(f, r"(\{}.{} {})", v, l, r)
                } else {
                    write!(f, r"(\{}.{})", v, b)
                }
            }
            Exp::App(l, r) => write!(f, "({} {})", l, r),
        }
    }
}

fn main() {
    let input = r"\f.(\x.f(x x))(\x.f(x x))";

    // TEMP: one char tokens, ignore whitespace
    let mut tokens = lex(input);
    preprocess(&mut tokens);
    // make tokens immutable
    let tokens = tokens.iter().map(String::as_str).collect::<Vec<_>>();
    let program = parse(&tokens);

    println!("{}", program.unwrap());
}

fn lex(input: &str) -> Vec<String> {
    // one character tokens initially
    input
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| c.to_string())
        .collect::<Vec<_>>()
}

fn preprocess(tokens: &mut Vec<String>) {
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

fn parse(tokens: &[&str]) -> Result<Exp<String>, String> {
    let mut ast = generate_ast(tokens)?;
    ast.make_identifiers_unique(&mut Default::default(), &mut Default::default())?;
    Ok(ast)
}

fn generate_ast(tokens: &[&str]) -> Result<Exp<String>, String> {
    // TODO: in the future, use bnf crate
    match tokens.len() {
        0 => Err(String::from("Can't parse zero-length list of tokens")),
        // create variable expression
        1 => Ok(Exp::Var(String::from(tokens[0]))),
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
                    String::from(tokens[1]),
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

fn get_unique_identifier(tag: &str, symbols: &mut HashMap<String, usize>) -> String {
    if !symbols.contains_key(tag) {
        symbols.insert(tag.to_string(), Default::default());
    }
    let n = symbols.get_mut(tag).unwrap();
    let identifier = format!("{}_{}", tag, n);
    *n += 1;
    identifier
}

/// Finds the index of the closing parenthesis, given that `v[0]` is `(`.
/// If it isn't `(`, `Some(0)` is returned.
/// If the end of the vector is reached without finding a closing brace,
/// `None` is returned.
fn find_matching_brace<'a>(
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
