use std::collections::HashMap;
use std::hash::Hash;
use std::{
    fmt::{self, Display},
    mem,
};

#[derive(Debug, Clone)]
pub enum Exp<T> {
    // variable
    Var(T),
    // abstraction
    Abs(T, Box<Exp<T>>),
    // application
    App(Box<Exp<T>>, Box<Exp<T>>),
}

impl Exp<String> {
    pub fn make_identifiers_unique(
        &mut self,
        symbols: &mut HashMap<String, usize>, // just use numbered ids for now
        scope: &mut HashMap<String, Vec<String>>, // pop and push ids
    ) -> Result<(), String> {
        // scope maps original variable name to unique id
        match self {
            Exp::Var(name) => {
                // try to replace with the new identifier at the top of the stack
                if let Some(identifiers) = scope.get(name) {
                    if let Some(new_id) = identifiers.last() {
                        *name = new_id.clone();
                    }
                }
            }
            Exp::Abs(var, body) => {
                // construct an empty vec if it is missing
                if !scope.contains_key(var) {
                    scope.insert(var.clone(), Default::default());
                }
                // scope in
                let id = get_unique_identifier(&var, symbols);
                scope.get_mut(var).unwrap().push(id.clone());
                // convert the body
                body.make_identifiers_unique(symbols, scope)?;
                // remove our identifier, it can never be referenced again
                let idents = scope.get_mut(var).unwrap();
                idents.pop();
                // shrink the vec if it's too big (premature optimization?)
                if idents.len() <= idents.capacity() / 2 {
                    idents.shrink_to_fit();
                }
                *var = id;
            }
            Exp::App(l, r) => {
                l.make_identifiers_unique(symbols, scope)?;
                r.make_identifiers_unique(symbols, scope)?;
            }
        };
        Ok(())
    }

    // returns a boolean indicating whether any changes were made
    pub fn beta_reduce(&mut self) -> bool {
        // beta reduce from bottom up one time to avoid infinite regress
        match self {
            // where everything interesting happens
            Exp::App(func, arg) => {
                // if the application has an abstraction on the left
                if let Exp::Abs(var, body) = func.as_ref() {
                    let mut reduced = body.as_ref().clone();
                    // bottom-up beta reduce
                    reduced.beta_reduce();
                    reduced.substitute(var, arg);
                    mem::replace(self, reduced);
                    true
                } else {
                    // otherwise reduce both sides
                    // use temp variable to avoid short-circuit eval
                    let changed = func.beta_reduce();
                    arg.beta_reduce() || changed
                }
            }
            Exp::Abs(_, body) => {
                // recurse
                body.beta_reduce()
            }
            // do nothing
            _ => false,
        }
    }

    pub fn eta_reduce(&mut self) -> bool {
        match self {
            Exp::App(func, arg) => {
                let changed = func.eta_reduce();
                arg.eta_reduce() || changed
            }
            Exp::Abs(var, body) => {
                let changed = body.eta_reduce();
                if let Exp::App(l, r) = body.as_ref() {
                    if let Exp::Var(v) = r.as_ref() {
                        if *var == *v {
                            let mut reduced = l.as_ref().clone();
                            reduced.eta_reduce();
                            mem::replace(self, reduced);
                            return true;
                        }
                    }
                }
                changed
            }
            _ => false,
        }
    }

    pub fn substitute(&mut self, symbol: &str, exp: &Exp<String>) {
        // TODO make unique vars for every bound variable in the
        match self {
            Exp::App(l, r) => {
                l.substitute(symbol, exp);
                r.substitute(symbol, exp);
            }
            Exp::Abs(_, body) => {
                // should be made unique already, so binding is ignored
                body.substitute(symbol, exp);
            }
            Exp::Var(id) => {
                if *id == symbol {
                    *self = exp.clone();
                }
            }
        }
    }
}

impl<T> Display for Exp<T>
where
    T: Display,
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

#[derive(Debug, Clone)]
pub struct Ident<T>
where
    T: Clone + Eq + Hash,
{
    tag: T,
    id: Option<usize>,
}

impl<T> Ident<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new(tag: T, symbols: &mut HashMap<T, usize>) -> Self {
        let mut s = Self::without_id(tag);
        s.realloc(symbols);
        s
    }

    pub fn without_id(tag: T) -> Self {
        Self { tag, id: None }
    }

    pub fn realloc(&mut self, symbols: &mut HashMap<T, usize>) {
        if !symbols.contains_key(&self.tag) {
            symbols.insert(self.tag.clone(), Default::default());
        }
        self.id = Some(*symbols.get_mut(&self.tag).unwrap());
    }
}

impl<T> Display for Ident<T>
where
    T: Clone + Eq + Hash + Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.id {
            Some(id) => write!(f, "{}{}", self.tag, id),
            None => write!(f, "{}", self.tag),
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