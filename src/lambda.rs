use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;

#[derive(Debug, Clone)]
pub enum Exp<T>
where
    T: Clone + Eq + Hash,
{
    // variable
    Var(Ident<T>),
    // abstraction
    Abs(Ident<T>, Box<Exp<T>>),
    // application
    App(Box<Exp<T>>, Box<Exp<T>>),
}

impl<T> Exp<T>
where
    T: Clone + Eq + Hash,
{
    pub fn make_identifiers_unique(
        &mut self,
        symbols: &mut HashMap<T, usize>, // just use numbered ids for now
        scope: &mut HashMap<Ident<T>, Vec<usize>>, // pop and push ids
    ) {
        // scope maps original variable name to unique id
        match self {
            Exp::Var(ident) => {
                // try to replace with the new identifier at the top of the stack
                if let Some(identifiers) = scope.get(&ident) {
                    if let Some(new_id) = identifiers.last() {
                        ident.1 = Some(*new_id);
                    }
                }
            }
            Exp::Abs(ident, body) => {
                // construct an empty vec if it is missing
                if !scope.contains_key(&ident) {
                    scope.insert(ident.clone(), Default::default());
                }
                // scope in
                let new_ident = ident.alloc(symbols);
                scope.get_mut(&ident).unwrap().push(new_ident.1.unwrap());
                // convert the body
                body.make_identifiers_unique(symbols, scope);
                // remove our identifier, it can never be referenced again
                let idents = scope.get_mut(&ident).unwrap();
                idents.pop();
                // shrink the vec if it's too big (premature optimization?)
                if idents.len() <= idents.capacity() / 2 {
                    idents.shrink_to_fit();
                }
                *ident = new_ident;
            }
            Exp::App(l, r) => {
                l.make_identifiers_unique(symbols, scope);
                r.make_identifiers_unique(symbols, scope);
            }
        };
    }

    pub fn reduce(&mut self) {
        // could loop forever, like a running program
        loop {
            let did_beta = self.beta_reduce();
            self.make_identifiers_unique(&mut Default::default(), &mut Default::default());
            let did_eta = self.eta_reduce();

            if !did_beta && !did_eta {
                break;
            }
        }
    }

    pub fn as_numeral(&self) -> Option<usize> {
        // try to coerce this expression into a Church numeral
        // assume the expression has been completely reduced.
        // we are looking for an expression of the pattern
        // \f.\x.f^n x
        // eventually, `box` destructuring can be used to avoid the nested ifs
        if let Exp::Abs(f, fbody) = self {
            if let Exp::Abs(x, body) = fbody.as_ref() {
                // manually make mut since rust patterns won't let me
                // make a mut variable that is a ref
                let mut body = body;
                let mut depth = 0;
                while let Exp::App(l, r) = body.as_ref() {
                    if let Exp::Var(ident) = l.as_ref() {
                        if ident == f {
                            // left side var matches f
                            depth += 1;
                            // now inspect the right side
                            body = r;
                            continue;
                        }
                    }
                    return None;
                }
                // body should now be x
                if let Exp::Var(ident) = body.as_ref() {
                    if ident == x {
                        return Some(depth);
                    }
                }
            }
        }
        None
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
                    *self = reduced;
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
                            *self = reduced;
                            return true;
                        }
                    }
                }
                changed
            }
            _ => false,
        }
    }

    pub fn substitute(&mut self, symbol: &Ident<T>, exp: &Exp<T>) {
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
                if *id == *symbol {
                    *self = exp.clone();
                }
            }
        }
    }
}

impl<T> Display for Exp<T>
where
    T: Display + Clone + Eq + Hash,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident<T: Clone + Eq + Hash>(T, Option<usize>);

impl<T> Ident<T>
where
    T: Clone + Eq + Hash,
{
    #[allow(dead_code)]
    pub fn new(tag: T, symbols: &mut HashMap<T, usize>) -> Self {
        let mut s = Self::without_id(tag);
        s.realloc(symbols);
        s
    }

    pub fn without_id(tag: T) -> Self {
        Self(tag, None)
    }

    pub fn realloc(&mut self, symbols: &mut HashMap<T, usize>) {
        if !symbols.contains_key(&self.0) {
            symbols.insert(self.0.clone(), Default::default());
        }
        let id = symbols.get_mut(&self.0).unwrap();
        self.1 = Some(*id);
        *id = id.wrapping_add(1);
    }

    pub fn alloc(&self, symbols: &mut HashMap<T, usize>) -> Self {
        let mut s = self.clone();
        s.realloc(symbols);
        s
    }
}

impl<T> Display for Ident<T>
where
    T: Clone + Eq + Hash + Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            Some(id) => write!(f, "{}{}", self.0, id),
            None => write!(f, "{}", self.0),
        }
    }
}
