use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq)]
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

use Exp::*;

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
            Var(ident) => {
                // try to replace with the new identifier at the top of the stack
                if let Some(identifiers) = scope.get(&ident) {
                    if let Some(new_id) = identifiers.last() {
                        ident.1 = Some(*new_id);
                    }
                }
            }
            Abs(ident, body) => {
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
            App(l, r) => {
                l.make_identifiers_unique(symbols, scope);
                r.make_identifiers_unique(symbols, scope);
            }
        };
    }

    pub fn reduce(&mut self) {
        // could loop forever, like a running program
        let mut symbols = Default::default();
        let mut scope = Default::default();
        loop {
            let did_beta = self.beta_reduce();
            self.make_identifiers_unique(&mut symbols, &mut scope);
            let did_eta = self.eta_reduce();

            if !did_beta && !did_eta {
                break;
            }

            // clear the maps for memory reuse
            symbols.clear();
            scope.clear();
        }
    }

    pub fn as_numeral(&self) -> Option<usize> {
        // try to coerce this expression into a Church numeral
        // assume the expression has been completely reduced.
        // we are looking for an expression of the pattern
        // \f.\x.f^n x
        // eventually, `box` destructuring can be used to avoid the nested ifs
        if let Abs(f, box Abs(x, body)) = self {
            // manually make mut since rust patterns won't let me
            // make a mut variable that is a ref
            let mut body = body;
            let mut depth = 0;
            while let box App(box Var(ident), r) = body {
                if ident != f {
                    return None;
                }
                // left side var matches f
                depth += 1;
                // now inspect the right side
                body = r;
                continue;
            }
            // body should now be x
            if let box Var(ident) = body {
                if ident == x {
                    return Some(depth);
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
            App(func, arg) => {
                // if the application has an abstraction on the left
                if let box Abs(var, body) = func {
                    let mut reduced = body.as_ref().clone();
                    // bottom-up beta reduce
                    reduced.beta_reduce();
                    reduced.substitute(var, arg);
                    *self = reduced;
                    true
                } else {
                    // otherwise reduce both sides
                    // prevent short circuit eval
                    func.beta_reduce() | arg.beta_reduce()
                }
            }
            Abs(_, body) => {
                // recurse
                body.beta_reduce()
            }
            // do nothing
            _ => false,
        }
    }

    pub fn eta_reduce(&mut self) -> bool {
        match self {
            App(func, arg) => func.eta_reduce() | arg.eta_reduce(),
            Abs(var, body) => {
                let changed = body.eta_reduce();
                if let box App(l, box Var(v)) = body {
                    if *var == *v {
                        let mut reduced = l.as_ref().clone();
                        reduced.eta_reduce();
                        *self = reduced;
                        return true;
                    }
                }
                changed
            }
            _ => false,
        }
    }

    // precondition: make_unique called
    pub fn substitute(&mut self, symbol: &Ident<T>, exp: &Exp<T>) -> bool {
        // TODO make unique vars for every bound variable in the
        match self {
            App(l, r) => l.substitute(symbol, exp) | r.substitute(symbol, exp),
            Abs(_, body) => {
                // should be made unique already, so binding is ignored
                body.substitute(symbol, exp)
            }
            Var(id) => {
                let matches = *id == *symbol;
                if matches {
                    *self = exp.clone();
                }
                matches
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
            Var(v) => write!(f, "{}", v),
            Abs(v, b) => {
                if let box App(l, r) = b {
                    // don't wrap the application in parentheses
                    write!(f, r"(\{}.{} {})", v, l, r)
                } else {
                    write!(f, r"(\{}.{})", v, b)
                }
            }
            App(l, r) => write!(f, "({} {})", l, r),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident<T: Clone + Eq + Hash>(pub T, pub Option<usize>);

impl<T> Ident<T>
where
    T: Clone + Eq + Hash,
{
    #[allow(dead_code)]
    pub fn new(tag: T, symbols: &mut HashMap<T, usize>) -> Self {
        let mut s = Self::free(tag);
        s.realloc(symbols);
        s
    }

    pub fn free(tag: T) -> Self {
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

// tests are super incomplete, coverage not examined
#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::*;

    #[test]
    fn substitute() {
        // test substituting l/r of App, r but not l of Abs, and Var
        let mut symbols = Default::default();
        let mut scope = Default::default();

        let mut exp = compile(r"\a.a (x ((\b. x) x))").unwrap();
        exp.make_identifiers_unique(&mut symbols, &mut scope);
        symbols.clear();
        scope.clear();

        // substituting non existent symbol should be false
        {
            let mut exp2 = exp.clone();
            assert!(!exp2.substitute(
                &Ident::free("z".into()),
                &Exp::Var(Ident("y".into(), Some(0))),
            ));
            assert_eq!(exp2, exp);
        }

        assert!(exp.substitute(
            &Ident::free("x".into()),
            &Exp::Var(Ident("y".into(), Some(0))),
        ));

        assert_eq!(
            exp,
            Abs(
                Ident("a".into(), Some(0)),
                Box::new(App(
                    Box::new(Var(Ident("a".into(), Some(0)))),
                    Box::new(App(
                        Box::new(Var(Ident("y".into(), Some(0)))),
                        Box::new(App(
                            Box::new(Abs(
                                Ident("b".into(), Some(0)),
                                Box::new(Var(Ident("y".into(), Some(0))))
                            )),
                            Box::new(Var(Ident("y".into(), Some(0))))
                        ))
                    ))
                ))
            )
        );
    }

    #[test]
    fn reduce() {
        // run 3 factorial
        let input = format!(
            r"({Y} \y.\n. {IS_ZERO} n (\f.\x.f x) ({MULT} n (y ({PRED} n)))) {N}",
            Y = r"(\f.(\x.f(x x)) (\x.f(x x)))",
            IS_ZERO = r"(\n.n (\x.(\a.\b.b)) (\a.\b.a))",
            MULT = r"(\m.\n.\f. m (n f))",
            PRED = r"(\n.\f.\x.n (\g.\h.h (gf)) (\u.x) (\u.u))",
            N = r"(\f.\x.f (f (f x)))" // 3
        );

        let mut program = compile(&input).unwrap();

        program.reduce();

        assert_eq!(
            program,
            compile(r"(\f.\x.f (f (f (f (f (f x))))))").unwrap() // 6
        );
    }

    #[test]
    fn as_numeral() {
        assert_eq!(compile(r"\f.\x.f").unwrap().as_numeral(), None);
        assert_eq!(compile(r"\f.\x.x f").unwrap().as_numeral(), None);
        assert_eq!(compile(r"\f.\x.f (x f)").unwrap().as_numeral(), None);
        assert_eq!(compile(r"\f.\x.f f x").unwrap().as_numeral(), None);

        assert_eq!(compile(r"\f.\x.x").unwrap().as_numeral(), Some(0));
        assert_eq!(compile(r"\f.\x.f x").unwrap().as_numeral(), Some(1));
        assert_eq!(compile(r"\f.\x.f (f x)").unwrap().as_numeral(), Some(2));
        assert_eq!(compile(r"\f.\x.f (f (f x))").unwrap().as_numeral(), Some(3));
    }

    #[test]
    fn beta_reduce() {
        let mut symbols = Default::default();
        let mut scope = Default::default();

        let mut exp = compile(r"(\x.(\a.(\u.a) b \u.x) (\u.u) x) \v.f v").unwrap();
        assert!(exp.beta_reduce());
        exp.make_identifiers_unique(&mut symbols, &mut scope);
        symbols.clear();
        scope.clear();

        assert_eq!(
            exp,
            App(
                Box::new(App(
                    Box::new(Abs(
                        Ident("u".into(), Some(0)),
                        Box::new(Var(Ident("u".into(), Some(0))))
                    )),
                    Box::new(Abs(
                        Ident("u".into(), Some(1)),
                        Box::new(Abs(
                            Ident("v".into(), Some(0)),
                            Box::new(App(
                                Box::new(Var(Ident::free("f".into()))),
                                Box::new(Var(Ident("v".into(), Some(0))))
                            ))
                        ))
                    ))
                )),
                Box::new(Abs(
                    Ident("v".into(), Some(1)),
                    Box::new(App(
                        Box::new(Var(Ident::free("f".into()))),
                        Box::new(Var(Ident("v".into(), Some(1))))
                    ))
                ))
            )
        );
        assert!(exp.beta_reduce());
        exp.make_identifiers_unique(&mut symbols, &mut scope);
        symbols.clear();
        scope.clear();

        assert_eq!(
            exp,
            App(
                Box::new(Abs(
                    Ident("u".into(), Some(0)),
                    Box::new(Abs(
                        Ident("v".into(), Some(0)),
                        Box::new(App(
                            Box::new(Var(Ident::free("f".into()))),
                            Box::new(Var(Ident("v".into(), Some(0))))
                        ))
                    ))
                )),
                Box::new(Abs(
                    Ident("v".into(), Some(1)),
                    Box::new(App(
                        Box::new(Var(Ident::free("f".into()))),
                        Box::new(Var(Ident("v".into(), Some(1))))
                    ))
                ))
            )
        );
        assert!(exp.beta_reduce());
        exp.make_identifiers_unique(&mut symbols, &mut scope);
        symbols.clear();
        scope.clear();

        assert_eq!(
            exp,
            Abs(
                Ident("v".into(), Some(0)),
                Box::new(App(
                    Box::new(Var(Ident::free("f".into()))),
                    Box::new(Var(Ident("v".into(), Some(0))))
                ))
            )
        );
        assert!(!exp.beta_reduce());

        assert_eq!(
            exp,
            Abs(
                Ident("v".into(), Some(0)),
                Box::new(App(
                    Box::new(Var(Ident::free("f".into()))),
                    Box::new(Var(Ident("v".into(), Some(0))))
                ))
            )
        );
    }

    #[test]
    fn eta_reduce() {
        let mut exp = compile(r"(\x.(\y.(\u.a) y) x) \v.g v").unwrap();
        assert!(exp.eta_reduce());
        assert_eq!(
            exp,
            App(
                Box::new(Abs(
                    Ident("u".into(), Some(0)),
                    Box::new(Var(Ident::free("a".into())))
                )),
                Box::new(Var(Ident::free("g".into())))
            )
        );
    }
}
