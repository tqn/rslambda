#![feature(box_patterns)]
mod compiler;
mod lambda;

use compiler::*;

fn main() {
    // test Y combinator
    //let input = r"\f.(\x.f (x x)) (\x.f (x x))";
    // test successor of 2
    // let input = r"(\n.\m.\f. m (n f)) (\f.\x.f (f x))  (\f.\x.f (f (f x)))";
    // test eta
    // let input = r"(\x.\y.f x y)";
    // factorial
    let input = format!(
        r"({Y} \y.\n. {IS_ZERO} n (\f.\x.f x) ({MULT} n (y ({PRED} n)))) {N}",
        Y = r"(\f.(\x.f(x x)) (\x.f(x x)))",
        IS_ZERO = r"(\n.n (\x.(\a.\b.b)) (\a.\b.a))",
        MULT = r"(\m.\n.\f. m (n f))",
        PRED = r"(\n.\f.\x.n (\g.\h.h (gf)) (\u.x) (\u.u))",
        N = r"(\f.\x.f (f (f (f x))))"
    );

    let mut program = compile(&input).unwrap();

    println!("{}", program);

    // time the program
    use std::time::Instant;
    let now = Instant::now();

    // run the program, may not halt for some inputs
    program.reduce();

    let elapsed = now.elapsed();
    println!(
        "elapsed {:6}.{:<03} s\n{}",
        elapsed.as_secs(),
        elapsed.subsec_millis(),
        program
    );

    // attempt to display the church numeral value
    if let Some(n) = program.as_numeral() {
        println!(" = {}", n);
    }
}
