#![feature(box_patterns)]

use std::str::FromStr;

mod lambda;

use lambda::Expression;
use lambda::Expression::{Lambda,Apply,Var};

fn v(s: &str) -> Expression { Var(String::from(s)) }
fn app(e1: Expression, e2: Expression) -> Expression { Apply(Box::new(e1), Box::new(e2)) }
fn lam(s: &str, e: Expression) -> Expression { Lambda(String::from(s), Box::new(e)) }

fn church_numeral(i: usize) -> Expression {
    fn make_body(i: usize) -> Expression {
        if i == 0 {
            v("x")
        } else {
            app(v("f"), make_body(i-1))
        }
    }
    lam("f", lam("x", make_body(i)))
}

fn main() {
    //let vy = "y".to_string();

    // λx. x x
    let omega_inner = lam("x", app(v("x"), v("x")));
    // (λx. x x) (λx. x x)
    let w = app(omega_inner.clone(), omega_inner.clone());

    println!("w = {}", w.clone());
    println!("step(w) = {}", w.clone().step().1);
    // println!("{}", w.clone().eval()); // stack overflow

    // arithmetic
    // (λm. λn. λf. λx. m f (n f x))
    let plus_inner = app(app(v("m"), v("f")),
                         app(app(v("n"), v("f")),
                             v("x")));
    let plus = lam("m", lam("n", lam("f", lam("x", plus_inner))));
    // (λm. λn. m (plus n) z)... because that's how church numerals work!
    let times = lam("m", lam("n", app(app(v("m"),
                                          app(plus.clone(), v("n"))),
                                      church_numeral(0))));

    println!("plus = {}", plus);
    println!("0 = {}", church_numeral(0));
    println!("1 = {}", church_numeral(1));
    println!("2 = {}", church_numeral(2));
    println!("2+1 = {}", app(app(plus.clone(), church_numeral(2)), church_numeral(1)).eval());
    println!("1+2 = {}", app(app(plus.clone(), church_numeral(1)), church_numeral(2)).eval());
    println!("2*2 = {}", app(app(times.clone(), church_numeral(2)), church_numeral(2)).eval());
    println!("6*7 = {}", app(app(times.clone(), church_numeral(6)), church_numeral(7)).eval());

    // (λx. f(x x))
    let y_inner = lam("x", app(v("f"), app(v("x"), v("x"))));
    // λf. (λx. f(x x))(λx. f(x x))
    let y = lam("f", app(y_inner.clone(), y_inner.clone()));

    println!("Y = {}", y.clone());

    // TODO: fibs
    // ("if b e1 e2" is simply "b e1 e2", of course)

    // println!("{:?}", Expression::from_str("λx.x"));
    println!("parse test 1 = {:?}", Expression::from_str("λx. x"));
    //println!("{:?}", Expression::from_str("λf. (λx. f(x x)) (λx. f(x x))"));
}
