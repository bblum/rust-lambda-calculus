#![macro_use]
extern crate nom;

use std::fmt;
use std::str::FromStr;
use std::str::Chars;
use std::string::String;

pub type Variable = String;

// TODO: implement PartialEq yourself including alpha variance
#[derive(Clone,PartialEq,Debug)]
pub enum Expression {
    Var(Variable),
    Lambda(Variable, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
}

use Expression::{Var,Lambda,Apply};

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(n) = self.clone().is_church_numeral() {
            write!(f, "{}", n)
        } else {
            match self {
                // some special cases
                Lambda(x, box Lambda(y, box Var(z))) if x == z && x != y => write!(f, "True"),
                Lambda(_, box Lambda(y, box Var(z))) if y == z           => write!(f, "False"),
                // TODO: and, or, not, xor, nand?
                // TODO: add a few of the church numbers
                // normal printing
                Var(v)                                        => write!(f, "{}", v),
                Lambda(v, e)                                  => write!(f, "(λ{}. {})", v, e),
                // print extra parens to disambiguate nested apply expressions e.g. x (y z) vs (x y) z
                Apply(e1, e2@box Apply(_, _))                 => write!(f, "{} ({})", e1, e2),
                // don't bother with these cases - function application is left-associative anyway
                // Apply(e1@box Apply(_, _), e2@box Apply(_, _)) => write!(f, "({}) ({})", e1, e2),
                // Apply(e1@box Apply(_, _), e2)                 => write!(f, "({}) {}", e1, e2),
                Apply(e1, e2)                                 => write!(f, "{} {}", e1, e2),
            }
        }
    }
}

impl Expression {
    /// evaluate [e/x]self, i.e., "replace x with e in self"
    pub fn subst(self, e: Expression, x: &Variable) -> Expression {
        match self {
            Var(v) => {
                // actual substitution occurs here
                if v == *x { e } else { Var(v) }
            },
            Lambda(v, box e1) => {
                // avoid capturing a shadowed variable
                // e.g. (λx. λx. x)(y) should evaluate to (λx. x), not (λx. y)
                let body = Box::new(if v == *x { e1 } else { e1.subst(e, x) });
                Lambda(v, body)
            },
            Apply(box e1, box e2) => {
                Apply(Box::new(e1.subst(e.clone(), x)),
                      Box::new(e2.subst(e.clone(), x)))
            },
        }
    }

    /// perform one evaluation step
    pub fn step(self) -> (bool, Expression) {
        match self {
            Apply(box Lambda(v, box body), box e2) => {
                (true, body.subst(e2, &v))
            },
            Apply(box e1, box e2) => {
                // try waiting for e1 to normalize to a lambda
                let (stepped1, new_e1) = e1.step();
                // call by value semantics
                // (call by name would omit this step, but makes it harder to check beta-equivalence)
                let (stepped2, new_e2) = e2.step();
                (stepped1 || stepped2, Apply(Box::new(new_e1), Box::new(new_e2)))
            },
            // reduce inside lambdas in pursuit of beta-normal form
            // (this allows us to check for beta-equivalence, for the sake of the demo)
            Lambda(v, box e) => {
                let (stepped, new_e) = e.step();
                (stepped, Lambda(v, Box::new(new_e)))
            },
            // free variables cannot step
            Var(v) => (false, Var(v)),
        }
    }

    /// evaluate until the expression reaches a terminal state
    pub fn eval(self) -> Expression {
        let (stepped, e) = self.step();
        if stepped { e.eval() } else { e }
    }

    // convenience function for pretty printing
    // only reliable if self is already in beta-normal form... to avoid crashing when printing omega!
    fn is_church_numeral(&self) -> Option<usize> {
        fn is_church_body(body: &Expression, f: &Variable, x: &Variable, n: usize) -> Option<usize> {
            match body {
                Var(y)               if y == x => Some(n),
                Apply(box Var(g), y) if g == f => is_church_body(y, f, x, n+1),
                _                              => None,
            }
        }
        match self {
            Lambda(f, box Lambda(x, body)) => is_church_body(body, f, x, 0),
            _                              => None,
        }
    }
}

impl FromStr for Expression {
    type Err = String;
    fn from_str(s: &str) -> Result<Expression, String> {
        #[allow(dead_code)]
        fn parse_char(s: &str, i: &mut usize, c0: char) -> Result<(), String> {
            if let Some(c) = s.chars().skip(*i).next() {
                if c == c0 {
                    *i += 1;
                    Result::Ok(())
                } else {
                    Result::Err(format!("expected '{}', got '{}'", c0, c))
                }
            } else {
                Result::Err("end of input".to_string())
            }
        }

        fn parse_one(input: &mut Chars) -> Result<Expression, String> {
            let nextc = input.next();
            println!("parse: next char {:?}", nextc);
            match nextc {
            //match input.next() {
                Some('(') => {
                    let mut parens_level = 0;
                    input.next();
                    // will consume the closing paren
                    let paren_contents = input.take_while(|c| {
                        if *c == '(' {
                            parens_level += 1;
                            true
                        } else if *c == ')' {
                            parens_level > 0
                        } else {
                            true
                        }
                    }).collect::<String>();
                    println!("parsing paren contents: {}", paren_contents);
                    Result::Ok(parse(&mut paren_contents.chars())?)
                },
                Some('λ') | Some('\\') => {
                    // will consume the chara after the varname, ideally '.'
                    let varname = input.take_while(|c| char::is_alphanumeric(*c)).collect::<String>();
                    println!("parse: lambda varname {}", varname);
                    if varname.len() == 0 {
                        Result::Err(format!("expected a variable name here: {}", input.collect::<String>()))
                    } else {
                        let body = parse(input)?;
                        Result::Ok(Lambda(varname, Box::new(body)))
                    }
                },
                Some(c) if char::is_alphanumeric(c) => {
                    // ugh
                    Result::Ok(Var(format!("{}{}", c, input.take_while(|c| char::is_alphanumeric(*c)).collect::<String>())))
                },
                Some(c) if char::is_whitespace(c) => {
                    println!("skipping ws case");
                    // XXX
                    input.skip_while(|c| char::is_whitespace(*c)).count();
                    parse_one(input)
                },
                Some(c) => Result::Err(format!("unexpected character: {}", c)),
                None    => Result::Err(format!("unexpected end of input")),
            }
        }

        fn parse(input: &mut Chars) -> Result<Expression, String> {
            let mut e = parse_one(input)?;
            println!("parsed {}", e);
            while input.peekable().peek().is_some() {
                e = Apply(Box::new(e), Box::new(parse_one(input)?));
            }
            Result::Ok(e)
        }

        parse(&mut s.chars())
    }
}

