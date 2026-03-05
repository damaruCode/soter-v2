use chumsky::prelude::*;

#[derive(Debug)]
enum Expr<'src> {
    Num(f64),
    Var(&'src str),

    Neg(Box<Expr<'src>>),
    Add(Box<Expr<'src>>, Box<Expr<'src>>),
    Sub(Box<Expr<'src>>, Box<Expr<'src>>),
    Mul(Box<Expr<'src>>, Box<Expr<'src>>),
    Div(Box<Expr<'src>>, Box<Expr<'src>>),

    Call(&'src str, Vec<Expr<'src>>),
    Let {
        name: &'src str,
        rhs: Box<Expr<'src>>,
        then: Box<Expr<'src>>,
    },
    Fn {
        name: &'src str,
        args: Vec<&'src str>,
        body: Box<Expr<'src>>,
        then: Box<Expr<'src>>,
    },
}

pub fn do_shit(path: &str) {
    let src = std::fs::read_to_string(path).unwrap();

    println!("{:?}", parser().parse(&src));
}

fn parser<'src>() -> impl Parser<'src, &'src str, Expr<'src>> {
    any()
        .filter(|c: &char| c.is_ascii_digit())
        .map(|c| Expr::Num(c.to_digit(10).unwrap() as f64))
}
