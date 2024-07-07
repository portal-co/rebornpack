use std::process::id;

use chumsky::prelude::*;
use chumsky::text::ident;
use chumsky::text::keyword;
use nonempty::nonempty;
use nonempty::NonEmpty;

use crate::backend::Ast;
use crate::backend::BinOp;
use crate::backend::CondType;

use super::Plat;

pub fn parse() -> impl Parser<char, Ast, Error = Simple<char>> {
    return recursive(|go| {
        let call = go.clone().padded().delimited_by(just('('), just(')')).then(
            go.clone()
                .padded()
                .separated_by(just(','))
                .delimited_by(just('('), just(')')),
        );
        let cond = choice((
            just("=0").map(|_| CondType::Eq),
            just(">0").map(|_| CondType::Greater),
        ));
        let plat = choice((
            keyword("linux").map(|_| Plat::Linux),
            keyword("wasm").map(|_| Plat::WASM),
        ));
        choice((
            go.clone()
                .padded()
                .separated_by(just(';'))
                .delimited_by(just('{'), just('}'))
                .map(|x| {
                    Ast::Many(Box::new(if x.len() == 0 {
                        nonempty![Ast::Const(0)]
                    } else {
                        NonEmpty::from_vec(x).unwrap()
                    }))
                }),
            text::int(10)
                .or(just('x').then(text::int(16)).map(|a| a.1))
                .from_str()
                .unwrapped()
                .map(Ast::Const),
            go.clone()
                .padded()
                .then(
                    one_of("+-*&|^")
                        .map(|c| match c {
                            '+' => BinOp::Add,
                            '-' => BinOp::Sub,
                            '*' => BinOp::Mul,
                            '&' => BinOp::And,
                            '|' => BinOp::Or,
                            '^' => BinOp::Xor,
                            _ => panic!("invalid char"),
                        })
                        .or(choice((
                            keyword("divu").map(|_| BinOp::DivU),
                            keyword("divs").map(|_| BinOp::DivS),
                            keyword("modu").map(|_| BinOp::ModU),
                            keyword("mods").map(|_| BinOp::ModS),
                            keyword("shl").map(|_| BinOp::Shl),
                            keyword("shru").map(|_| BinOp::ShrU),
                            keyword("shrs").map(|_| BinOp::ShrS),
                        ))
                        .padded()),
                )
                .then(go.clone().padded())
                .delimited_by(just('('), just(')'))
                .map(|((a, b), c)| Ast::Bin(b, Box::new(a), Box::new(c))),
            just("@").then(ident()).map(|(_, a)| Ast::Sym(a)),
            just("$")
                .then(ident())
                .padded()
                .then(just('=').then(go.clone().padded()).or_not())
                .map(|((_, i), n)| match n {
                    None => Ast::Var(i),
                    Some((_, a)) => Ast::Assign(i, Box::new(a)),
                }),
            call.clone().map(|x| Ast::Call(Box::new(x.0), x.1)),
            keyword("tail")
                .then(call.clone())
                .map(|x| Ast::Jmp(Box::new(x.1 .0), x.1 .1)),
            just('$')
                .then(text::int(10).from_str().unwrapped())
                .map(|a| Ast::Param(a.1)),
            keyword("return")
                .then(go.clone())
                .map(|a| Ast::Ret(Box::new(a.1))),
            keyword("alloca")
                .then(go.clone())
                .map(|a| Ast::Alloca(Box::new(a.1))),
            just("*")
                .then(one_of("bdwq"))
                .then(go.clone().padded().delimited_by(just('('), just(')')))
                .padded()
                .then(just('=').then(go.clone().padded()).or_not())
                .map(|(((_, sc), i), n)| {
                    let s = match sc {
                        'b' => super::Size::_8,
                        'd' => super::Size::_16,
                        'w' => super::Size::_32,
                        'q' => super::Size::_64,
                        _ => unreachable!(),
                    };
                    match n {
                        None => Ast::Load(Box::new(i), s),
                        Some((_, a)) => Ast::Store(Box::new(i), Box::new(a), s),
                    }
                }),
            keyword("if")
                .then(go.clone().padded())
                .then(cond.clone())
                .then(go.clone().padded())
                .then(keyword("else"))
                .then(go.clone().padded())
                .map(|m| Ast::If {
                    cond: Box::new(m.0 .0 .0 .0 .1),
                    cty: m.0 .0 .0 .1,
                    if_true: Box::new(m.0 .0 .1),
                    if_false: Box::new(m.1),
                }),
            keyword("do")
                .then(go.clone().padded())
                .then(keyword("while"))
                .then(go.clone().padded())
                .then(cond.clone())
                .map(|m| Ast::DoWhile {
                    body: Box::new(m.0 .0 .0 .1),
                    cond: Box::new(m.0 .1),
                    cty: m.1,
                }),
            plat.padded()
                .then(
                    go.clone()
                        .padded()
                        .separated_by(just(','))
                        .delimited_by(just('('), just(')')),
                )
                .map(|a| Ast::Plat(a.0, a.1)),
            keyword("os").map(|_| Ast::OS),
        ))
    });
}
