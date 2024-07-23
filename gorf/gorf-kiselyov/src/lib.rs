use lambda_calculus::Term;

// use crate::underload::{Underload, K, underload_parser};

#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash)]
pub enum Ski {
    S,
    K,
    B,
    R,
    U,
    I,
    App(Box<Ski>, Box<Ski>),
}
#[derive(Eq, Ord, Clone, PartialEq, PartialOrd, Hash)]
pub enum RealSki {
    RealApp(Box<RealSki>, Box<RealSki>),
    S,
    K,
    I,
}
pub fn ra(a: RealSki, b: RealSki) -> RealSki {
    return RealSki::RealApp(Box::new(a), Box::new(b));
}
impl From<Ski> for RealSki {
    fn from(a: Ski) -> RealSki {
        match a {
            Ski::S => RealSki::S,
            Ski::K => RealSki::K,
            Ski::B => ra(ra(RealSki::S, ra(RealSki::K, RealSki::S)), RealSki::K),
            //const R = S (K (S (K (S)) (K))) (S (K (S (I))) (K))
            Ski::R => ra(
                ra(
                    RealSki::S,
                    ra(
                        RealSki::K,
                        ra(ra(RealSki::S, ra(RealSki::K, RealSki::S)), RealSki::K),
                    ),
                ),
                ra(
                    ra(RealSki::S, ra(RealSki::K, ra(RealSki::S, RealSki::I))),
                    RealSki::K,
                ),
            ),
            Ski::U => ra(ra(RealSki::S, RealSki::I), RealSki::I),
            Ski::I => RealSki::I,
            Ski::App(a, b) => RealSki::RealApp(Box::new(Self::from(*a)), Box::new(Self::from(*b))),
        }
    }
}
impl RealSki {
    pub fn convert_default(t: Term) -> RealSki {
        return Self::from(Ski::convert_default(t));
    }
    // pub fn to_underload_str(self) -> String{
    //     match self{
    //         RealSki::RealApp(a, b) => format!("{}{}~^",a.to_underload_str(),b.to_underload_str()),
    //         RealSki::S => "(:)~*(~)*a(~*(~^)*)*)".to_owned(),
    //         RealSki::K => "(a(!)~*)".to_owned(),
    //         RealSki::I => "()".to_owned(),
    //     }
    // }
    // pub fn to_underload(self) -> Underload{
    //     return underload_parser::<Simple<char>>().parse(self.to_underload_str()).unwrap();
    // }
}
pub fn app(a: Ski, b: Ski) -> Ski {
    return Ski::App(Box::new(a), Box::new(b));
}
pub type SkiBall = (usize, Ski);
impl Ski {
    pub fn convert(t: Term, hash: &impl Fn(SkiBall, SkiBall) -> Ski) -> SkiBall {
        match t {
            Term::Var(0) => (0, Ski::U),
            Term::Var(1) => (1, Ski::I),
            Term::Var(n) => {
                let t = Ski::convert(Term::Var(n - 1), hash);
                let n = t.0;
                return (n + 1, hash((0, Ski::K), t));
            }
            Term::Abs(e) => {
                let (n, d) = Ski::convert(*e, hash);
                if n == 0 {
                    return (0, Ski::App(Box::new(Ski::K), Box::new(d)));
                }
                return (n - 1, d);
            }
            Term::App(a) => {
                let (e1, e2) = *a;
                let t1 = Ski::convert(e1, hash);
                let t2 = Ski::convert(e2, hash);
                return (t1.0.max(t2.0), hash(t1, t2));
            }
        }
    }
    pub fn default_hash(a: SkiBall, b: SkiBall) -> Ski {
        let (n1, d1) = a;
        let (n2, d2) = b;
        if n1 == 0 {
            if n2 == 0 {
                return Ski::App(Box::new(d1), Box::new(d2));
            } else {
                return Self::default_hash((0, app(Ski::B, d1)), (n2 - 1, d2));
            }
        } else {
            if n2 == 0 {
                return Self::default_hash((0, app(Ski::R, d2)), (n2 - 1, d1));
            } else {
                let a = Self::default_hash((0, Ski::S), (n1 - 1, d1));
                return Self::default_hash((n1 - 1, a), (n2 - 1, d2));
            }
        }
    }
    pub fn convert_default(t: Term) -> Ski {
        return Self::convert(t, &|a, b| Self::default_hash(a, b)).1;
    }
}
