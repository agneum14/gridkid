use std::fmt::Display;


use anyhow::{bail, ensure, Context, Result};

struct Cell {
    ast: Option<Token>,
    eval: Option<Token>,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            ast: None,
            eval: None,
        }
    }
}

struct Grid {
    cells: Vec<Cell>,
}

impl Default for Grid {
    fn default() -> Self {
        Self {
            cells: (0..(Self::WIDTH * Self::WIDTH))
                .map(|_| Cell::default())
                .collect(),
        }
    }
}

impl Grid {
    const WIDTH: usize = 100;

    /// Calculate an grid index from an AddrPrim
    fn idx(addr: &Token) -> Result<usize> {
        if let Token::AddrPrim(x, y) = addr {
            ensure!(*x < Self::WIDTH && *y < Self::WIDTH);
            Ok(*x * Self::WIDTH + *y * Self::WIDTH)
        } else {
            bail!("cannot index grid with type {}", addr)
        }
    }

    /// Get a cell reference from an AddrPrim
    fn cell(&self, addr: &Token) -> Result<&Cell> {
        let idx = Self::idx(addr)?;
        Ok(self.cells.get(idx).unwrap())
    }

    /// Get a mutable cell reference from an AddrPrim
    fn cell_mut(&mut self, addr: &Token) -> Result<&mut Cell> {
        let idx = Self::idx(addr)?;
        Ok(self.cells.get_mut(idx).unwrap())
    }
}

struct Runtime {
    grid: Grid,
}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            grid: Grid::default(),
        }
    }
}

impl Runtime {
    /// Get a cell reference from an AddrPrim
    fn cell(&self, addr: &Token) -> Result<&Cell> {
        self.grid.cell(addr)
    }

    /// Get a mutable cell reference from an AddrPrim
    fn cell_mut(&mut self, addr: &Token) -> Result<&mut Cell> {
        self.grid.cell_mut(addr)
    }

    /// Set a cell from an AddrPrim and AST
    fn set_cell(&mut self, addr: &Token, ast: &Token) -> Result<()> {
        let eval = ast.eval(self)?;
        let cell = self.cell_mut(addr)?;
        cell.ast = Some(ast.clone());
        cell.eval = Some(eval);
        Ok(())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    // primatives
    IntPrim(i64),
    FloatPrim(f64),
    BoolPrim(bool),
    AddrPrim(usize, usize),

    // arithmetic
    Add(Box<Token>, Box<Token>),
    Sub(Box<Token>, Box<Token>),
    Mult(Box<Token>, Box<Token>),
    Div(Box<Token>, Box<Token>),
    Mod(Box<Token>, Box<Token>),
    Exp(Box<Token>, Box<Token>),

    // logical
    LogicAnd(Box<Token>, Box<Token>),
    LogicOr(Box<Token>, Box<Token>),
    LogicNot(Box<Token>),

    // cell values
    RValue(Box<Token>, Box<Token>),

    // bitwise
    BitwiseAnd(Box<Token>, Box<Token>),
    BitwiseOr(Box<Token>, Box<Token>),
    BitwiseXor(Box<Token>, Box<Token>),
    BitwiseNot(Box<Token>),
    BitwiseLeftShift(Box<Token>, Box<Token>),
    BitwiseRightShift(Box<Token>, Box<Token>),

    // relational
    Equals(Box<Token>, Box<Token>),
    NotEquals(Box<Token>, Box<Token>),
    LessThan(Box<Token>, Box<Token>),
    LessThanOrEquals(Box<Token>, Box<Token>),
    GreaterThan(Box<Token>, Box<Token>),
    GreaterThanOrEquals(Box<Token>, Box<Token>),

    // casting
    CastToInt(Box<Token>),
    CastToFloat(Box<Token>),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntPrim(_) => write!(f, "IntPrim"),
            Self::FloatPrim(_) => write!(f, "FloatPrim"),
            Self::BoolPrim(_) => write!(f, "BoolPrim"),
            Self::AddrPrim(_, _) => write!(f, "BoolPrim"),
            Self::Add(_, _) => write!(f, "Add"),
            Self::Sub(_, _) => write!(f, "Sub"),
            Self::Mult(_, _) => write!(f, "Mult"),
            Self::Div(_, _) => write!(f, "Div"),
            Self::Mod(_, _) => write!(f, "Mod"),
            Self::Exp(_, _) => write!(f, "Exp"),
            Self::LogicAnd(_, _) => write!(f, "LogicAnd"),
            Self::LogicOr(_, _) => write!(f, "LogicOr"),
            Self::LogicNot(_) => write!(f, "LogicNot"),
            Self::RValue(_, _) => write!(f, "RValue"),
            Self::BitwiseAnd(_, _) => write!(f, "BitwiseAnd"),
            Self::BitwiseOr(_, _) => write!(f, "BitwiseOr"),
            Self::BitwiseXor(_, _) => write!(f, "BitwiseXor"),
            Self::BitwiseNot(_) => write!(f, "BitwiseNot"),
            Self::BitwiseLeftShift(_, _) => write!(f, "BitwiseLeftShift"),
            Self::BitwiseRightShift(_, _) => write!(f, "BitwiseRightShift"),
            Self::Equals(_, _) => write!(f, "Equals"),
            Self::NotEquals(_, _) => write!(f, "NotEquals"),
            Self::LessThan(_, _) => write!(f, "LessThan"),
            Self::LessThanOrEquals(_, _) => write!(f, "LessThanOrEquals"),
            Self::GreaterThan(_, _) => write!(f, "GreaterThan"),
            Self::GreaterThanOrEquals(_, _) => write!(f, "GreaterThanOrEquals"),
            Self::CastToInt(_) => write!(f, "CastToInt"),
            Self::CastToFloat(_) => write!(f, "CastToFloat"),
        }
    }
}

impl Token {
    /// Get the inner value of an IntPrim
    fn inner_int_prim(&self) -> Result<i64> {
        match self {
            Token::IntPrim(a) => Ok(*a),
            _ => bail!("{} is not an IntPrim", self),
        }
    }

    /// Create an AddrPrim from two IntPrims with bounds checking
    fn new_addr_prim(x: &Token, y: &Token) -> Result<Token> {
        let (x, y) = match (x, y) {
            (Token::IntPrim(x), Token::IntPrim(y)) => (*x, *y),
            _ => bail!("cannot create AddrPrim from types {} and {}", x, y),
        };
        ensure!(x >= 0 && x < Grid::WIDTH as i64 && y >= 0 && y < Grid::WIDTH as i64);
        Ok(Token::AddrPrim(x as usize, y as usize))
    }

    /// Converts an IntPrim and a FloatPrim to FloatPrims. Other tokens are unchanged.
    fn coerce(a: &Token, b: &Token) -> (Token, Token) {
        if let (Token::IntPrim(av), Token::FloatPrim(_)) = (a, b) {
            (Token::FloatPrim(*av as f64), b.clone())
        } else if let (Token::FloatPrim(_), Token::IntPrim(bv)) = (a, b) {
            (a.clone(), Token::FloatPrim(*bv as f64))
        } else {
            (a.clone(), b.clone())
        }
    }

    /// Evaluate a Token, recursing through the AST
    pub fn eval(&self, runtime: &Runtime) -> Result<Token> {
        match self {
            Self::IntPrim(_) | Self::FloatPrim(_) | Self::BoolPrim(_) | Self::AddrPrim(_, _) => {
                Ok(self.clone())
            }
            Self::Add(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av + bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av + bv)),
                    _ => bail!("cannot add types {} and {}", a, b),
                }
            }
            Self::Sub(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av - bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av - bv)),
                    _ => bail!("cannot sub types {} and {}", a, b),
                }
            }
            Self::Mult(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av * bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av * bv)),
                    _ => bail!("cannot mult types {} and {}", a, b),
                }
            }
            Self::Div(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av / bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av / bv)),
                    _ => bail!("cannot div types {} and {}", a, b),
                }
            }
            Self::Mod(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av % bv)),
                    _ => bail!("cannot mod types {} and {}", a, b),
                }
            }
            Self::Exp(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => {
                        Ok(Token::IntPrim(av.pow(*bv as u32)))
                    }
                    (Token::IntPrim(av), Token::FloatPrim(bv)) => {
                        Ok(Token::FloatPrim((*av as f64).powf(*bv)))
                    }
                    (Token::FloatPrim(av), Token::IntPrim(bv)) => {
                        Ok(Token::FloatPrim(av.powi(*bv as i32)))
                    }
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => {
                        Ok(Token::FloatPrim(av.powf(*bv)))
                    }
                    _ => bail!("cannot exponentiate types {} and {}", a, b),
                }
            }
            Self::LogicAnd(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::BoolPrim(av), Token::BoolPrim(bv)) => Ok(Token::BoolPrim(*av && *bv)),
                    _ => bail!("cannot logic and types {} and {}", a, b),
                }
            }
            Self::LogicOr(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::BoolPrim(av), Token::BoolPrim(bv)) => Ok(Token::BoolPrim(*av || *bv)),
                    _ => bail!("cannot logic or types {} and {}", a, b),
                }
            }
            Self::LogicNot(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Token::BoolPrim(av) => Ok(Token::BoolPrim(!av)),
                    _ => bail!("cannot logic not type {}", a),
                }
            }
            Self::RValue(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let addr = Self::new_addr_prim(a, b)?;
                let cell = runtime.cell(&addr)?;
                let (x, y) = (a.inner_int_prim().unwrap(), b.inner_int_prim().unwrap());
                cell.eval
                    .as_ref()
                    .cloned()
                    .context(format!("accessed empty cell ({}, {})", x, y))
            }
            Self::BitwiseAnd(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Self::IntPrim(av & bv)),
                    _ => bail!("cannot bitwise and types {} and {}", a, b),
                }
            }
            Self::BitwiseOr(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Self::IntPrim(av | bv)),
                    _ => bail!("cannot bitwise or types {} and {}", a, b),
                }
            }
            Self::BitwiseXor(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Self::IntPrim(av ^ bv)),
                    _ => bail!("cannot bitwise xor types {} and {}", a, b),
                }
            }
            Self::BitwiseNot(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Token::IntPrim(av) => Ok(Token::IntPrim(!av)),
                    _ => bail!("cannot bitwise not type {}", a),
                }
            }
            Self::BitwiseLeftShift(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Self::IntPrim(av << bv)),
                    _ => bail!("cannot left shift types {} and {}", a, b),
                }
            }
            Self::BitwiseRightShift(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Self::IntPrim(av >> bv)),
                    _ => bail!("cannot right shift types {} and {}", a, b),
                }
            }
            Self::CastToInt(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Token::IntPrim(_) => Ok(a.clone()),
                    Token::FloatPrim(v) => Ok(Token::IntPrim(*v as i64)),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
                }
            }
            Self::CastToFloat(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Token::IntPrim(v) => Ok(Token::FloatPrim(*v as f64)),
                    Token::FloatPrim(_) => Ok(a.clone()),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
                }
            }
            Self::Equals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av == bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av == bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::NotEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av != bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av != bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::LessThan(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av < bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av < bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::LessThanOrEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av <= bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av <= bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::GreaterThan(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av > bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av > bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::GreaterThanOrEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::BoolPrim(av >= bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::BoolPrim(av >= bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
        }
    }

    /// Serialize a Token, recursing through the AST
    fn serialize(&self) -> String {
        match self {
            Self::IntPrim(a) => format!("{}", a),
            Self::FloatPrim(a) => format!("{}", a),
            Self::BoolPrim(a) => format!("{}", a),
            Self::AddrPrim(a, b) => format!("[{}, {}]", a, b),
            Self::Add(a, b) => format!("({} + {})", a.serialize(), b.serialize()),
            Self::Sub(a, b) => format!("({} - {})", a.serialize(), b.serialize()),
            Self::Mult(a, b) => format!("({} * {})", a.serialize(), b.serialize()),
            Self::Div(a, b) => format!("({} / {})", a.serialize(), b.serialize()),
            Self::Exp(a, b) => format!("({}^{})", a.serialize(), b.serialize()),
            Self::Mod(a, b) => format!("({} % {})", a.serialize(), b.serialize()),
            Self::LogicAnd(a, b) => format!("({} && {})", a.serialize(), b.serialize()),
            Self::LogicOr(a, b) => format!("({} || {})", a.serialize(), b.serialize()),
            Self::LogicNot(a) => format!("!{}", a.serialize()),
            Self::RValue(a, b) => format!("#[{}, {}]", a.serialize(), b.serialize()),
            Self::BitwiseAnd(a, b) => format!("({} & {})", a.serialize(), b.serialize()),
            Self::BitwiseOr(a, b) => format!("({} | {})", a.serialize(), b.serialize()),
            Self::BitwiseXor(a, b) => format!("({} | {})", a.serialize(), b.serialize()),
            Self::BitwiseNot(a) => format!("!{}", a.serialize()),
            Self::BitwiseLeftShift(a, b) => format!("({} << {})", a.serialize(), b.serialize()),
            Self::BitwiseRightShift(a, b) => format!("({} >> {})", a.serialize(), b.serialize()),
            Self::Equals(a, b) => format!("({} == {})", a.serialize(), b.serialize()),
            Self::NotEquals(a, b) => format!("({} != {})", a.serialize(), b.serialize()),
            Self::LessThan(a, b) => format!("({} < {})", a.serialize(), b.serialize()),
            Self::LessThanOrEquals(a, b) => format!("({} <= {})", a.serialize(), b.serialize()),
            Self::GreaterThan(a, b) => format!("({} > {})", a.serialize(), b.serialize()),
            Self::GreaterThanOrEquals(a, b) => format!("({} >= {})", a.serialize(), b.serialize()),
            Self::CastToInt(a) => format!("int({})", a.serialize()),
            Self::CastToFloat(a) => format!("float({})", a.serialize()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// (7 * 4 + 3) % 12 = 7
    #[test]
    fn arithmetic() {
        let x = Token::Mult(Box::new(Token::IntPrim(7)), Box::new(Token::IntPrim(4)));
        let x = Token::Add(Box::new(x), Box::new(Token::IntPrim(3)));
        let x = Token::Mod(Box::new(x), Box::new(Token::IntPrim(12)));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Token::IntPrim(7), x);
    }

    // #[1 + 1, 4] << 3
    #[test]
    fn rvalue_shift() {
        // store 5 in (2, 4)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Token::AddrPrim(2, 4), &Token::IntPrim(5))
            .unwrap();

        let x = Token::Add(Box::new(Token::IntPrim(1)), Box::new(Token::IntPrim(1)));
        let x = Token::RValue(Box::new(x), Box::new(Token::IntPrim(4)));
        let x = Token::BitwiseLeftShift(Box::new(x), Box::new(Token::IntPrim(3)));
        let x = x.eval(&runtime).unwrap();
        assert_eq!(Token::IntPrim(40), x);
    }

    /// #[0, 0] < #[0, 1]
    #[test]
    fn rvalue_cmp() {
        // store 1 in (0, 0) and 7 in (0, 1)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Token::AddrPrim(0, 0), &Token::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Token::AddrPrim(0, 1), &Token::IntPrim(7))
            .unwrap();

        let x = Token::RValue(Box::new(Token::IntPrim(0)), Box::new(Token::IntPrim(0)));
        let y = Token::RValue(Box::new(Token::IntPrim(0)), Box::new(Token::IntPrim(1)));
        let res = Token::LessThan(Box::new(x), Box::new(y));
        let res = res.eval(&runtime).unwrap();
        assert_eq!(Token::BoolPrim(true), res)
    }

    /// !(3.3 > 3.2)
    #[test]
    fn logic_cmp() {
        let x = Token::GreaterThan(
            Box::new(Token::FloatPrim(3.3)),
            Box::new(Token::FloatPrim(3.2)),
        );
        let x = Token::LogicNot(Box::new(x));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Token::BoolPrim(false), x);
    }

    /// float(7) / 2 = 3.5
    #[test]
    fn casting() {
        let x = Token::CastToFloat(Box::new(Token::IntPrim(7)));
        let x = Token::Div(Box::new(x), Box::new(Token::IntPrim(2)));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Token::FloatPrim(3.5), x);
    }

    /// (7 * 4 + 3) % 12 = 7
    #[test]
    fn arithmetic_serialize() {
        let x = Token::Mult(Box::new(Token::IntPrim(7)), Box::new(Token::IntPrim(4)));
        let x = Token::Add(Box::new(x), Box::new(Token::IntPrim(3)));
        let x = Token::Mod(Box::new(x), Box::new(Token::IntPrim(12)));
        assert_eq!("(((7 * 4) + 3) % 12)", x.serialize());
    }

    // #[1 + 1, 4] << 3
    #[test]
    fn rvalue_shift_serialize() {
        // store 5 in (2, 4)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Token::AddrPrim(2, 4), &Token::IntPrim(5))
            .unwrap();

        let x = Token::Add(Box::new(Token::IntPrim(1)), Box::new(Token::IntPrim(1)));
        let x = Token::RValue(Box::new(x), Box::new(Token::IntPrim(4)));
        let x = Token::BitwiseLeftShift(Box::new(x), Box::new(Token::IntPrim(3)));
        assert_eq!("(#[(1 + 1), 4] << 3)", x.serialize());
    }

    /// #[0, 0] < #[0, 1]
    #[test]
    fn rvalue_cmp_serialize() {
        // store 1 in (0, 0) and 7 in (0, 1)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Token::AddrPrim(0, 0), &Token::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Token::AddrPrim(0, 1), &Token::IntPrim(7))
            .unwrap();

        let x = Token::RValue(Box::new(Token::IntPrim(0)), Box::new(Token::IntPrim(0)));
        let y = Token::RValue(Box::new(Token::IntPrim(0)), Box::new(Token::IntPrim(1)));
        let res = Token::LessThan(Box::new(x), Box::new(y));
        assert_eq!("(#[0, 0] < #[0, 1])", res.serialize())
    }

    /// !(3.3 > 3.2)
    #[test]
    fn logic_cmp_serialize() {
        let x = Token::GreaterThan(
            Box::new(Token::FloatPrim(3.3)),
            Box::new(Token::FloatPrim(3.2)),
        );
        let x = Token::LogicNot(Box::new(x));
        assert_eq!("!(3.3 > 3.2)", x.serialize());
    }

    /// float(7) / 2 = 3.5
    #[test]
    fn casting_serialize() {
        let x = Token::CastToFloat(Box::new(Token::IntPrim(7)));
        let x = Token::Div(Box::new(x), Box::new(Token::IntPrim(2)));
        assert_eq!("(float(7) / 2)", x.serialize());
    }
}
