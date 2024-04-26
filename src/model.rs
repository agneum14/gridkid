use std::fmt::Display;

use anyhow::{bail, ensure, Result};
use derive_new::new;

pub const GRID_WIDTH: usize = 13;

#[derive(Debug)]
pub struct Cell {
    pub ast: Option<Expr>,
    pub eval: Option<Result<Expr>>,
    pub src: String,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            ast: None,
            eval: None,
            src: "".to_string(),
        }
    }
}

#[derive(Debug)]
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
    fn idx(addr: &Expr) -> Result<usize> {
        if let Expr::AddrPrim(x, y) = addr {
            ensure!(*x < Self::WIDTH && *y < Self::WIDTH);
            Ok(*x * Self::WIDTH + *y)
        } else {
            bail!("cannot index grid with type {}", addr)
        }
    }

    /// Get a cell reference from an AddrPrim
    fn cell(&self, addr: &Expr) -> Result<&Cell> {
        let idx = Self::idx(addr)?;
        Ok(self.cells.get(idx).unwrap())
    }

    /// Get a mutable cell reference from an AddrPrim
    fn cell_mut(&mut self, addr: &Expr) -> Result<&mut Cell> {
        let idx = Self::idx(addr)?;
        Ok(self.cells.get_mut(idx).unwrap())
    }
}

#[derive(Debug)]
pub struct Runtime {
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
    pub fn cell(&self, addr: &Expr) -> Result<&Cell> {
        self.grid.cell(addr)
    }

    /// Get a mutable cell reference from an AddrPrim
    pub fn cell_mut(&mut self, addr: &Expr) -> Result<&mut Cell> {
        self.grid.cell_mut(addr)
    }

    /// Set a cell from an AddrPrim and AST
    pub fn set_cell(&mut self, addr: &Expr, ast: &Expr) -> Result<()> {
        let eval = ast.eval(self);
        let cell = self.cell_mut(addr)?;
        cell.ast = Some(ast.clone());
        cell.eval = Some(eval);
        Ok(())
    }

    pub fn set_cell_ast(&mut self, addr: &Expr, ast: Option<Expr>) -> Result<()> {
        let cell = self.cell_mut(addr)?;
        cell.ast = ast;
        Ok(())
    }

    pub fn set_cell_eval(&mut self, addr: &Expr, eval: Option<Result<Expr>>) -> Result<()> {
        let cell = self.cell_mut(addr)?;
        cell.eval = eval;
        Ok(())
    }

    pub fn set_cell_src(&mut self, addr: &Expr, src: String) -> Result<()> {
        let cell = self.cell_mut(addr)?;
        cell.src = src;
        Ok(())
    }

    /// Get references to cells in a bounding box of two AddrPrims
    fn cell_range(&self, addr1: &Expr, addr2: &Expr) -> Result<Vec<&Cell>> {
        let (x1, y1, x2, y2) = match (addr1, addr2) {
            (Expr::AddrPrim(x1, y1), Expr::AddrPrim(x2, y2)) => (*x1, *y1, *x2, *y2),
            _ => bail!("cannot get cell range from types {} and {}", addr1, addr2),
        };
        ensure!(x1 < Grid::WIDTH && y1 < Grid::WIDTH && x2 < Grid::WIDTH && y2 < Grid::WIDTH);

        let mut cells: Vec<&Cell> = Vec::new();
        for x in x1.min(x2)..=x1.max(x2) {
            for y in y1.min(y2)..=y1.max(y2) {
                let cell = self.cell(&Expr::AddrPrim(x, y))?;
                cells.push(cell);
            }
        }

        Ok(cells)
    }

    /// Get the evals of cells in a bounding box of two AddrPrims
    fn eval_range(&self, addr1: &Expr, addr2: &Expr) -> Result<Vec<Expr>> {
        let cells = self.cell_range(addr1, addr2)?;

        let mut evals: Vec<Expr> = Vec::new();
        for cell in cells {
            let expr = match &cell.eval {
                Some(v) => match v {
                    Ok(v) => match v {
                        Expr::IntPrim(v) => Expr::FloatPrim(*v as f64),
                        Expr::FloatPrim(v) => Expr::FloatPrim(*v),
                        _ => bail!("cannot perform statistics on type {}", v),
                    },
                    Err(_) => bail!("one of the cells in range contains an error"),
                },
                None => bail!("accessed empty cell"),
            };
            evals.push(expr.clone());
        }
        Ok(evals)
    }
}

#[derive(new)]
pub struct Block {
    statements: Vec<Statement>,
    expr: Expr,
}

pub enum Statement {
    Assignment(Assignment),
}

#[derive(new)]
pub struct Assignment {
    name: String,
    value: Expr,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // primitives
    IntPrim(i64),
    FloatPrim(f64),
    BoolPrim(bool),
    StringPrim(String),
    AddrPrim(usize, usize),

    // arithmetic
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Exp(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),

    // logical
    LogicAnd(Box<Expr>, Box<Expr>),
    LogicOr(Box<Expr>, Box<Expr>),
    LogicNot(Box<Expr>),

    // cell values
    RValue(Box<Expr>, Box<Expr>),
    LValue(Box<Expr>, Box<Expr>),

    // bitwise
    BitwiseAnd(Box<Expr>, Box<Expr>),
    BitwiseOr(Box<Expr>, Box<Expr>),
    BitwiseXor(Box<Expr>, Box<Expr>),
    BitwiseNot(Box<Expr>),
    BitwiseLeftShift(Box<Expr>, Box<Expr>),
    BitwiseRightShift(Box<Expr>, Box<Expr>),

    // relational
    Equals(Box<Expr>, Box<Expr>),
    NotEquals(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEquals(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEquals(Box<Expr>, Box<Expr>),

    // casting
    CastToInt(Box<Expr>),
    CastToFloat(Box<Expr>),

    // statistical
    Max(Box<Expr>, Box<Expr>),
    Min(Box<Expr>, Box<Expr>),
    Mean(Box<Expr>, Box<Expr>),
    Sum(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntPrim(v) => write!(f, "IntPrim({})", v),
            Self::FloatPrim(v) => write!(f, "FloatPrim({})", v),
            Self::BoolPrim(v) => write!(f, "BoolPrim({})", v),
            Self::StringPrim(v) => write!(f, "StringPrim({})", v),
            Self::AddrPrim(_, _) => write!(f, "BoolPrim"),
            Self::Add(_, _) => write!(f, "Add"),
            Self::Sub(_, _) => write!(f, "Sub"),
            Self::Mult(_, _) => write!(f, "Mult"),
            Self::Div(_, _) => write!(f, "Div"),
            Self::Mod(_, _) => write!(f, "Mod"),
            Self::Exp(_, _) => write!(f, "Exp"),
            Self::Neg(_) => write!(f, "Neg"),
            Self::LogicAnd(_, _) => write!(f, "LogicAnd"),
            Self::LogicOr(_, _) => write!(f, "LogicOr"),
            Self::LogicNot(_) => write!(f, "LogicNot"),
            Self::RValue(_, _) => write!(f, "RValue"),
            Self::LValue(_, _) => write!(f, "LValue"),
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
            Self::Max(_, _) => write!(f, "Max"),
            Self::Min(_, _) => write!(f, "Min"),
            Self::Mean(_, _) => write!(f, "Mean"),
            Self::Sum(_, _) => write!(f, "Sum"),
        }
    }
}

impl Expr {
    /// Get the inner value of an FloatPrim
    fn inner_float_prim(&self) -> Result<f64> {
        match self {
            Expr::FloatPrim(a) => Ok(*a),
            _ => bail!("{} is not a FloatPrim", self),
        }
    }

    /// Get the inner value of an IntPrim
    fn inner_int_prim(&self) -> Result<i64> {
        match self {
            Expr::IntPrim(a) => Ok(*a),
            _ => bail!("{} is not an IntPrim", self),
        }
    }

    /// Create an AddrPrim from two IntPrims with bounds checking
    fn new_addr_prim(x: &Expr, y: &Expr) -> Result<Expr> {
        let (x, y) = match (x, y) {
            (Expr::IntPrim(x), Expr::IntPrim(y)) => (*x, *y),
            _ => bail!("cannot create AddrPrim from types {} and {}", x, y),
        };
        ensure!(x >= 0 && x < Grid::WIDTH as i64 && y >= 0 && y < Grid::WIDTH as i64);
        Ok(Expr::AddrPrim(x as usize, y as usize))
    }

    /// Converts an IntPrim and a FloatPrim to FloatPrims. Other exprs are unchanged.
    fn coerce(a: &Expr, b: &Expr) -> (Expr, Expr) {
        if let (Expr::IntPrim(av), Expr::FloatPrim(_)) = (a, b) {
            (Expr::FloatPrim(*av as f64), b.clone())
        } else if let (Expr::FloatPrim(_), Expr::IntPrim(bv)) = (a, b) {
            (a.clone(), Expr::FloatPrim(*bv as f64))
        } else {
            (a.clone(), b.clone())
        }
    }

    /// Evaluate an Expr, recursing through the AST
    pub fn eval(&self, runtime: &Runtime) -> Result<Expr> {
        match self {
            Self::IntPrim(_)
            | Self::FloatPrim(_)
            | Self::BoolPrim(_)
            | Self::StringPrim(_)
            | Self::AddrPrim(_, _) => Ok(self.clone()),
            Self::Add(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av + bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::FloatPrim(av + bv)),
                    _ => bail!("cannot add types {} and {}", a, b),
                }
            }
            Self::Sub(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av - bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::FloatPrim(av - bv)),
                    _ => bail!("cannot sub types {} and {}", a, b),
                }
            }
            Self::Mult(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av * bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::FloatPrim(av * bv)),
                    _ => bail!("cannot mult types {} and {}", a, b),
                }
            }
            Self::Div(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av / bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::FloatPrim(av / bv)),
                    _ => bail!("cannot div types {} and {}", a, b),
                }
            }
            Self::Mod(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av % bv)),
                    _ => bail!("cannot mod types {} and {}", a, b),
                }
            }
            Self::Exp(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::IntPrim(av.pow(*bv as u32))),
                    (Expr::IntPrim(av), Expr::FloatPrim(bv)) => {
                        Ok(Expr::FloatPrim((*av as f64).powf(*bv)))
                    }
                    (Expr::FloatPrim(av), Expr::IntPrim(bv)) => {
                        Ok(Expr::FloatPrim(av.powi(*bv as i32)))
                    }
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::FloatPrim(av.powf(*bv))),
                    _ => bail!("cannot exponentiate types {} and {}", a, b),
                }
            }
            Self::Neg(a) => {
                let a = a.eval(runtime)?;
                match a {
                    Expr::IntPrim(av) => Ok(Expr::IntPrim(-1 * av)),
                    Expr::FloatPrim(av) => Ok(Expr::FloatPrim(-1.0 * av)),
                    _ => bail!("cannot negate type {}", a),
                }
            }
            Self::LogicAnd(a, b) => {
                let a = &a.eval(runtime)?;
                if let Expr::BoolPrim(av) = *a {
                    if !av {
                        return Ok(Expr::BoolPrim(false));
                    }

                    let b = &b.eval(runtime)?;
                    if let Expr::BoolPrim(bv) = *b {
                        return Ok(Expr::BoolPrim(av && bv));
                    }
                }

                bail!("cannot logic and types {} and {}", a, b);
            }
            Self::LogicOr(a, b) => {
                let a = &a.eval(runtime)?;
                if let Expr::BoolPrim(av) = *a {
                    if av {
                        return Ok(Expr::BoolPrim(true));
                    }

                    let b = &b.eval(runtime)?;
                    if let Expr::BoolPrim(bv) = *b {
                        return Ok(Expr::BoolPrim(av || bv));
                    }
                }

                bail!("cannot logic or types {} and {}", a, b);
            }
            Self::LogicNot(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Expr::BoolPrim(av) => Ok(Expr::BoolPrim(!av)),
                    _ => bail!("cannot logic not type {}", a),
                }
            }
            Self::RValue(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let addr = Self::new_addr_prim(a, b)?;
                let cell = runtime.cell(&addr)?;
                let (x, y) = (a.inner_int_prim()?, b.inner_int_prim()?);
                let eval = match &cell.eval {
                    Some(v) => match v {
                        Ok(v) => v.clone(),
                        Err(_) => bail!("accessed cell with error ({}, {})", x, y),
                    },
                    None => bail!("accessed empty cell ({}, {})", x, y),
                };
                Ok(eval)
            }
            Self::LValue(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                Self::new_addr_prim(a, b)
            }
            Self::BitwiseAnd(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Self::IntPrim(av & bv)),
                    _ => bail!("cannot bitwise and types {} and {}", a, b),
                }
            }
            Self::BitwiseOr(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Self::IntPrim(av | bv)),
                    _ => bail!("cannot bitwise or types {} and {}", a, b),
                }
            }
            Self::BitwiseXor(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Self::IntPrim(av ^ bv)),
                    _ => bail!("cannot bitwise xor types {} and {}", a, b),
                }
            }
            Self::BitwiseNot(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Expr::IntPrim(av) => Ok(Expr::IntPrim(!av)),
                    _ => bail!("cannot bitwise not type {}", a),
                }
            }
            Self::BitwiseLeftShift(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Self::IntPrim(av << bv)),
                    _ => bail!("cannot left shift types {} and {}", a, b),
                }
            }
            Self::BitwiseRightShift(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Self::IntPrim(av >> bv)),
                    _ => bail!("cannot right shift types {} and {}", a, b),
                }
            }
            Self::CastToInt(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Expr::IntPrim(_) => Ok(a.clone()),
                    Expr::FloatPrim(v) => Ok(Expr::IntPrim(*v as i64)),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
                }
            }
            Self::CastToFloat(a) => {
                let a = &a.eval(runtime)?;
                match a {
                    Expr::IntPrim(v) => Ok(Expr::FloatPrim(*v as f64)),
                    Expr::FloatPrim(_) => Ok(a.clone()),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
                }
            }
            Self::Equals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av == bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av == bv)),
                    (Expr::BoolPrim(av), Expr::BoolPrim(bv)) => Ok(Expr::BoolPrim(av == bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::NotEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av != bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av != bv)),
                    (Expr::BoolPrim(av), Expr::BoolPrim(bv)) => Ok(Expr::BoolPrim(av != bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::LessThan(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av < bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av < bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::LessThanOrEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av <= bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av <= bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::GreaterThan(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av > bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av > bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::GreaterThanOrEquals(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Expr::IntPrim(av), Expr::IntPrim(bv)) => Ok(Expr::BoolPrim(av >= bv)),
                    (Expr::FloatPrim(av), Expr::FloatPrim(bv)) => Ok(Expr::BoolPrim(av >= bv)),
                    _ => bail!("cannot check equality for types {} and {}", a, b),
                }
            }
            Self::Max(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let vals: Vec<f64> = runtime
                    .eval_range(a, b)?
                    .iter()
                    .map(|x| x.inner_float_prim().unwrap())
                    .collect();
                let mut max = std::f64::NEG_INFINITY;
                for v in vals {
                    if v > max {
                        max = v;
                    }
                }
                Ok(Expr::FloatPrim(max))
            }
            Self::Min(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let vals: Vec<f64> = runtime
                    .eval_range(a, b)?
                    .iter()
                    .map(|x| x.inner_float_prim().unwrap())
                    .collect();
                let mut min = std::f64::INFINITY;
                for v in vals {
                    if v < min {
                        min = v;
                    }
                }
                Ok(Expr::FloatPrim(min))
            }
            Self::Mean(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let evals = runtime.eval_range(a, b)?;
                let mean = evals
                    .iter()
                    .map(|x| x.inner_float_prim().unwrap())
                    .sum::<f64>() as f64
                    / evals.len() as f64;
                Ok(Self::FloatPrim(mean))
            }
            Self::Sum(a, b) => {
                let (a, b) = &(a.eval(runtime)?, b.eval(runtime)?);
                let evals = runtime.eval_range(a, b)?;
                let sum = evals
                    .iter()
                    .map(|x| x.inner_float_prim().unwrap())
                    .sum::<f64>();
                Ok(Expr::FloatPrim(sum))
            }
        }
    }

    /// Serialize an Expr, recursing through the AST
    pub fn serialize(&self) -> String {
        match self {
            Self::IntPrim(a) => format!("{}", a),
            Self::FloatPrim(a) => format!("{}", a),
            Self::BoolPrim(a) => format!("{}", a),
            Self::StringPrim(a) => format!("\"{}\"", a),
            Self::AddrPrim(a, b) => format!("[{}, {}]", a, b),
            Self::Add(a, b) => format!("({} + {})", a.serialize(), b.serialize()),
            Self::Sub(a, b) => format!("({} - {})", a.serialize(), b.serialize()),
            Self::Mult(a, b) => format!("({} * {})", a.serialize(), b.serialize()),
            Self::Div(a, b) => format!("({} / {})", a.serialize(), b.serialize()),
            Self::Exp(a, b) => format!("({}^{})", a.serialize(), b.serialize()),
            Self::Neg(a) => format!("-{}", a.serialize()),
            Self::Mod(a, b) => format!("({} % {})", a.serialize(), b.serialize()),
            Self::LogicAnd(a, b) => format!("({} && {})", a.serialize(), b.serialize()),
            Self::LogicOr(a, b) => format!("({} || {})", a.serialize(), b.serialize()),
            Self::LogicNot(a) => format!("!{}", a.serialize()),
            Self::RValue(a, b) => format!("#[{}, {}]", a.serialize(), b.serialize()),
            Self::LValue(a, b) => format!("[{}, {}]", a.serialize(), b.serialize()),
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
            Self::Max(a, b) => format!("max({}, {})", a.serialize(), b.serialize()),
            Self::Min(a, b) => format!("min({}, {})", a.serialize(), b.serialize()),
            Self::Mean(a, b) => format!("mean({}, {})", a.serialize(), b.serialize()),
            Self::Sum(a, b) => format!("sum({}, {})", a.serialize(), b.serialize()),
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;

    use crate::parser::parse;

    use super::*;

    // Instantiate a Runtime with cells in the bounding box set to random IntPrims. Returns the
    // Runtime and the random IntPrims
    fn build_runtime_grid(x1: usize, y1: usize, x2: usize, y2: usize) -> (Runtime, Vec<i64>) {
        let mut runtime = Runtime::default();
        let mut nums: Vec<i64> = Vec::new();

        for x in x1.min(x2)..=x1.max(x2) {
            for y in y1.min(y2)..=y1.max(y2) {
                let n: i64 = rand::thread_rng().gen_range(0..1000);
                nums.push(n);
                runtime
                    .set_cell(&Expr::AddrPrim(x, y), &Expr::IntPrim(n))
                    .unwrap();
            }
        }

        (runtime, nums)
    }

    /// (7 * 4 + 3) % 12 = 7
    #[test]
    fn arithmetic() {
        let x = Expr::Mult(Box::new(Expr::IntPrim(7)), Box::new(Expr::IntPrim(4)));
        let x = Expr::Add(Box::new(x), Box::new(Expr::IntPrim(3)));
        let x = Expr::Mod(Box::new(x), Box::new(Expr::IntPrim(12)));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Expr::IntPrim(7), x);
    }

    // #[1 + 1, 4] << 3
    #[test]
    fn rvalue_shift() {
        // store 5 in (2, 4)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(2, 4), &Expr::IntPrim(5))
            .unwrap();

        let x = Expr::Add(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(1)));
        let x = Expr::RValue(Box::new(x), Box::new(Expr::IntPrim(4)));
        let x = Expr::BitwiseLeftShift(Box::new(x), Box::new(Expr::IntPrim(3)));
        let x = x.eval(&runtime).unwrap();
        assert_eq!(Expr::IntPrim(40), x)
    }

    /// #[0, 0] < #[0, 1]
    #[test]
    fn rvalue_cmp() {
        // store 1 in (0, 0) and 7 in (0, 1)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(0, 0), &Expr::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(0, 1), &Expr::IntPrim(7))
            .unwrap();

        let x = Expr::RValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(0)));
        let y = Expr::RValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(1)));
        let res = Expr::LessThan(Box::new(x), Box::new(y));
        let res = res.eval(&runtime).unwrap();
        assert_eq!(Expr::BoolPrim(true), res)
    }

    /// sum([1, 2], [5, 3])
    #[test]
    fn sum() {
        let (runtime, nums) = build_runtime_grid(1, 2, 5, 3);

        let x = Expr::LValue(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(2)));
        let y = Expr::LValue(Box::new(Expr::IntPrim(5)), Box::new(Expr::IntPrim(3)));
        let res = Expr::Sum(Box::new(x), Box::new(y)).eval(&runtime).unwrap();

        let sum = nums.iter().sum::<i64>();
        assert_eq!(Expr::FloatPrim(sum as f64), res)
    }

    /// sum([1, 2], [5, 3])
    #[test]
    fn sum_parsed() {
        let (runtime, nums) = build_runtime_grid(1, 2, 5, 3);
        let res = parse("sum([1, 2], [5, 3])")
            .unwrap()
            .eval(&runtime)
            .unwrap();
        let sum = nums.iter().sum::<i64>();
        assert_eq!(Expr::FloatPrim(sum as f64), res)
    }

    /// !(3.3 > 3.2)
    #[test]
    fn logic_cmp() {
        let x = Expr::GreaterThan(
            Box::new(Expr::FloatPrim(3.3)),
            Box::new(Expr::FloatPrim(3.2)),
        );
        let x = Expr::LogicNot(Box::new(x));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Expr::BoolPrim(false), x);
    }

    /// float(7) / 2 = 3.5
    #[test]
    fn casting() {
        let x = Expr::CastToFloat(Box::new(Expr::IntPrim(7)));
        let x = Expr::Div(Box::new(x), Box::new(Expr::IntPrim(2)));
        let x = x.eval(&Runtime::default()).unwrap();
        assert_eq!(Expr::FloatPrim(3.5), x);
    }

    /// (7 * 4 + 3) % 12 = 7
    #[test]
    fn arithmetic_serialize() {
        let x = Expr::Mult(Box::new(Expr::IntPrim(7)), Box::new(Expr::IntPrim(4)));
        let x = Expr::Add(Box::new(x), Box::new(Expr::IntPrim(3)));
        let x = Expr::Mod(Box::new(x), Box::new(Expr::IntPrim(12)));
        assert_eq!("(((7 * 4) + 3) % 12)", x.serialize());
    }

    /// #[1 + 1, 4] << 3
    #[test]
    fn rvalue_shift_serialize() {
        // store 5 in (2, 4)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(2, 4), &Expr::IntPrim(5))
            .unwrap();

        let x = Expr::Add(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(1)));
        let x = Expr::RValue(Box::new(x), Box::new(Expr::IntPrim(4)));
        let x = Expr::BitwiseLeftShift(Box::new(x), Box::new(Expr::IntPrim(3)));
        assert_eq!("(#[(1 + 1), 4] << 3)", x.serialize());
    }

    /// #[0, 0] < #[0, 1]
    #[test]
    fn rvalue_cmp_serialize() {
        // store 1 in (0, 0) and 7 in (0, 1)
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(0, 0), &Expr::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(0, 1), &Expr::IntPrim(7))
            .unwrap();

        let x = Expr::RValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(0)));
        let y = Expr::RValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(1)));
        let res = Expr::LessThan(Box::new(x), Box::new(y));
        assert_eq!("(#[0, 0] < #[0, 1])", res.serialize())
    }

    /// sum([1, 2], [5, 3])
    #[test]
    fn sum_serialize() {
        let x = Expr::LValue(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(2)));
        let y = Expr::LValue(Box::new(Expr::IntPrim(5)), Box::new(Expr::IntPrim(3)));
        let res = Expr::Sum(Box::new(x), Box::new(y));
        assert_eq!("sum([1, 2], [5, 3])", res.serialize())
    }

    /// !(3.3 > 3.2)
    #[test]
    fn logic_cmp_serialize() {
        let x = Expr::GreaterThan(
            Box::new(Expr::FloatPrim(3.3)),
            Box::new(Expr::FloatPrim(3.2)),
        );
        let x = Expr::LogicNot(Box::new(x));
        assert_eq!("!(3.3 > 3.2)", x.serialize());
    }

    /// float(7) / 2 = 3.5
    #[test]
    fn casting_serialize() {
        let x = Expr::CastToFloat(Box::new(Expr::IntPrim(7)));
        let x = Expr::Div(Box::new(x), Box::new(Expr::IntPrim(2)));
        assert_eq!("(float(7) / 2)", x.serialize());
    }

    #[test]
    fn sum_manual() {
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(1, 2), &Expr::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(1, 3), &Expr::IntPrim(2))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(1, 4), &Expr::IntPrim(3))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(2, 2), &Expr::IntPrim(4))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(2, 3), &Expr::IntPrim(5))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(2, 4), &Expr::IntPrim(6))
            .unwrap();

        let a = Expr::LValue(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(2)));
        let b = Expr::LValue(Box::new(Expr::IntPrim(2)), Box::new(Expr::IntPrim(4)));
        let res = Expr::Sum(Box::new(a), Box::new(b)).eval(&runtime).unwrap();
        assert_eq!(Expr::FloatPrim(21.0), res)
    }

    #[test]
    fn max() {
        let (runtime, nums) = build_runtime_grid(50, 90, 1, 35);

        let x = Expr::LValue(Box::new(Expr::IntPrim(50)), Box::new(Expr::IntPrim(90)));
        let y = Expr::LValue(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(35)));
        let res = Expr::Max(Box::new(y), Box::new(x)).eval(&runtime).unwrap();

        let max = nums.iter().max().unwrap();
        assert_eq!(Expr::FloatPrim(*max as f64), res)
    }

    #[test]
    fn min() {
        let (runtime, nums) = build_runtime_grid(0, 99, 0, 99);

        let x = Expr::LValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(99)));
        let y = Expr::LValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(99)));
        let res = Expr::Min(Box::new(y), Box::new(x)).eval(&runtime).unwrap();

        let min = nums.iter().min().unwrap();
        assert_eq!(Expr::FloatPrim(*min as f64), res)
    }

    #[test]
    fn mean() {
        let (runtime, nums) = build_runtime_grid(7, 24, 2, 15);

        let x = Expr::LValue(Box::new(Expr::IntPrim(7)), Box::new(Expr::IntPrim(24)));
        let y = Expr::LValue(Box::new(Expr::IntPrim(2)), Box::new(Expr::IntPrim(15)));
        let res = Expr::Mean(Box::new(y), Box::new(x)).eval(&runtime).unwrap();

        let mean = nums.iter().sum::<i64>() as f64 / nums.len() as f64;
        assert_eq!(Expr::FloatPrim(mean), res)
    }

    #[test]
    fn access_invalid_range() {
        let x = Expr::LValue(Box::new(Expr::IntPrim(0)), Box::new(Expr::IntPrim(100)));
        let x = x.eval(&Runtime::default());
        assert!(x.is_err());
    }

    #[test]
    fn access_empty_cell() {
        let x = Expr::RValue(Box::new(Expr::IntPrim(1)), Box::new(Expr::IntPrim(1)));
        let x = x.eval(&Runtime::default());
        assert!(x.is_err());
    }

    #[test]
    fn invalid_prims() {
        let x = Expr::Add(Box::new(Expr::IntPrim(5)), Box::new(Expr::BoolPrim(true)));
        let x = x.eval(&Runtime::default());
        assert!(x.is_err())
    }

    #[test]
    fn invalid_addition() {
        let x = parse("1 + true").unwrap().eval(&Runtime::default());
        assert!(x.is_err())
    }

    #[test]
    fn invalid_greater() {
        let x = parse("\"Ruby\" > \"nearly any other language (for me)\"")
            .unwrap()
            .eval(&Runtime::default()); // notice this results in a error
        assert!(x.is_err())
    }

    #[test]
    fn invalid_lvalue() {
        let x = parse("[1.5, 2]").unwrap().eval(&Runtime::default());
        assert!(x.is_err())
    }

    #[test]
    fn invalid_shift_left() {
        let x = parse("1 << 2.0").unwrap().eval(&Runtime::default());
        assert!(x.is_err());
    }

    #[test]
    fn sum2() {
        let (runtime, nums) = build_runtime_grid(0, 0, 2, 1);
        let sum = nums.iter().sum::<i64>();
        let expected = Expr::FloatPrim(sum as f64 + 1.0);
        let res = parse("1 + sum([0, 0], [2, 1])")
            .unwrap()
            .eval(&runtime)
            .unwrap();
        assert_eq!(expected, res)
    }

    #[test]
    fn short_circuit_or() {
        let res = parse("true || 7")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    #[test]
    fn short_circuit_and() {
        let res = parse("false && 7")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::BoolPrim(false), res);
    }
}
