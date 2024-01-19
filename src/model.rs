use std::fmt::Display;

use anyhow::{bail, Result};

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    // primatives
    IntPrim(i64),
    FloatPrim(f64),
    BoolPrim(bool),

    // arithmetic
    Add(Box<Token>, Box<Token>),
    Sub(Box<Token>, Box<Token>),
    Mult(Box<Token>, Box<Token>),
    Div(Box<Token>, Box<Token>),
    Mod(Box<Token>, Box<Token>),
    Exp(Box<Token>, Box<Token>),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntPrim(_) => write!(f, "IntPrim"),
            Self::FloatPrim(_) => write!(f, "FloatPrim"),
            Self::BoolPrim(_) => write!(f, "BoolPrim"),
            Self::Add(_, _) => write!(f, "Add"),
            Self::Sub(_, _) => write!(f, "Sub"),
            Self::Mult(_, _) => write!(f, "Mult"),
            Self::Div(_, _) => write!(f, "Div"),
            Self::Mod(_, _) => write!(f, "Mod"),
            Self::Exp(_, _) => write!(f, "Exp"),
        }
    }
}

impl Token {
    /// Evaluate a Token, recursing through the AST
    pub fn eval(&self) -> Result<Token> {
        match self {
            Self::IntPrim(_) | Self::FloatPrim(_) | Self::BoolPrim(_) => Ok(self.clone()),
            Self::Add(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av + bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av + bv)),
                    _ => bail!("cannot add types {} and {}", a, b),
                }
            }
            Self::Sub(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av - bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av - bv)),
                    _ => bail!("cannot sub types {} and {}", a, b),
                }
            }
            Self::Mult(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av * bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av * bv)),
                    _ => bail!("cannot mult types {} and {}", a, b),
                }
            }
            Self::Div(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av / bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av / bv)),
                    _ => bail!("cannot div types {} and {}", a, b),
                }
            }
            Self::Mod(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av % bv)),
                    _ => bail!("cannot mod types {} and {}", a, b),
                }
            }
            Self::Exp(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
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
        let x = x.eval().unwrap();
        assert_eq!(Token::IntPrim(7), x);
    }
}
