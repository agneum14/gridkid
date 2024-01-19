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
            Self::Add(_, _) => write!(f, "Add"),
            Self::Sub(_, _) => write!(f, "Sub"),
            Self::Mult(_, _) => write!(f, "Mult"),
            Self::Div(_, _) => write!(f, "Div"),
            Self::Mod(_, _) => write!(f, "Mod"),
            Self::Exp(_, _) => write!(f, "Exp"),
            Self::CastToInt(_) => write!(f, "CastToInt"),
            Self::CastToFloat(_) => write!(f, "CastToFloat"),
        }
    }
}

impl Token {
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
    pub fn eval(&self) -> Result<Token> {
        match self {
            Self::IntPrim(_) | Self::FloatPrim(_) | Self::BoolPrim(_) => Ok(self.clone()),
            Self::Add(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av + bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av + bv)),
                    _ => bail!("cannot add types {} and {}", a, b),
                }
            }
            Self::Sub(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av - bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av - bv)),
                    _ => bail!("cannot sub types {} and {}", a, b),
                }
            }
            Self::Mult(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                let (a, b) = &Self::coerce(a, b);
                match (a, b) {
                    (Token::IntPrim(av), Token::IntPrim(bv)) => Ok(Token::IntPrim(av * bv)),
                    (Token::FloatPrim(av), Token::FloatPrim(bv)) => Ok(Token::FloatPrim(av * bv)),
                    _ => bail!("cannot mult types {} and {}", a, b),
                }
            }
            Self::Div(a, b) => {
                let (a, b) = &(a.eval()?, b.eval()?);
                let (a, b) = &Self::coerce(a, b);
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
            Self::CastToInt(a) => {
                let a = &a.eval()?;
                match a {
                    Token::IntPrim(_) => Ok(a.clone()),
                    Token::FloatPrim(v) => Ok(Token::IntPrim(*v as i64)),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
                }
            }
            Self::CastToFloat(a) => {
                let a = &a.eval()?;
                match a {
                    Token::IntPrim(v) => Ok(Token::FloatPrim(*v as f64)),
                    Token::FloatPrim(_) => Ok(a.clone()),
                    _ => bail!("cannot cast type {} to FloatPrim", a),
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

    // float(7) / 2 = 3.5
    #[test]
    fn casting() {
        let x = Token::CastToFloat(Box::new(Token::IntPrim(7)));
        let x = Token::Div(Box::new(x), Box::new(Token::IntPrim(2)));
        let x = x.eval().unwrap();
        assert_eq!(Token::FloatPrim(3.5), x);
    }
}
