use anyhow::{bail, Result};

use crate::{
    lexer::{lex, Lexeme, LexemeKind},
    model::Token,
};

struct Parser {
    lexemes: Vec<Lexeme>,
    i: usize,
}

impl Parser {
    fn unary_tree(operator: &LexemeKind, right: Token) -> Result<Token> {
        match operator {
            LexemeKind::LogicNot => Ok(Token::LogicNot(Box::new(right))),
            LexemeKind::BitNot => Ok(Token::BitwiseNot(Box::new(right))),
            _ => bail!("invalid unary operator"),
        }
    }

    fn binary_tree(left: Token, operator: &LexemeKind, right: Token) -> Result<Token> {
        match operator {
            LexemeKind::LogicOr => Ok(Token::LogicOr(Box::new(left), Box::new(right))),
            LexemeKind::LogicAnd => Ok(Token::LogicAnd(Box::new(left), Box::new(right))),
            LexemeKind::BitOr => Ok(Token::BitwiseOr(Box::new(left), Box::new(right))),
            LexemeKind::BitXor => Ok(Token::BitwiseXor(Box::new(left), Box::new(right))),
            LexemeKind::BitAnd => Ok(Token::BitwiseAnd(Box::new(left), Box::new(right))),
            LexemeKind::EqualTo => Ok(Token::Equals(Box::new(left), Box::new(right))),
            LexemeKind::NotEqualTo => Ok(Token::NotEquals(Box::new(left), Box::new(right))),
            LexemeKind::LessThan => Ok(Token::LessThan(Box::new(left), Box::new(right))),
            LexemeKind::LessThanOrEqualTo => {
                Ok(Token::LessThanOrEquals(Box::new(left), Box::new(right)))
            }
            LexemeKind::GreaterThan => Ok(Token::GreaterThan(Box::new(left), Box::new(right))),
            LexemeKind::GreaterThanOrEqualTo => {
                Ok(Token::GreaterThanOrEquals(Box::new(left), Box::new(right)))
            }
            LexemeKind::BitshiftLeft => {
                Ok(Token::BitwiseLeftShift(Box::new(left), Box::new(right)))
            }
            LexemeKind::BitshiftRight => {
                Ok(Token::BitwiseRightShift(Box::new(left), Box::new(right)))
            }
            LexemeKind::Add => Ok(Token::Add(Box::new(left), Box::new(right))),
            LexemeKind::Sub => Ok(Token::Sub(Box::new(left), Box::new(right))),
            LexemeKind::Mult => Ok(Token::Mult(Box::new(left), Box::new(right))),
            LexemeKind::Div => Ok(Token::Div(Box::new(left), Box::new(right))),
            LexemeKind::Mod => Ok(Token::Mod(Box::new(left), Box::new(right))),
            LexemeKind::Exp => Ok(Token::Exp(Box::new(left), Box::new(right))),
            _ => bail!("tried to create binary tree with invalid operator"),
        }
    }

    fn peek(&self) -> Option<&LexemeKind> {
        match self.lexemes.get(self.i) {
            Some(v) => Some(&v.kind),
            None => None,
        }
    }

    fn previous(&self) -> LexemeKind {
        self.lexemes
            .get(self.i - 1)
            .expect("indexed lexemes at -1")
            .kind
    }

    fn prev_lexeme(&self) -> &Lexeme {
        self.lexemes.get(self.i - 1).expect("indexed lexemes at -1")
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn check(&mut self, kind: &LexemeKind) -> bool {
        if let Some(v) = self.peek() {
            if v == kind {
                self.advance();
                return true;
            }
        }
        false
    }

    fn demand(&mut self, kind: &LexemeKind) -> Result<()> {
        match self.check(kind) {
            true => Ok(()),
            false => bail!("tmp msg"),
        }
    }

    fn expression(&mut self) -> Result<Token> {
        self.logic_or()
    }

    fn logic_or(&mut self) -> Result<Token> {
        let mut expr = self.logic_and()?;
        while self.check(&LexemeKind::LogicOr) {
            let operator = self.previous();
            let right = self.logic_and()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Token> {
        let mut expr = self.bit_or()?;
        while self.check(&LexemeKind::LogicAnd) {
            let operator = self.previous();
            let right = self.bit_or()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }
    fn bit_or(&mut self) -> Result<Token> {
        let mut expr = self.bit_and()?;
        while self.check(&LexemeKind::BitOr) {
            let operator = self.previous();
            let right = self.bit_and()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn bit_and(&mut self) -> Result<Token> {
        let mut expr = self.eq()?;
        while self.check(&LexemeKind::BitAnd) {
            let operator = self.previous();
            let right = self.eq()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn eq(&mut self) -> Result<Token> {
        let mut expr = self.meq()?;
        while self.check(&LexemeKind::EqualTo) || self.check(&LexemeKind::NotEqualTo) {
            let operator = self.previous();
            let right = self.meq()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn meq(&mut self) -> Result<Token> {
        let mut expr = self.shift()?;
        while self.check(&LexemeKind::LessThan)
            || self.check(&LexemeKind::LessThanOrEqualTo)
            || self.check(&LexemeKind::GreaterThan)
            || self.check(&LexemeKind::GreaterThanOrEqualTo)
        {
            let operator = self.previous();
            let right = self.shift()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn shift(&mut self) -> Result<Token> {
        let mut expr = self.add()?;
        while self.check(&LexemeKind::BitshiftLeft) || self.check(&LexemeKind::BitshiftRight) {
            let operator = self.previous();
            let right = self.add()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn add(&mut self) -> Result<Token> {
        let mut expr = self.mult()?;
        while self.check(&LexemeKind::Add) || self.check(&LexemeKind::Sub) {
            let operator = self.previous();
            let right = self.mult()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn mult(&mut self) -> Result<Token> {
        let mut expr = self.exp()?;
        while self.check(&LexemeKind::Mult)
            || self.check(&LexemeKind::Div)
            || self.check(&LexemeKind::Mod)
        {
            let operator = self.previous();
            let right = self.exp()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn exp(&mut self) -> Result<Token> {
        let mut expr = self.unary()?;
        if self.check(&LexemeKind::Exp) {
            let operator = self.previous();
            let right = self.exp()?;
            expr = Parser::binary_tree(expr, &operator, right)?;
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Token> {
        if self.check(&LexemeKind::Exp) {
            let operator = self.previous();
            let right = self.unary()?;
            return Parser::unary_tree(&operator, right);
        }

        self.primary()
    }

    fn paren_expression(&mut self) -> Result<Token> {
        self.demand(&LexemeKind::LeftParen)?;
        let inner = self.expression()?;
        self.demand(&LexemeKind::RightParen)?;
        Ok(inner)
    }

    fn primary(&mut self) -> Result<Token> {
        self.advance();
        let lexeme = self.prev_lexeme();

        match lexeme.kind {
            LexemeKind::Int => {
                let inner: i64 = lexeme.src.parse()?;
                Ok(Token::IntPrim(inner))
            }
            LexemeKind::Float => {
                let inner: f64 = lexeme.src.parse()?;
                Ok(Token::FloatPrim(inner))
            }
            LexemeKind::Bool => {
                let inner: bool = lexeme.src.parse()?;
                Ok(Token::BoolPrim(inner))
            }
            LexemeKind::Str => {
                let inner = lexeme.src.clone();
                // remove outer quotations
                let len = inner.len();
                let inner: String = inner.chars().skip(1).take(len - 2).collect();
                Ok(Token::StringPrim(inner))
            }
            LexemeKind::FloatCast => {
                let expr = self.paren_expression()?;
                Ok(Token::CastToFloat(Box::new(expr)))
            }
            LexemeKind::IntCast => {
                let expr = self.paren_expression()?;
                Ok(Token::CastToInt(Box::new(expr)))
            }
            LexemeKind::Pound => {
                self.demand(&LexemeKind::LeftSquare)?;
                let x = self.expression()?;
                self.demand(&LexemeKind::Comma)?;
                let y = self.expression()?;
                self.demand(&LexemeKind::RightSquare)?;
                Ok(Token::RValue(Box::new(x), Box::new(y)))
            }
            LexemeKind::LeftSquare => {
                let x = self.expression()?;
                self.demand(&LexemeKind::Comma)?;
                let y = self.expression()?;
                self.demand(&LexemeKind::RightSquare)?;
                Ok(Token::LValue(Box::new(x), Box::new(y)))
            }
            _ => {
                self.i -= 1; // decrement because paren_expression demands "()"
                self.paren_expression()
            }
        }
    }
}

pub fn parse(src: &str) -> Result<Token> {
    let lexemes = lex(src)?;
    let mut parser = Parser { lexemes, i: 0 };
    parser.expression()
}

#[cfg(test)]
mod tests {
    use crate::model::Runtime;

    use super::*;

    #[test]
    fn simple() {
        let res = parse("1 + 7 * 4 + 10")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Token::IntPrim(39), res);
    }

    #[test]
    fn exp() {
        let res = parse("3 ** 4 ** 2")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Token::IntPrim(43046721), res);
    }

    #[test]
    fn lvalue() {
        let res = parse("[1 + 1, 7]")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Token::AddrPrim(2, 7), res)
    }

    #[test]
    fn paren() {
        let res = parse("2 * (2 + 7)")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Token::IntPrim(18), res)
    }
}
