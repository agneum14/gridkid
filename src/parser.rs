use anyhow::{bail, Result};

use crate::{
    lexer::{diag as lexer_diag, lex, Token, TokenKind},
    model::{Assignment, Block, Expr, Statement},
};

struct Parser {
    tokens: Vec<Token>,
    i: usize,
    src: String,
}

impl Parser {
    fn unary_tree(operator: &TokenKind, right: Expr) -> Expr {
        match operator {
            TokenKind::Minus => Expr::Neg(Box::new(right)),
            TokenKind::LogicNot => Expr::LogicNot(Box::new(right)),
            TokenKind::BitNot => Expr::BitwiseNot(Box::new(right)),
            TokenKind::IntCast => Expr::CastToInt(Box::new(right)),
            TokenKind::FloatCast => Expr::CastToFloat(Box::new(right)),
            _ => panic!(
                "tried to create a unary tree with invalid operator {:?}",
                operator
            ),
        }
    }

    fn binary_tree(left: Expr, operator: &TokenKind, right: Expr) -> Expr {
        match operator {
            TokenKind::LogicOr => Expr::LogicOr(Box::new(left), Box::new(right)),
            TokenKind::LogicAnd => Expr::LogicAnd(Box::new(left), Box::new(right)),
            TokenKind::BitOr => Expr::BitwiseOr(Box::new(left), Box::new(right)),
            TokenKind::BitXor => Expr::BitwiseXor(Box::new(left), Box::new(right)),
            TokenKind::BitAnd => Expr::BitwiseAnd(Box::new(left), Box::new(right)),
            TokenKind::EqualTo => Expr::Equals(Box::new(left), Box::new(right)),
            TokenKind::NotEqualTo => Expr::NotEquals(Box::new(left), Box::new(right)),
            TokenKind::LessThan => Expr::LessThan(Box::new(left), Box::new(right)),
            TokenKind::LessThanOrEqualTo => Expr::LessThanOrEquals(Box::new(left), Box::new(right)),
            TokenKind::GreaterThan => Expr::GreaterThan(Box::new(left), Box::new(right)),
            TokenKind::GreaterThanOrEqualTo => {
                Expr::GreaterThanOrEquals(Box::new(left), Box::new(right))
            }
            TokenKind::BitshiftLeft => Expr::BitwiseLeftShift(Box::new(left), Box::new(right)),
            TokenKind::BitshiftRight => Expr::BitwiseRightShift(Box::new(left), Box::new(right)),
            TokenKind::Add => Expr::Add(Box::new(left), Box::new(right)),
            TokenKind::Minus => Expr::Sub(Box::new(left), Box::new(right)),
            TokenKind::Mult => Expr::Mult(Box::new(left), Box::new(right)),
            TokenKind::Div => Expr::Div(Box::new(left), Box::new(right)),
            TokenKind::Mod => Expr::Mod(Box::new(left), Box::new(right)),
            TokenKind::Exp => Expr::Exp(Box::new(left), Box::new(right)),
            TokenKind::Max => Expr::Max(Box::new(left), Box::new(right)),
            TokenKind::Min => Expr::Min(Box::new(left), Box::new(right)),
            TokenKind::Mean => Expr::Mean(Box::new(left), Box::new(right)),
            TokenKind::Sum => Expr::Sum(Box::new(left), Box::new(right)),
            _ => panic!(
                "tried to create binary tree with invalid operator {:?}",
                operator
            ),
        }
    }

    fn peek(&self) -> Option<&TokenKind> {
        match self.tokens.get(self.i) {
            Some(v) => Some(&v.kind),
            None => None,
        }
    }

    fn previous(&self) -> TokenKind {
        self.tokens
            .get(self.i - 1)
            .expect("indexed tokens at -1")
            .kind
    }

    fn prev_token(&self) -> Result<&Token> {
        if self.i != 0 {
            if let Some(v) = self.tokens.get(self.i - 1) {
                return Ok(v);
            }
        }
        bail!(diag(self.src.as_str(), "None", self.src.len()))
    }

    fn advance(&mut self) {
        self.i += 1;
    }

    fn check(&mut self, kind: &TokenKind) -> bool {
        if let Some(v) = self.peek() {
            if v == kind {
                self.advance();
                return true;
            }
        }
        false
    }

    fn demand(&mut self, kind: &TokenKind) -> Result<()> {
        if self.check(kind) {
            Ok(())
        } else {
            let token = self.prev_token()?;
            bail!(diag(self.src.as_str(), token.src.as_str(), token.start))
        }
    }

    fn block(&mut self) -> Result<Block> {
        let mut statements: Vec<Statement> = Vec::new();
        loop {
            let statement = self.statement()?;
            if statement.is_none() {
                break;
            }
            statements.push(statement.unwrap());
        }

        let expr = self.expression()?;
        Ok(Block::new(statements, expr))
    }

    fn statement(&mut self) -> Result<Option<Statement>> {
        if self.check(&TokenKind::Ident) {
            Ok(Some(Statement::Assignment(self.assignment()?)))
        } else {
            Ok(None)
        }
    }

    fn assignment(&mut self) -> Result<Assignment> {
        self.demand(&TokenKind::Ident)?;
        let name = self.prev_token()?.src.to_owned();
        self.demand(&TokenKind::Equals)?;
        let value = self.expression()?;
        Ok(Assignment::new(name, value))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.logic_or()
    }

    fn logic_or(&mut self) -> Result<Expr> {
        let mut expr = self.logic_and()?;
        while self.check(&TokenKind::LogicOr) {
            let operator = self.previous();
            let right = self.logic_and()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr> {
        let mut expr = self.bit_or()?;
        while self.check(&TokenKind::LogicAnd) {
            let operator = self.previous();
            let right = self.bit_or()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }
    fn bit_or(&mut self) -> Result<Expr> {
        let mut expr = self.bit_and()?;
        while self.check(&TokenKind::BitOr) {
            let operator = self.previous();
            let right = self.bit_and()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn bit_and(&mut self) -> Result<Expr> {
        let mut expr = self.eq()?;
        while self.check(&TokenKind::BitAnd) {
            let operator = self.previous();
            let right = self.eq()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn eq(&mut self) -> Result<Expr> {
        let mut expr = self.meq()?;
        while self.check(&TokenKind::EqualTo) || self.check(&TokenKind::NotEqualTo) {
            let operator = self.previous();
            let right = self.meq()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn meq(&mut self) -> Result<Expr> {
        let mut expr = self.shift()?;
        while self.check(&TokenKind::LessThan)
            || self.check(&TokenKind::LessThanOrEqualTo)
            || self.check(&TokenKind::GreaterThan)
            || self.check(&TokenKind::GreaterThanOrEqualTo)
        {
            let operator = self.previous();
            let right = self.shift()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn shift(&mut self) -> Result<Expr> {
        let mut expr = self.add()?;
        while self.check(&TokenKind::BitshiftLeft) || self.check(&TokenKind::BitshiftRight) {
            let operator = self.previous();
            let right = self.add()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn add(&mut self) -> Result<Expr> {
        let mut expr = self.mult()?;
        while self.check(&TokenKind::Add) || self.check(&TokenKind::Minus) {
            let operator = self.previous();
            let right = self.mult()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn mult(&mut self) -> Result<Expr> {
        let mut expr = self.exp()?;
        while self.check(&TokenKind::Mult)
            || self.check(&TokenKind::Div)
            || self.check(&TokenKind::Mod)
        {
            let operator = self.previous();
            let right = self.exp()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn exp(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        if self.check(&TokenKind::Exp) {
            let operator = self.previous();
            let right = self.exp()?;
            expr = Parser::binary_tree(expr, &operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.check(&TokenKind::LogicNot)
            || self.check(&TokenKind::BitNot)
            || self.check(&TokenKind::Minus)
        {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(Parser::unary_tree(&operator, right));
        }

        self.primary()
    }

    fn paren_expression(&mut self) -> Result<Expr> {
        self.demand(&TokenKind::LeftParen)?;
        let inner = self.expression()?;
        self.demand(&TokenKind::RightParen)?;
        Ok(inner)
    }

    fn lvalue(&mut self) -> Result<Expr> {
        self.demand(&TokenKind::LeftSquare)?;
        let x = self.expression()?;
        self.demand(&TokenKind::Comma)?;
        let y = self.expression()?;
        self.demand(&TokenKind::RightSquare)?;
        Ok(Expr::LValue(Box::new(x), Box::new(y)))
    }

    fn stat_inner(&mut self) -> Result<(Expr, Expr)> {
        self.demand(&TokenKind::LeftParen)?;
        let left = self.lvalue()?;
        self.demand(&TokenKind::Comma)?;
        let right = self.lvalue()?;
        self.demand(&TokenKind::RightParen)?;
        Ok((left, right))
    }

    fn primary(&mut self) -> Result<Expr> {
        self.advance();
        let token = self.prev_token()?;

        match token.kind {
            TokenKind::Int => {
                let inner: i64 = token.src.parse()?;
                Ok(Expr::IntPrim(inner))
            }
            TokenKind::Float => {
                let inner: f64 = token.src.parse()?;
                Ok(Expr::FloatPrim(inner))
            }
            TokenKind::Bool => {
                let inner: bool = token.src.parse()?;
                Ok(Expr::BoolPrim(inner))
            }
            TokenKind::Str => {
                let inner = token.src.clone();
                // remove outer quotations
                let len = inner.len();
                let inner: String = inner.chars().skip(1).take(len - 2).collect();
                Ok(Expr::StringPrim(inner))
            }
            TokenKind::FloatCast | TokenKind::IntCast => {
                let cast = &token.kind.clone();
                let expr = self.paren_expression()?;
                Ok(Self::unary_tree(cast, expr))
            }
            TokenKind::Max | TokenKind::Min | TokenKind::Mean | TokenKind::Sum => {
                let stat = token.kind.clone();
                let (left, right) = self.stat_inner()?;
                Ok(Self::binary_tree(left, &stat, right))
            }
            TokenKind::Pound => {
                self.demand(&TokenKind::LeftSquare)?;
                let x = self.expression()?;
                self.demand(&TokenKind::Comma)?;
                let y = self.expression()?;
                self.demand(&TokenKind::RightSquare)?;
                Ok(Expr::RValue(Box::new(x), Box::new(y)))
            }
            TokenKind::LeftSquare => {
                self.i -= 1;
                self.lvalue()
            }
            _ => {
                self.i -= 1; // decrement because paren_expression demands "()"
                self.paren_expression()
            }
        }
    }
}

fn diag(src: &str, found: &str, pos: usize) -> String {
    let src: Box<[char]> = src.chars().collect();
    lexer_diag(&src, "token", found, pos, None)
}

pub fn parse(src: &str) -> Result<Expr> {
    let tokens = lex(src)?;
    let mut parser = Parser {
        tokens,
        i: 0,
        src: src.to_string(),
    };
    parser.expression()
}

#[cfg(test)]
mod tests {
    use crate::model::Runtime;
    use indoc::indoc;

    use super::*;

    #[test]
    fn simple() {
        let res = parse("1 + 7 * 4 + 10")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(39), res);
    }

    #[test]
    fn exp() {
        let res = parse("3 ** 4 ** 2")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(43046721), res);
    }

    #[test]
    fn lvalue() {
        let res = parse("[1 + 1, 7]")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::AddrPrim(2, 7), res)
    }

    #[test]
    fn complex() {
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(9, 5), &Expr::IntPrim(7))
            .unwrap();
        let res = parse("#[(1 ** 1 + 7 * (2 - ~1)) / 3, (1 << 2 | 3) ** 2 / 9] == 7 == 7 > 1")
            .unwrap()
            .eval(&runtime)
            .unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    #[test]
    fn paren() {
        let res = parse("2 * (2 + 7)")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(18), res)
    }

    #[test]
    fn unary_bool() {
        let res = parse("!false").unwrap().eval(&Runtime::default()).unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    #[test]
    fn unary_int_paren() {
        let res = parse("10 + ~(3)")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(6), res);
    }

    #[test]
    fn unary_minus_basic() {
        let res = parse("1 - -2").unwrap().eval(&Runtime::default()).unwrap();
        assert_eq!(Expr::IntPrim(3), res);
    }

    #[test]
    fn unary_minus_complex() {
        let res = parse("1 - -(-3 - 1)")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(-3), res);
    }

    #[test]
    fn unary_minus_float() {
        let res = parse("1 - -(-3.0 - 1)")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::FloatPrim(-3.0), res);
    }

    #[test]
    fn comparisons() {
        let res = parse("1 <= 1 && 1 < 2 && 2 >= 2 && 2 > 1 && 1 != 2")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    #[test]
    fn invalid_end() {
        let res = parse("2 + 2 + ");
        let expected = indoc! {"
            2 + 2 + 
                    ^ <--- WRONG
            invalid token 'None' at position 8"};
        if let Err(e) = res {
            assert_eq!(expected, e.to_string());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn invalid_token() {
        let res = parse("2 + 2 + ^ + 2");
        let expected = indoc! {"
            2 + 2 + ^ + 2
                    ^ <--- WRONG
            invalid token '^' at position 8"};
        if let Err(e) = res {
            assert_eq!(expected, e.to_string());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn invalid_single_op() {
        let res = parse("+");
        let expected = indoc! {"
            +
             ^ <--- WRONG
            invalid token 'None' at position 1"};
        if let Err(e) = res {
            assert_eq!(expected, e.to_string());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn arithmetic() {
        let res = parse("(5 + 2) * 3 % 4")
            .unwrap()
            .eval(&Runtime::default())
            .unwrap();
        assert_eq!(Expr::IntPrim(1), res);
    }

    #[test]
    fn rvalue_shift() {
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(0, 0), &Expr::IntPrim(1))
            .unwrap();
        let res = parse("#[0, 0] + 3").unwrap().eval(&runtime).unwrap();
        assert_eq!(Expr::IntPrim(4), res);
    }

    #[test]
    fn rvalue_cmp() {
        let mut runtime = Runtime::default();
        runtime
            .set_cell(&Expr::AddrPrim(0, 0), &Expr::IntPrim(1))
            .unwrap();
        runtime
            .set_cell(&Expr::AddrPrim(1, 1), &Expr::IntPrim(7))
            .unwrap();
        let res = parse("#[1 - 1, 0] < #[1 * 1, 1]")
            .unwrap()
            .eval(&runtime)
            .unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    #[test]
    fn logic_cmp() {
        let runtime = Runtime::default();
        let res = parse("5 > 3 && !(2 > 8)").unwrap().eval(&runtime).unwrap();
        assert_eq!(Expr::BoolPrim(true), res);
    }

    // the sum test can be found in model

    #[test]
    fn casting() {
        let runtime = Runtime::default();
        let res = parse("float(10) / 4.0").unwrap().eval(&runtime).unwrap();
        assert_eq!(Expr::FloatPrim(2.5), res);
    }
}
