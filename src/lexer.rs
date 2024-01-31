use anyhow::{bail, Result};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum LexemeKind {
    LogicOr,
    LogicAnd,
    BitOr,
    BitXor,
    BitAnd,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    BitshiftRight,
    BitshiftLeft,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Exp,
    LeftParen,
    RightParen,
    FloatCast,
    IntCast,
    LogicNot,
    BitNot,
    Pound,
    Comma,
    LeftSquare,
    RightSquare,
    Int,
    Float,
    Bool,
    Str,
}

#[derive(Debug)]
pub struct Lexeme {
    pub src: String,
    pub kind: LexemeKind,
}

struct Lexer {
    tokens: Vec<Lexeme>,
    token_so_far: String,
    i: usize,
    src: Box<[char]>,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            tokens: Vec::new(),
            token_so_far: "".to_string(),
            i: 0,
            src: src.chars().collect(),
        }
    }

    fn get(&self) -> Option<&char> {
        self.src.get(self.i)
    }

    fn emit_token(&mut self, kind: LexemeKind) {
        let token = Lexeme {
            src: self.token_so_far.to_owned(),
            kind,
        };
        self.tokens.push(token);
        self.token_so_far.clear();
    }

    fn has(&self, targ: char) -> bool {
        if let Some(c) = self.get() {
            return *c == targ;
        }
        false
    }

    fn has_digit(&self) -> bool {
        if let Some(c) = self.get() {
            if c.is_digit(10) {
                return true;
            }
        }
        false
    }

    fn cap(&mut self) {
        let c = self
            .get()
            .expect(&format!("invalid cap at index {}", self.i));
        self.token_so_far.push(*c);
        self.i += 1;
    }

    fn cap_targ(&mut self, targ: char) -> Result<()> {
        if let Some(c) = self.get() {
            if *c == targ {
                self.cap();
                return Ok(());
            }
        }
        bail!("expected {} at index {}", targ, self.i)
    }

    fn cap_digit(&mut self) -> Result<()> {
        if let Some(c) = self.get() {
            if c.is_digit(10) {
                self.cap();
                return Ok(());
            }
        }
        bail!("expected digit at index {}", self.i)
    }

    fn cap_emit(&mut self, kind: LexemeKind) {
        self.cap();
        self.emit_token(kind);
    }

    fn lex(&mut self) -> Result<()> {
        while self.i < self.src.len() {
            match self.src.get(self.i).unwrap() {
                '+' => self.cap_emit(LexemeKind::Add),
                '-' => self.cap_emit(LexemeKind::Sub),
                '/' => self.cap_emit(LexemeKind::Div),
                '%' => self.cap_emit(LexemeKind::Mod),
                '(' => self.cap_emit(LexemeKind::LeftParen),
                ')' => self.cap_emit(LexemeKind::RightParen),
                '~' => self.cap_emit(LexemeKind::BitNot),
                '[' => self.cap_emit(LexemeKind::LeftSquare),
                ']' => self.cap_emit(LexemeKind::RightSquare),
                '#' => self.cap_emit(LexemeKind::Pound),
                ',' => self.cap_emit(LexemeKind::Comma),
                '|' => {
                    self.cap();
                    match self.get() {
                        Some('|') => self.cap_emit(LexemeKind::LogicOr),
                        _ => self.emit_token(LexemeKind::BitOr)
                    }
                }
                '&' => {
                    self.cap();
                    match self.get() {
                        Some('&') => self.cap_emit(LexemeKind::LogicAnd),
                        _ => self.emit_token(LexemeKind::BitAnd)
                    }
                }
                '=' => {
                    self.cap();
                    self.cap_targ('=')?;
                    self.emit_token(LexemeKind::EqualTo);
                }
                '!' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::NotEqualTo),
                        _ => self.emit_token(LexemeKind::LogicNot),
                    }
                }
                '<' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::LessThanOrEqualTo),
                        Some('<') => self.cap_emit(LexemeKind::BitshiftLeft),
                        _ => self.emit_token(LexemeKind::LessThan)
                    }
                }
                '>' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::GreaterThanOrEqualTo),
                        Some('>') => self.cap_emit(LexemeKind::BitshiftRight),
                        _ => self.emit_token(LexemeKind::GreaterThan)
                    }
                }
                '*' => {
                    self.cap();
                    match self.get() {
                        Some('*') => self.cap_emit(LexemeKind::Exp),
                        _ => self.emit_token(LexemeKind::Mult)
                    }
                }
                _ if self.has_digit() => {
                    self.cap();
                    while self.has_digit() {
                        self.cap();
                    }

                    if self.has('.') {
                        self.cap();
                        self.cap_digit()?;
                        while self.has_digit() {
                            self.cap();
                        }
                        self.emit_token(LexemeKind::Float);
                    } else {
                        self.emit_token(LexemeKind::Int);
                    }
                }
                &c if c.is_alphabetic() => {
                    self.cap();
                    while c.is_alphabetic() {
                        self.cap();
                    }

                    match self.token_so_far.as_str() {
                        "float" => self.emit_token(LexemeKind::FloatCast),
                        "int" => self.emit_token(LexemeKind::IntCast),
                        "false" | "true" => self.emit_token(LexemeKind::Bool),
                        _ => bail!("unknown identifier"),
                    }
                }
                '"' => {
                    self.cap();
                    while let Some(c) = self.get() {
                        if *c != '"' {
                            self.cap();
                        } else {
                            break;
                        }
                    }
                    self.cap_targ('"')?;
                    self.emit_token(LexemeKind::Str);
                }
                &c if c.is_whitespace() => self.i += 1,
                &c => bail!("invalid char {} at position {}", c, self.i),
            }
        }

        Ok(())
    }
}

pub fn lex(src: &str) -> Result<Vec<Lexeme>> {
    let mut lexer = Lexer::new(src);
    lexer.lex()?;
    Ok(lexer.tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cmp_toks(expected: &[LexemeKind], actual: &Vec<Lexeme>) -> bool {
        if expected.len() == actual.len() {
            for i in 0..expected.len() {
                let e = expected.get(i).unwrap();
                let a = &actual.get(i).unwrap().kind;
                if e != a {
                    return false;
                }
            }
        }
        true
    }

    #[test]
    fn invalid_char() {
        let tokens = lex("`");
        assert!(tokens.is_err())
    }

    #[test]
    fn invalid_float() {
        let tokens = lex("1.");
        assert!(tokens.is_err())
    }

    #[test]
    fn float() {
        let tokens = lex("1.5").unwrap();
        assert_eq!(LexemeKind::Float, tokens.get(0).unwrap().kind)
    }
    
    #[test]
    fn int_addition() {
        let tokens = lex("1 + 2").unwrap();
        let expected = [LexemeKind::Int, LexemeKind::Add, LexemeKind::Int].as_slice();
        assert!(cmp_toks(expected, &tokens))
    }

    #[test]
    fn string() {
        let tokens = lex("1 + 2 + \"hello world!!!\" + 3 + 4").unwrap();
        let expected = [
            LexemeKind::Int,
            LexemeKind::Add,
            LexemeKind::Str,
            LexemeKind::Add,
            LexemeKind::Int,
            LexemeKind::Add,
            LexemeKind::Int,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &tokens));
    }

    #[test]
    fn rvalue() {
        let tokens = lex("#[1 - 2.5, 0] * 5").unwrap();
        let expected = [
            LexemeKind::Pound,
            LexemeKind::LeftSquare,
            LexemeKind::Int,
            LexemeKind::Sub,
            LexemeKind::Float,
            LexemeKind::Comma,
            LexemeKind::Int,
            LexemeKind::RightSquare,
            LexemeKind::Mult,
            LexemeKind::Int,
        ].as_slice();
        assert!(cmp_toks(expected, &tokens));
    }

    #[test]
    fn lvalue() {
        let tokens = lex("[0, 0]").unwrap();
        let expected = [
            LexemeKind::LeftSquare,
            LexemeKind::Int,
            LexemeKind::Comma,
            LexemeKind::Int,
            LexemeKind::RightSquare,
        ].as_slice();
        assert!(cmp_toks(expected, &tokens));
    }
}
