use anyhow::{bail, Result};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum TokenKind {
    DotDot,
    For,
    In,
    If,
    Else,
    RightCurly,
    LeftCurly,
    Ident,
    Semicolon,
    LogicOr,
    LogicAnd,
    BitOr,
    BitXor,
    BitAnd,
    Equals,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    BitshiftRight,
    BitshiftLeft,
    Add,
    Minus,
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
    Min,
    Max,
    Mean,
    Sum,
}

#[derive(Debug)]
pub struct Token {
    pub src: String,
    pub kind: TokenKind,
    pub start: usize,
}

struct Lexer {
    tokens: Vec<Token>,
    token_so_far: String,
    i: usize,
    start: usize,
    src: Box<[char]>,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            tokens: Vec::new(),
            token_so_far: "".to_string(),
            i: 0,
            start: 0,
            src: src.chars().collect(),
        }
    }

    fn get(&self) -> Option<&char> {
        self.src.get(self.i)
    }

    fn emit_token(&mut self, kind: TokenKind) {
        let token = Token {
            src: self.token_so_far.to_owned(),
            kind,
            start: self.start,
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
            bail!(diag(
                &self.src,
                "char",
                c.to_string().as_str(),
                self.i,
                Some(format!("expected '{}'", targ).as_str())
            ));
        }
        bail!(diag(
            &self.src,
            "char",
            "None",
            self.i,
            Some(format!("expected '{}'", targ).as_str())
        ));
    }

    fn cap_digit(&mut self) -> Result<()> {
        if let Some(c) = self.get() {
            if c.is_digit(10) {
                self.cap();
                return Ok(());
            }
            bail!(diag(
                &self.src,
                "char",
                c.to_string().as_str(),
                self.i,
                Some("expected digit")
            ));
        }
        bail!(diag(
            &self.src,
            "char",
            "None",
            self.i,
            Some("expected digit")
        ));
    }

    fn cap_emit(&mut self, kind: TokenKind) {
        self.cap();
        self.emit_token(kind);
    }

    fn is_alphabetic(&self) -> bool {
        if let Some(c) = self.get() {
            return c.is_alphabetic();
        }
        false
    }

    fn lex(&mut self) -> Result<()> {
        while self.i < self.src.len() {
            self.start = self.i;
            match self.src.get(self.i).unwrap() {
                ';' => self.cap_emit(TokenKind::Semicolon),
                '+' => self.cap_emit(TokenKind::Add),
                '-' => self.cap_emit(TokenKind::Minus),
                '/' => self.cap_emit(TokenKind::Div),
                '%' => self.cap_emit(TokenKind::Mod),
                '(' => self.cap_emit(TokenKind::LeftParen),
                ')' => self.cap_emit(TokenKind::RightParen),
                '~' => self.cap_emit(TokenKind::BitNot),
                '[' => self.cap_emit(TokenKind::LeftSquare),
                ']' => self.cap_emit(TokenKind::RightSquare),
                '#' => self.cap_emit(TokenKind::Pound),
                ',' => self.cap_emit(TokenKind::Comma),
                '{' => self.cap_emit(TokenKind::LeftCurly),
                '}' => self.cap_emit(TokenKind::RightCurly),
                '.' => {
                    self.cap();
                    self.cap_targ('.')?;
                    self.emit_token(TokenKind::DotDot)
                }
                '|' => {
                    self.cap();
                    match self.get() {
                        Some('|') => self.cap_emit(TokenKind::LogicOr),
                        _ => self.emit_token(TokenKind::BitOr),
                    }
                }
                '&' => {
                    self.cap();
                    match self.get() {
                        Some('&') => self.cap_emit(TokenKind::LogicAnd),
                        _ => self.emit_token(TokenKind::BitAnd),
                    }
                }
                '=' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(TokenKind::EqualTo),
                        _ => self.emit_token(TokenKind::Equals),
                    }
                }
                '!' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(TokenKind::NotEqualTo),
                        _ => self.emit_token(TokenKind::LogicNot),
                    }
                }
                '<' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(TokenKind::LessThanOrEqualTo),
                        Some('<') => self.cap_emit(TokenKind::BitshiftLeft),
                        _ => self.emit_token(TokenKind::LessThan),
                    }
                }
                '>' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(TokenKind::GreaterThanOrEqualTo),
                        Some('>') => self.cap_emit(TokenKind::BitshiftRight),
                        _ => self.emit_token(TokenKind::GreaterThan),
                    }
                }
                '*' => {
                    self.cap();
                    match self.get() {
                        Some('*') => self.cap_emit(TokenKind::Exp),
                        _ => self.emit_token(TokenKind::Mult),
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
                        self.emit_token(TokenKind::Float);
                    } else {
                        self.emit_token(TokenKind::Int);
                    }
                }
                _ if self.is_alphabetic() => {
                    self.cap();
                    while self.is_alphabetic() {
                        self.cap();
                    }

                    match self.token_so_far.as_str() {
                        "float" => self.emit_token(TokenKind::FloatCast),
                        "int" => self.emit_token(TokenKind::IntCast),
                        "false" | "true" => self.emit_token(TokenKind::Bool),
                        "max" => self.emit_token(TokenKind::Max),
                        "min" => self.emit_token(TokenKind::Min),
                        "mean" => self.emit_token(TokenKind::Mean),
                        "sum" => self.emit_token(TokenKind::Sum),
                        "if" => self.emit_token(TokenKind::If),
                        "else" => self.emit_token(TokenKind::Else),
                        "for" => self.emit_token(TokenKind::For),
                        "in" => self.emit_token(TokenKind::In),
                        _ => self.emit_token(TokenKind::Ident),
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
                    self.emit_token(TokenKind::Str);
                }
                &c if c.is_whitespace() => self.i += 1,
                &c => bail!(diag(
                    &self.src,
                    "token",
                    c.to_string().as_str(),
                    self.i,
                    None
                )),
            }
        }

        Ok(())
    }
}

pub fn diag(
    src: &Box<[char]>,
    label: &str,
    found: &str,
    pos: usize,
    extra: Option<&str>,
) -> String {
    let pad: String = (0..pos).into_iter().skip(1).map(|_| ' ').collect();
    let src: String = src.into_iter().collect();
    let mut msg = format!(
        "{}\n{} ^ <--- WRONG\ninvalid {} '{}' at position {}",
        src, pad, label, found, pos
    );
    if let Some(v) = extra {
        msg.push_str(format!(", {}", v).as_str());
    }
    msg
}

pub fn lex(src: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(src);
    lexer.lex()?;
    Ok(lexer.tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn cmp_toks(expected: &[TokenKind], actual: &Vec<Token>) -> bool {
        if expected.len() == actual.len() {
            for i in 0..expected.len() {
                let e = expected.get(i).unwrap();
                let a = &actual.get(i).unwrap().kind;
                if e != a {
                    return false;
                }
            }
        } else {
            return false;
        }
        true
    }

    #[test]
    fn invalid_char() {
        let token = lex("`");
        assert!(token.is_err())
    }

    #[test]
    fn invalid_float() {
        let tokens = lex("1.");
        assert!(tokens.is_err())
    }

    #[test]
    fn float() {
        let tokens = lex("1.5").unwrap();
        assert_eq!(TokenKind::Float, tokens.get(0).unwrap().kind)
    }

    #[test]
    fn int_addition() {
        let tokens = lex("1 + 2").unwrap();
        let expected = [TokenKind::Int, TokenKind::Add, TokenKind::Int].as_slice();
        assert!(cmp_toks(expected, &tokens))
    }

    #[test]
    fn string() {
        let tokens = lex("1 + 2 + \"hello world!!!\" + 3 + 4").unwrap();
        let expected = [
            TokenKind::Int,
            TokenKind::Add,
            TokenKind::Int,
            TokenKind::Add,
            TokenKind::Str,
            TokenKind::Add,
            TokenKind::Int,
            TokenKind::Add,
            TokenKind::Int,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &tokens));
    }

    #[test]
    fn rvalue() {
        let tokens = lex("#[1 - 2.5, 0] * 5").unwrap();
        let expected = [
            TokenKind::Pound,
            TokenKind::LeftSquare,
            TokenKind::Int,
            TokenKind::Minus,
            TokenKind::Float,
            TokenKind::Comma,
            TokenKind::Int,
            TokenKind::RightSquare,
            TokenKind::Mult,
            TokenKind::Int,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &tokens));
    }

    #[test]
    fn lvalue() {
        let tokens = lex("[0, 0]").unwrap();
        let expected = [
            TokenKind::LeftSquare,
            TokenKind::Int,
            TokenKind::Comma,
            TokenKind::Int,
            TokenKind::RightSquare,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &tokens));
    }

    #[test]
    fn invalid_token() {
        let tokens = lex("1 + 2 ` 3");
        let expected = indoc! {"
            1 + 2 ` 3
                  ^ <--- WRONG
            invalid token '`' at position 6"};

        match tokens {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(tokens.is_err()),
        }
    }

    #[test]
    fn invalid_digit() {
        let tokens = lex("1 + 1.a");
        let expected = indoc! {"
            1 + 1.a
                  ^ <--- WRONG
            invalid char 'a' at position 6, expected digit"};

        match tokens {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(tokens.is_err()),
        }
    }

    #[test]
    fn invalid_none_digit() {
        let tokens = lex("1 + 1.");
        let expected = indoc! {"
            1 + 1.
                  ^ <--- WRONG
            invalid char 'None' at position 6, expected digit"};

        match tokens {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(tokens.is_err()),
        }
    }

    #[test]
    fn assignment() {
        let tokens = lex("waldo = 1 + 1").unwrap();
        let expected = [
            TokenKind::Ident,
            TokenKind::Equals,
            TokenKind::Int,
            TokenKind::Add,
            TokenKind::Int,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &tokens))
    }

    #[test]
    fn dot_dot() {
        let tokens = lex("..").unwrap();
        let expected = [TokenKind::DotDot].as_slice();
        assert!(cmp_toks(expected, &tokens))
    }
}
