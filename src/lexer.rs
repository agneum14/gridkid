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
    pub start: usize,
}

struct Lexer {
    lexemes: Vec<Lexeme>,
    lexeme_so_far: String,
    i: usize,
    start: usize,
    src: Box<[char]>,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            lexemes: Vec::new(),
            lexeme_so_far: "".to_string(),
            i: 0,
            start: 0,
            src: src.chars().collect(),
        }
    }

    fn get(&self) -> Option<&char> {
        self.src.get(self.i)
    }

    fn emit_lexeme(&mut self, kind: LexemeKind) {
        let lexeme = Lexeme {
            src: self.lexeme_so_far.to_owned(),
            kind,
            start: self.start,
        };
        self.lexemes.push(lexeme);
        self.lexeme_so_far.clear();
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
        self.lexeme_so_far.push(*c);
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

    fn cap_emit(&mut self, kind: LexemeKind) {
        self.cap();
        self.emit_lexeme(kind);
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
                        _ => self.emit_lexeme(LexemeKind::BitOr),
                    }
                }
                '&' => {
                    self.cap();
                    match self.get() {
                        Some('&') => self.cap_emit(LexemeKind::LogicAnd),
                        _ => self.emit_lexeme(LexemeKind::BitAnd),
                    }
                }
                '=' => {
                    self.cap();
                    self.cap_targ('=')?;
                    self.emit_lexeme(LexemeKind::EqualTo);
                }
                '!' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::NotEqualTo),
                        _ => self.emit_lexeme(LexemeKind::LogicNot),
                    }
                }
                '<' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::LessThanOrEqualTo),
                        Some('<') => self.cap_emit(LexemeKind::BitshiftLeft),
                        _ => self.emit_lexeme(LexemeKind::LessThan),
                    }
                }
                '>' => {
                    self.cap();
                    match self.get() {
                        Some('=') => self.cap_emit(LexemeKind::GreaterThanOrEqualTo),
                        Some('>') => self.cap_emit(LexemeKind::BitshiftRight),
                        _ => self.emit_lexeme(LexemeKind::GreaterThan),
                    }
                }
                '*' => {
                    self.cap();
                    match self.get() {
                        Some('*') => self.cap_emit(LexemeKind::Exp),
                        _ => self.emit_lexeme(LexemeKind::Mult),
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
                        self.emit_lexeme(LexemeKind::Float);
                    } else {
                        self.emit_lexeme(LexemeKind::Int);
                    }
                }
                _ if self.is_alphabetic() => {
                    self.cap();
                    while self.is_alphabetic() {
                        self.cap();
                    }

                    match self.lexeme_so_far.as_str() {
                        "float" => self.emit_lexeme(LexemeKind::FloatCast),
                        "int" => self.emit_lexeme(LexemeKind::IntCast),
                        "false" | "true" => self.emit_lexeme(LexemeKind::Bool),
                        _ => bail!(diag(
                            &self.src,
                            "identifier",
                            &self.lexeme_so_far,
                            self.start,
                            None
                        )),
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
                    self.emit_lexeme(LexemeKind::Str);
                }
                &c if c.is_whitespace() => self.i += 1,
                &c => bail!(diag(
                    &self.src,
                    "lexeme",
                    c.to_string().as_str(),
                    self.i,
                    None
                )),
            }
        }

        Ok(())
    }
}

pub fn diag(src: &Box<[char]>, label: &str, found: &str, pos: usize, extra: Option<&str>) -> String {
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

pub fn lex(src: &str) -> Result<Vec<Lexeme>> {
    let mut lexer = Lexer::new(src);
    lexer.lex()?;
    Ok(lexer.lexemes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

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
        let lexeme = lex("`");
        assert!(lexeme.is_err())
    }

    #[test]
    fn invalid_float() {
        let lexemes = lex("1.");
        assert!(lexemes.is_err())
    }

    #[test]
    fn float() {
        let lexemes = lex("1.5").unwrap();
        assert_eq!(LexemeKind::Float, lexemes.get(0).unwrap().kind)
    }

    #[test]
    fn int_addition() {
        let lexemes = lex("1 + 2").unwrap();
        let expected = [LexemeKind::Int, LexemeKind::Add, LexemeKind::Int].as_slice();
        assert!(cmp_toks(expected, &lexemes))
    }

    #[test]
    fn string() {
        let lexemes = lex("1 + 2 + \"hello world!!!\" + 3 + 4").unwrap();
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
        assert!(cmp_toks(expected, &lexemes));
    }

    #[test]
    fn rvalue() {
        let lexemes = lex("#[1 - 2.5, 0] * 5").unwrap();
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
        ]
        .as_slice();
        assert!(cmp_toks(expected, &lexemes));
    }

    #[test]
    fn lvalue() {
        let lexemes = lex("[0, 0]").unwrap();
        let expected = [
            LexemeKind::LeftSquare,
            LexemeKind::Int,
            LexemeKind::Comma,
            LexemeKind::Int,
            LexemeKind::RightSquare,
        ]
        .as_slice();
        assert!(cmp_toks(expected, &lexemes));
    }

    #[test]
    fn invalid_lexeme() {
        let lexemes = lex("1 + 2 ` 3");
        let expected = indoc! {"
            1 + 2 ` 3
                  ^ <--- WRONG
            invalid lexeme '`' at position 6"};

        match lexemes {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(lexemes.is_err()),
        }
    }

    #[test]
    fn invalid_identifier() {
        let lexemes = lex("1 + 2 + WillyWonka + 3");
        let expected = indoc! {"
            1 + 2 + WillyWonka + 3
                    ^ <--- WRONG
            invalid identifier 'WillyWonka' at position 8"};

        match lexemes {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(lexemes.is_err()),
        }
    }

    #[test]
    fn invalid_digit() {
        let lexemes = lex("1 + 1.a");
        let expected = indoc! {"
            1 + 1.a
                  ^ <--- WRONG
            invalid char 'a' at position 6, expected digit"};

        match lexemes {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(lexemes.is_err()),
        }
    }

    #[test]
    fn invalid_none_digit() {
        let lexemes = lex("1 + 1.");
        let expected = indoc! {"
            1 + 1.
                  ^ <--- WRONG
            invalid char 'None' at position 6, expected digit"};

        match lexemes {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(lexemes.is_err()),
        }
    }

    #[test]
    fn invalid_targ() {
        let lexemes = lex("1 + 1 =a 2");
        let expected = indoc! {"
            1 + 1 =a 2
                   ^ <--- WRONG
            invalid char 'a' at position 7, expected '='"};

        match lexemes {
            Err(e) => assert_eq!(expected, e.to_string()),
            Ok(_) => assert!(lexemes.is_err()),
        }
    }
}
