use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use model::{Expr, Runtime, GRID_WIDTH};
use parser::{parse, parse_expr};
use tui::ui;
use tui_textarea::TextArea;

mod lexer;
mod model;
mod parser;
mod tui;

fn main() -> io::Result<()> {
    let mut terminal = tui::init()?;
    let app_result = App::default().run(&mut terminal);
    tui::restore()?;
    app_result
}

#[derive(Debug, Default, PartialEq)]
pub enum Mode {
    #[default]
    Grid,
    Editor,
}

#[derive(Debug, Default)]
pub struct App<'a> {
    exit: bool,
    runtime: Runtime,
    cursor: (usize, usize),
    editor: TextArea<'a>,
    mode: Mode,
}

impl App<'_> {
    /// runs the application's main loop until the user quits
    pub fn run(&mut self, terminal: &mut tui::Tui) -> io::Result<()> {
        while !self.exit {
            terminal.draw(|frame| ui(frame, self))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn handle_events(&mut self) -> io::Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        };
        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Esc => self.mode = Mode::Grid,
            KeyCode::Enter => {
                self.mode = Mode::Grid;
                self.evaluate();
            }
            _ => {}
        }

        if self.mode == Mode::Grid {
            match key_event.code {
                KeyCode::Char('q') => self.exit(),
                KeyCode::Char('h') => self.decrement_cursor_x(),
                KeyCode::Char('j') => self.increment_cursor_y(),
                KeyCode::Char('k') => self.decrement_cursor_y(),
                KeyCode::Char('l') => self.increment_cursor_x(),
                KeyCode::Char('i') => self.mode = Mode::Editor,
                _ => {}
            }
        } else {
            self.editor.input(key_event);
        }
    }

    fn evaluate(&mut self) {
        let text = self.editor.lines()[0].clone();
        let (x, y) = self.cursor;
        let addr = &Expr::AddrPrim(x, y);

        if let Ok(v) = text.parse::<f64>() {
            self.runtime.set_cell(addr, &Expr::FloatPrim(v)).unwrap();
        } else if let Ok(v) = text.parse::<bool>() {
            self.runtime.set_cell(addr, &Expr::BoolPrim(v)).unwrap();
        } else if let Ok(v) = text.parse::<i64>() {
            self.runtime.set_cell(addr, &Expr::IntPrim(v)).unwrap();
        } else {
            if text != "" && text.chars().nth(0).unwrap() == '=' {
                let text: String = text.chars().skip(1).collect();
                let res = parse(&text);
                let eval;
                let ast;

                match res {
                    Ok(v) => {
                        v.execute(self.runtime.cell_mut(addr).unwrap());
                        ast = Some(v.expr);
                        eval = Some(ast.clone().unwrap().eval(&self.runtime, addr));
                    }
                    Err(e) => {
                        ast = None;
                        eval = Some(Err(e));
                    }
                }
                self.runtime.set_cell_ast(addr, ast).unwrap();
                self.runtime.set_cell_eval(addr, eval).unwrap();
            } else {
                self.runtime
                    .set_cell_ast(addr, Some(Expr::StringPrim(text.clone())))
                    .unwrap();
                self.runtime
                    .set_cell_eval(addr, Some(Ok(Expr::StringPrim(text.clone()))))
                    .unwrap();
            }
        }

        self.runtime.set_cell_src(addr, text).unwrap();

        // evaluate all cells
        for row in 0..GRID_WIDTH {
            for col in 0..GRID_WIDTH {
                let addr = &Expr::AddrPrim(row, col);
                let cell = self.runtime.cell_mut(addr).unwrap();
                if let Some(ast) = cell.ast.clone() {
                    let eval = ast.eval(&self.runtime, addr);
                    self.runtime.set_cell_eval(addr, Some(eval)).unwrap();
                }
            }
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }

    fn decrement_cursor_x(&mut self) {
        let (x, y) = self.cursor;
        let new_x = match x {
            0 => 0,
            v => v - 1,
        };
        self.cursor = (new_x, y);
    }

    fn increment_cursor_x(&mut self) {
        let (mut x, y) = self.cursor;
        x += 1;
        if x == GRID_WIDTH {
            x = GRID_WIDTH - 1;
        }
        self.cursor = (x, y)
    }

    fn increment_cursor_y(&mut self) {
        let (x, mut y) = self.cursor;
        y += 1;
        if y == GRID_WIDTH {
            y = GRID_WIDTH - 1;
        }
        self.cursor = (x, y)
    }

    fn decrement_cursor_y(&mut self) {
        let (x, y) = self.cursor;
        let new_y = match y {
            0 => 0,
            v => v - 1,
        };
        self.cursor = (x, new_y);
    }
}
