use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use model::{Runtime, GRID_WITH};
use tui::ui;

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

#[derive(Debug, Default)]
pub struct App {
    exit: bool,
    runtime: Runtime,
    cursor: (usize, usize),
}

impl App {
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
            KeyCode::Char('q') => self.exit(),
            KeyCode::Char('h') => self.decrement_cursor_x(),
            KeyCode::Char('j') => self.increment_cursor_y(),
            KeyCode::Char('k') => self.decrement_cursor_y(),
            KeyCode::Char('l') => self.increment_cursor_x(),
            _ => {}
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }

    fn decrement_cursor_x(&mut self) {
        let (x, y) = self.cursor;
        let new_x = match x {
            0 => 0,
            v => v - 1
        };
        self.cursor = (new_x, y);
    }

    fn increment_cursor_x(&mut self) {
        let (mut x, y) = self.cursor;
        x += 1;
        if x == GRID_WITH {
            x = GRID_WITH - 1;
        }
        self.cursor = (x, y)
    }

    fn increment_cursor_y(&mut self) {
        let (x, mut y) = self.cursor;
        y += 1;
        if y == GRID_WITH {
            y = GRID_WITH - 1;
        }
        self.cursor = (x, y)
    }

    fn decrement_cursor_y(&mut self) {
        let (x, y) = self.cursor;
        let new_y = match y {
            0 => 0,
            v => v - 1
        };
        self.cursor = (x, new_y);
    }
}
