use std::{
    io::{self, stdout, Stdout},
    rc::Rc,
};

use crossterm::{execute, terminal::*};
use ratatui::{
    prelude::*,
    symbols::border,
    widgets::{block::Title, Block, Borders, Paragraph},
};

use crate::{
    model::{Expr, GRID_WITH},
    App, Mode,
};

/// A type alias for the terminal type used in this application
pub type Tui = Terminal<CrosstermBackend<Stdout>>;

/// Initialize the terminal
pub fn init() -> io::Result<Tui> {
    execute!(stdout(), EnterAlternateScreen)?;
    enable_raw_mode()?;
    Terminal::new(CrosstermBackend::new(stdout()))
}

/// Restore the terminal to its original state
pub fn restore() -> io::Result<()> {
    execute!(stdout(), LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

pub fn ui(f: &mut Frame, app: &mut App) {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints(vec![
            Constraint::Length(1),
            Constraint::Percentage(10),
            Constraint::Min(1),
        ])
        .split(f.size());

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(vertical[1]);

    render_heading(f, vertical[0]);

    render_editor(f, horizontal[0], app);
    render_display(f, horizontal[1]);

    render_table(f, vertical[2], app);
}

fn render_table(f: &mut Frame, area: Rect, app: &App) {
    let grid_constraints: Vec<Constraint> = (0..GRID_WITH)
        .into_iter()
        .map(|_| Constraint::Min(1))
        .collect();

    let col_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(&grid_constraints)
        .split(area);
    let row_layouts: Vec<Rc<[Rect]>> = (0..GRID_WITH)
        .into_iter()
        .map(|i| {
            Layout::default()
                .direction(Direction::Vertical)
                .constraints(&grid_constraints)
                .split(col_layout[i])
        })
        .collect();

    for row in 0..GRID_WITH {
        for col in 0..GRID_WITH {
            let para = make_cell(row, col, app);
            f.render_widget(para, row_layouts[row][col]);
        }
    }
}

fn make_cell<'a>(x: usize, y: usize, app: &App<'a>) -> Paragraph<'a> {
    let runtime = &app.runtime;
    let cell_data = runtime.cell(&Expr::AddrPrim(x, y)).unwrap();
    let text = match &cell_data.eval {
        Some(v) => match v {
            Ok(v) => v.serialize(),
            Err(e) => e.to_string(),
        },
        None => "".to_string()
    };

    let mut border_color = Color::White;
    if (x, y) == app.cursor {
        border_color = Color::LightRed;
    }
    let block = Block::new().borders(Borders::ALL).set_style(border_color);
    Paragraph::new(text).block(block)
}

fn render_editor(f: &mut Frame, area: Rect, app: &mut App) {
    let mut border_color = Color::White;
    if app.mode == Mode::Editor {
        border_color = Color::LightRed;
    }
    let title = Title::from(" Formula Editor ".bold().light_magenta());
    let block = Block::default()
        .title(title.alignment(Alignment::Center))
        .borders(Borders::ALL)
        .border_set(border::THICK)
        .border_style(border_color);
    app.editor.set_block(block);
    f.render_widget(app.editor.widget(), area);
}

fn render_display(f: &mut Frame, area: Rect) {
    let title = Title::from(" Display ".bold().light_blue());
    let block = Block::default()
        .title(title.alignment(Alignment::Center))
        .borders(Borders::ALL)
        .border_set(border::THICK);
    f.render_widget(block, area);
}

fn render_heading(f: &mut Frame, area: Rect) {
    let title = Title::from(" GridKid ".bold().light_red());
    let block = Block::default()
        .title(title.alignment(Alignment::Center))
        .borders(Borders::TOP)
        .border_set(border::THICK);
    f.render_widget(block, area);
}
