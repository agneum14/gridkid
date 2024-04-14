use std::io::{self, stdout, Stdout};

use crossterm::{execute, terminal::*};
use ratatui::{
    prelude::*,
    symbols::border,
    widgets::{
        block::{Title, *}, Block, Borders, Paragraph, Row, Table
    },
};

use crate::App;

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
            Constraint::Percentage(25),
            Constraint::Min(1),
        ])
        .split(f.size());

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(vertical[1]);

    render_heading(f, vertical[0]);

    render_editor(f, horizontal[0]);
    render_display(f, horizontal[1]);

    render_table(f, vertical[2]);
}

fn render_table(f: &mut Frame, area: Rect) {
    let title = Title::from(" Table ".bold().light_green());
    let block = Block::default()
        .title(title.alignment(Alignment::Center))
        .borders(Borders::ALL)
        .border_set(border::THICK);
    f.render_widget(block, area)
}

fn render_editor(f: &mut Frame, area: Rect) {
    let title = Title::from(" Formula Editor ".bold().light_magenta());
    let block = Block::default()
        .title(title.alignment(Alignment::Center))
        .borders(Borders::ALL)
        .border_set(border::THICK);
    f.render_widget(block, area);
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
