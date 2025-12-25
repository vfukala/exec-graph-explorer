mod app;
mod exec_graph;
mod interpreter;
mod model;
mod optimizer;
mod random_program;
mod ui;

use crate::random_program::random_program;
use crate::ui::{TerminalUI, UserInterface};

fn main() {
    let program = random_program();
    let mut app = app::AppState::new(program);
    let mut ui = TerminalUI;
    ui.run(&mut app);
}
