mod app;
mod exec_graph;
mod interpreter;
mod model;
mod optimizer;
mod parser;
mod random_program;
mod ui;

use crate::parser::parse_program_from_file;
use crate::random_program::{random_program, random_program_with_options, RandomOptions};
use crate::ui::{TerminalUI, UserInterface};
use std::env;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut check_mode = false;
    let mut options = RandomOptions::default();
    let mut paths = Vec::new();
    for arg in &args {
        if arg == "--check" {
            check_mode = true;
        } else if arg == "--no-assume" {
            options.allow_assume = false;
        } else if arg == "--no-assert" {
            options.allow_assert = false;
        } else if arg == "--no-na" {
            options.allow_non_atomic = false;
        } else if arg.starts_with("--") {
            eprintln!("unknown flag: {}", arg);
            exit(2);
        } else {
            paths.push(arg.clone());
        }
    }

    let program = if check_mode {
        if paths.is_empty() {
            eprintln!("usage: mc-play --check <file>...");
            exit(2);
        }
        let mut ok = true;
        for path in paths {
            match parse_program_from_file(&path) {
                Ok(_) => println!("ok {}", path),
                Err(err) => {
                    eprintln!("error {}: {}", path, err);
                    ok = false;
                }
            }
        }
        if !ok {
            exit(1);
        }
        return;
    } else if paths.is_empty() {
        if options == RandomOptions::default() {
            random_program()
        } else {
            random_program_with_options(options)
        }
    } else if paths.len() == 1 {
        match parse_program_from_file(&paths[0]) {
            Ok(program) => program,
            Err(err) => {
                eprintln!("failed to parse {}: {}", paths[0], err);
                exit(1);
            }
        }
    } else {
        eprintln!("usage: mc-play [--no-assume] [--no-assert] [--no-na] | <file>");
        exit(2);
    };
    let mut app = app::AppState::new(program);
    let mut ui = TerminalUI;
    ui.run(&mut app);
}
