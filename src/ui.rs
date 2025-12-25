use crate::app::{format_editable_graph, format_graph, format_program, AppState};
use crate::exec_graph::{EditableExecutionGraph, ExecutionGraph};
use crate::interpreter::{next_actions, NextAction};
use crate::model::Program;
use std::io::{self, Write};

pub trait UserInterface {
    fn run(&mut self, app: &mut AppState);
}

pub struct TerminalUI;

impl UserInterface for TerminalUI {
    fn run(&mut self, app: &mut AppState) {
        println!("mc-play interactive explorer");
        println!("Type 'help' for commands.");
        loop {
            print!("> ");
            let _ = io::stdout().flush();
            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() {
                break;
            }
            let line = input.trim();
            if line.is_empty() {
                continue;
            }
            let mut parts = line.split_whitespace();
            let cmd = parts.next().unwrap_or("");
            match cmd {
                "help" => print_help(),
                "quit" | "exit" => break,
                "program" => println!("{}", format_program(&app.program)),
                "tree" => print_tree(app),
                "select" => {
                    if let Some(id) = parts.next().and_then(|v| v.parse::<usize>().ok()) {
                        if !is_alive(app, id) {
                            println!("node {} is not available", id);
                        } else {
                            app.current_leaf = id;
                            if is_leaf(app, id) {
                                println!("selected leaf {}", id);
                            } else {
                                println!("selected node {} (not a leaf)", id);
                            }
                        }
                    } else {
                        println!("usage: select <node_id>");
                    }
                }
                "graph" => {
                    if let Some(node) = app.nodes.get(app.current_leaf) {
                        println!("{}", format_graph(&node.graph));
                    }
                }
                "actions" => print_actions(app),
                "schedule" => {
                    if let Some(thread) = parts.next().and_then(|v| v.parse::<usize>().ok()) {
                        schedule_with_user_choices(app, thread);
                    } else {
                        println!("usage: schedule <thread_id>");
                    }
                }
                _ => println!("unknown command: {}", cmd),
            }
        }
    }
}

fn print_help() {
    println!("commands:");
    println!("  program          show program");
    println!("  tree             show execution graph tree");
    println!("  select <id>      select a leaf node");
    println!("  graph            show current leaf graph");
    println!("  actions          show next actions per thread");
    println!("  schedule <tid>   add children for thread");
    println!("  quit             exit");
}

fn print_tree(app: &AppState) {
    for (id, node) in app.nodes.iter().enumerate() {
        if !node.alive {
            continue;
        }
        let depth = depth(app, id);
        let indent = "  ".repeat(depth);
        let marker = if id == app.current_leaf { "*" } else { " " };
        let children = app.alive_children(id);
        let actions = node
            .next_actions
            .iter()
            .enumerate()
            .map(|(tid, action)| format!("t{}: {}", tid, format_action(action)))
            .collect::<Vec<_>>()
            .join(", ");
        println!(
            "{}{}node {} children={:?} scheduled={:?} actions=[{}]",
            indent, marker, id, children, node.scheduled_thread, actions
        );
    }
}

fn depth(app: &AppState, mut id: usize) -> usize {
    let mut depth = 0;
    while let Some(parent) = app.nodes.get(id).and_then(|n| n.parent) {
        depth += 1;
        id = parent;
    }
    depth
}

fn is_leaf(app: &AppState, id: usize) -> bool {
    app.nodes.get(id).is_some_and(|n| n.alive && app.alive_children(id).is_empty())
}

fn is_alive(app: &AppState, id: usize) -> bool {
    app.nodes.get(id).is_some_and(|n| n.alive)
}

fn print_actions(app: &AppState) {
    if let Some(node) = app.nodes.get(app.current_leaf) {
        for (tid, action) in node.next_actions.iter().enumerate() {
            println!("t{}: {}", tid, format_action(action));
        }
    }
}

fn format_action(action: &NextAction) -> String {
    match action {
        NextAction::Read { location, order } => format!("read s{} ({})", location, format_order(*order)),
        NextAction::Write {
            location,
            value,
            order,
        } => format!("write s{}={} ({})", location, value, format_order(*order)),
        NextAction::AssertionViolation => "assertion violation".to_string(),
        NextAction::Blocked => "blocked".to_string(),
        NextAction::Finished => "finished".to_string(),
        NextAction::Inconsistent(msg) => format!("inconsistent: {}", msg),
    }
}

fn format_order(order: crate::model::MemoryOrder) -> &'static str {
    match order {
        crate::model::MemoryOrder::RelaxedAtomic => "RA",
        crate::model::MemoryOrder::NonAtomic => "NA",
    }
}

fn schedule_with_user_choices(app: &mut AppState, thread_id: usize) {
    let current = app.current_leaf;
    if !app.nodes.get(current).is_some_and(|n| n.alive) {
        println!("current node is not available");
        return;
    }
    let existing_children = app.alive_children(current);
    if !existing_children.is_empty() {
        println!("node {} already scheduled. reschedule and prune subtree? (y/n)", current);
        let choice = read_line();
        if !choice.eq_ignore_ascii_case("y") {
            println!("schedule canceled");
            return;
        }
        app.prune_children(current);
    }
    if let Some(node) = app.nodes.get(current) {
        println!("{}", format_graph(&node.graph));
    }
    let Some(action) = app
        .nodes
        .get(current)
        .and_then(|n| n.next_actions.get(thread_id).cloned())
    else {
        println!("no action for thread {}", thread_id);
        return;
    };

    match action {
        NextAction::Read { location, order } => {
            let base_graph = match app.nodes.get(current) {
                Some(node) => node.graph.clone(),
                None => {
                    println!("current node is not available");
                    return;
                }
            };
            let mut children = Vec::new();
            loop {
                let mut editable = EditableExecutionGraph::from_complete(&base_graph);
                editable.add_read_unset(thread_id, location, order);
                if let Some(graph) = edit_child_graph(&app.program, editable) {
                    if let Some(child) = app.add_child_from_graph(current, thread_id, graph) {
                        children.push(child);
                    }
                }
                if !ask_to_continue("add another child? (y/n)") {
                    break;
                }
            }
            print_children_result(&children);
        }
        NextAction::Write {
            location,
            value,
            order,
        } => {
            let base_graph = match app.nodes.get(current) {
                Some(node) => node.graph.clone(),
                None => {
                    println!("current node is not available");
                    return;
                }
            };
            let mut children = Vec::new();
            loop {
                let mut editable = EditableExecutionGraph::from_complete(&base_graph);
                editable.add_write_unset(thread_id, location, value, order);
                if let Some(graph) = edit_child_graph(&app.program, editable) {
                    if let Some(child) = app.add_child_from_graph(current, thread_id, graph) {
                        children.push(child);
                    }
                }
                if !ask_to_continue("add another child? (y/n)") {
                    break;
                }
            }
            print_children_result(&children);
        }
        other => {
            println!("thread {}: {}", thread_id, format_action(&other));
            println!("no scheduling needed");
        }
    }
}

fn read_line() -> String {
    let mut input = String::new();
    let _ = io::stdout().flush();
    if io::stdin().read_line(&mut input).is_err() {
        return String::new();
    }
    input.trim().to_string()
}

fn ask_to_continue(prompt: &str) -> bool {
    println!("{}", prompt);
    read_line().eq_ignore_ascii_case("y")
}

fn print_children_result(children: &[usize]) {
    if children.is_empty() {
        println!("no children generated");
    } else {
        println!("new children: {:?}", children);
    }
}

fn edit_child_graph(program: &Program, mut graph: EditableExecutionGraph) -> Option<ExecutionGraph> {
    println!("editing child graph");
    println!("type 'help' for edit commands");
    println!("{}", format_editable_graph(&graph));
    loop {
        print!("edit> ");
        let _ = io::stdout().flush();
        let input = read_line();
        if input.is_empty() {
            continue;
        }
        let mut parts = input.split_whitespace();
        let cmd = parts.next().unwrap_or("");
        match cmd {
            "help" => {
                println!("edit commands:");
                println!("  show                         show graph");
                println!("  actions                      show next actions");
                println!("  set-rf <read> <write>         update rf edge");
                println!("  del <event>                   delete event");
                println!("  set-co <write> <index>        place/move write in co (index >= 1)");
                println!("  done                          keep child");
                println!("  discard                       drop child");
            }
            "show" => {
                println!("{}", format_editable_graph(&graph));
            }
            "actions" => {
                let errors = graph.validate();
                if !errors.is_empty() {
                    println!("graph invalid:");
                    for err in errors {
                        println!("  {}", err);
                    }
                } else if let Some(complete) = graph.clone().into_complete() {
                    let actions = next_actions(program, &complete);
                    for (tid, action) in actions.iter().enumerate() {
                        println!("t{}: {}", tid, format_action(action));
                    }
                } else {
                    println!("graph invalid: unable to materialize execution graph");
                }
            }
            "set-rf" => {
                let read_id = parts.next().and_then(|v| v.parse::<usize>().ok());
                let rf_id = parts.next().and_then(|v| v.parse::<usize>().ok());
                match (read_id, rf_id) {
                    (Some(read_id), Some(rf_id)) => {
                        if graph.set_read_rf(read_id, rf_id) {
                            println!("updated rf for e{}", read_id);
                        } else {
                            println!("failed to update rf for e{}", read_id);
                        }
                    }
                    _ => println!("usage: set-rf <read> <write>"),
                }
            }
            "del" => {
                let event_id = parts.next().and_then(|v| v.parse::<usize>().ok());
                if let Some(event_id) = event_id {
                    if graph.remove_event(event_id) {
                        println!("deleted e{}", event_id);
                    } else {
                        println!("failed to delete e{}", event_id);
                    }
                } else {
                    println!("usage: del <event>");
                }
            }
            "set-co" => {
                let write_id = parts.next().and_then(|v| v.parse::<usize>().ok());
                let index = parts.next().and_then(|v| v.parse::<usize>().ok());
                match (write_id, index) {
                    (Some(write_id), Some(index)) => {
                        if graph.set_write_co(write_id, index) {
                            println!("set co for e{} to index {}", write_id, index);
                        } else {
                            println!("failed to set co for e{}", write_id);
                        }
                    }
                    _ => println!("usage: set-co <write> <index>"),
                }
            }
            "done" => {
                let errors = graph.validate();
                if errors.is_empty() {
                    return graph.into_complete();
                }
                println!("graph invalid:");
                for err in errors {
                    println!("  {}", err);
                }
            }
            "discard" => {
                return None;
            }
            _ => println!("unknown edit command: {}", cmd),
        }
    }
}
