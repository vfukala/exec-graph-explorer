use crate::exec_graph::{EventKind, ExecutionGraph};
use crate::interpreter::{next_actions, NextAction};
use crate::model::{MemoryOrder, Program};

#[derive(Clone, Debug)]
pub struct GraphNode {
    pub graph: ExecutionGraph,
    pub children: Vec<usize>,
    pub parent: Option<usize>,
    pub scheduled_thread: Option<usize>,
    pub next_actions: Vec<NextAction>,
    pub alive: bool,
}

#[derive(Clone, Debug)]
pub struct AppState {
    pub program: Program,
    pub nodes: Vec<GraphNode>,
    pub current_leaf: usize,
}

impl AppState {
    pub fn new(program: Program) -> Self {
        let graph = ExecutionGraph::new(&program);
        let next_actions = next_actions(&program, &graph);
        let root = GraphNode {
            graph,
            children: Vec::new(),
            parent: None,
            scheduled_thread: None,
            next_actions,
            alive: true,
        };
        Self {
            program,
            nodes: vec![root],
            current_leaf: 0,
        }
    }

    pub fn recompute_node_actions(&mut self, node_id: usize) {
        if let Some(node) = self.nodes.get_mut(node_id) {
            node.next_actions = next_actions(&self.program, &node.graph);
        }
    }

    pub fn add_children_for_thread(&mut self, node_id: usize, thread_id: usize) -> Vec<usize> {
        let Some(node) = self.nodes.get(node_id) else {
            return Vec::new();
        };
        if !node.alive {
            return Vec::new();
        }
        let base_graph = node.graph.clone();
        let action = node.next_actions.get(thread_id).cloned();
        let Some(action) = action else {
            return Vec::new();
        };

        let mut new_ids = Vec::new();
        match action {
            NextAction::Read { location, order } => {
                let candidates = base_graph.writes_for_location(location);
                for rf in candidates {
                    let mut graph = base_graph.clone();
                    graph.add_read(thread_id, location, rf, order);
                    let next_actions = next_actions(&self.program, &graph);
                    let child = GraphNode {
                        graph,
                        children: Vec::new(),
                        parent: Some(node_id),
                        scheduled_thread: Some(thread_id),
                        next_actions,
                        alive: true,
                    };
                    let child_id = self.nodes.len();
                    self.nodes.push(child);
                    new_ids.push(child_id);
                }
            }
            NextAction::Write {
                location,
                value,
                order,
            } => {
                let mut graph = base_graph.clone();
                graph.add_write(thread_id, location, value, order);
                let next_actions = next_actions(&self.program, &graph);
                let child = GraphNode {
                    graph,
                    children: Vec::new(),
                    parent: Some(node_id),
                    scheduled_thread: Some(thread_id),
                    next_actions,
                    alive: true,
                };
                let child_id = self.nodes.len();
                self.nodes.push(child);
                new_ids.push(child_id);
            }
            _ => {}
        }

        if let Some(node) = self.nodes.get_mut(node_id) {
            node.children.extend(&new_ids);
            if !new_ids.is_empty() {
                node.scheduled_thread = Some(thread_id);
            }
        }
        new_ids
    }

    pub fn add_child_read_with_rf(
        &mut self,
        node_id: usize,
        thread_id: usize,
        location: usize,
        rf: usize,
        order: MemoryOrder,
    ) -> Option<usize> {
        let node = self.nodes.get(node_id)?;
        if !node.alive {
            return None;
        }
        let base_graph = node.graph.clone();
        let mut graph = base_graph.clone();
        graph.add_read(thread_id, location, rf, order);
        let next_actions = next_actions(&self.program, &graph);
        let child = GraphNode {
            graph,
            children: Vec::new(),
            parent: Some(node_id),
            scheduled_thread: Some(thread_id),
            next_actions,
            alive: true,
        };
        let child_id = self.nodes.len();
        self.nodes.push(child);
        if let Some(node) = self.nodes.get_mut(node_id) {
            node.children.push(child_id);
            node.scheduled_thread = Some(thread_id);
        }
        Some(child_id)
    }

    pub fn add_child_write_with_co_index(
        &mut self,
        node_id: usize,
        thread_id: usize,
        location: usize,
        value: i64,
        order: MemoryOrder,
        co_index: usize,
    ) -> Option<usize> {
        let node = self.nodes.get(node_id)?;
        if !node.alive {
            return None;
        }
        let base_graph = node.graph.clone();
        let mut graph = base_graph.clone();
        graph.add_write_at(thread_id, location, value, order, co_index)?;
        let next_actions = next_actions(&self.program, &graph);
        let child = GraphNode {
            graph,
            children: Vec::new(),
            parent: Some(node_id),
            scheduled_thread: Some(thread_id),
            next_actions,
            alive: true,
        };
        let child_id = self.nodes.len();
        self.nodes.push(child);
        if let Some(node) = self.nodes.get_mut(node_id) {
            node.children.push(child_id);
            node.scheduled_thread = Some(thread_id);
        }
        Some(child_id)
    }

    pub fn remove_last_event(&mut self, node_id: usize, thread_id: usize) -> bool {
        let Some(node) = self.nodes.get_mut(node_id) else {
            return false;
        };
        if !node.alive {
            return false;
        }
        let removed = node.graph.remove_last_event(thread_id).is_some();
        if removed {
            node.next_actions = next_actions(&self.program, &node.graph);
        }
        removed
    }

    pub fn prune_children(&mut self, node_id: usize) -> bool {
        let Some(node) = self.nodes.get_mut(node_id) else {
            return false;
        };
        if !node.alive {
            return false;
        }
        let children = node.children.clone();
        node.children.clear();
        node.scheduled_thread = None;
        for child in children {
            self.mark_dead_recursive(child);
        }
        true
    }

    pub fn remove_node(&mut self, node_id: usize) -> bool {
        let Some(node) = self.nodes.get(node_id) else {
            return false;
        };
        if !node.alive {
            return false;
        }
        let parent = node.parent;
        self.mark_dead_recursive(node_id);
        if let Some(parent_id) = parent {
            if let Some(parent_node) = self.nodes.get_mut(parent_id) {
                parent_node.children.retain(|&id| id != node_id);
            }
        }
        true
    }

    fn mark_dead_recursive(&mut self, node_id: usize) {
        let Some(node) = self.nodes.get_mut(node_id) else {
            return;
        };
        if !node.alive {
            return;
        }
        node.alive = false;
        let children = node.children.clone();
        node.children.clear();
        node.parent = None;
        for child in children {
            self.mark_dead_recursive(child);
        }
    }

    pub fn alive_children(&self, node_id: usize) -> Vec<usize> {
        self.nodes
            .get(node_id)
            .map(|n| {
                n.children
                    .iter()
                    .copied()
                    .filter(|id| self.nodes.get(*id).is_some_and(|c| c.alive))
                    .collect()
            })
            .unwrap_or_default()
    }
}

pub fn format_program(program: &Program) -> String {
    let mut out = String::new();
    out.push_str("Shared:\n");
    for (idx, var) in program.shared.iter().enumerate() {
        out.push_str(&format!("  s{}: {} = {}\n", idx, var.name, var.init));
    }
    out.push_str("Threads:\n");
    for (idx, thread) in program.threads.iter().enumerate() {
        out.push_str(&format!("  t{} locals: {:?}\n", idx, thread.locals));
        out.push_str(&format!("  t{} stmt: {}\n", idx, format_stmt(&thread.stmt)));
    }
    out
}

pub fn format_graph(graph: &ExecutionGraph) -> String {
    let mut out = String::new();
    out.push_str("Events:\n");
    for (id, event) in graph.events.iter().enumerate() {
        if !event.alive {
            continue;
        }
        let line = match &event.kind {
            EventKind::Init => format!("  e{}: init\n", id),
            EventKind::Read {
                location,
                rf,
                order,
            } => format!(
                "  e{}: read s{} rf=e{} ({})\n",
                id,
                location,
                rf,
                format_order(*order)
            ),
            EventKind::Write {
                location,
                value,
                order,
            } => format!(
                "  e{}: write s{}={} ({})\n",
                id,
                location,
                value,
                format_order(*order)
            ),
        };
        out.push_str(&line);
    }
    out.push_str("Per-thread order:\n");
    for (tid, events) in graph.thread_events.iter().enumerate() {
        out.push_str(&format!("  t{}: {:?}\n", tid, events));
    }
    out.push_str("Coherence:\n");
    for (loc, events) in graph.co.iter().enumerate() {
        out.push_str(&format!("  s{}: {:?}\n", loc, events));
    }
    out
}

fn format_order(order: MemoryOrder) -> &'static str {
    match order {
        MemoryOrder::RelaxedAtomic => "RA",
        MemoryOrder::NonAtomic => "NA",
    }
}

fn format_stmt(stmt: &crate::model::Stmt) -> String {
    use crate::model::{Assign, Stmt};
    match stmt {
        Stmt::NoOp => "nop".to_string(),
        Stmt::Assert(expr) => format!("assert({})", format_expr(expr)),
        Stmt::Assume(expr) => format!("assume({})", format_expr(expr)),
        Stmt::Assign(assign) => match assign {
            Assign::Local { dst, value } => format!("l{} = {}", dst, format_expr(value)),
            Assign::Load {
                location,
                dst,
                order,
            } => format!("l{} = load s{} ({})", dst, location, format_order(*order)),
            Assign::Store {
                location,
                value,
                order,
            } => format!("store s{} = {} ({})", location, format_expr(value), format_order(*order)),
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => format!(
            "if {} then {} else {}",
            format_expr(cond),
            format_stmt(then_branch),
            format_stmt(else_branch)
        ),
        Stmt::While { cond, body } => format!("while {} do {}", format_expr(cond), format_stmt(body)),
        Stmt::Seq { first, second } => format!("({}; {})", format_stmt(first), format_stmt(second)),
    }
}

fn format_expr(expr: &crate::model::Expr) -> String {
    use crate::model::{BinOp, Expr};
    match expr {
        Expr::Local(id) => format!("l{}", id),
        Expr::Const(val) => format!("{}", val),
        Expr::BinOp { op, left, right } => {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
            };
            format!("({} {} {})", format_expr(left), op_str, format_expr(right))
        }
    }
}
