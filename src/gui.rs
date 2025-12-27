use crate::app::AppState;
use crate::exec_graph::{
    EditableEventKind, EditableExecutionGraph, ExecutionGraph, EventKind,
};
use crate::interpreter::NextAction;
use crate::model::MemoryOrder;
use crate::ui::UserInterface;
use eframe::egui;
use std::collections::HashMap;

pub struct GuiUI;

impl UserInterface for GuiUI {
    fn run(&mut self, app: &mut AppState) {
        let app_state = std::mem::replace(app, AppState::new(app.program.clone()));
        let options = eframe::NativeOptions::default();
        let _ = eframe::run_native(
            "mc-play",
            options,
            Box::new(|_cc| Box::new(GuiApp::new(app_state))),
        );
    }
}

struct GuiApp {
    app: AppState,
    ui_state: UiState,
}

struct UiState {
    selected_node: usize,
    selected_child_preview: Option<usize>,
    selected_action_thread: Option<usize>,
    mode: Mode,
    status: Option<String>,
    confirm_dialog: Option<ConfirmDialog>,
}

enum Mode {
    View,
    Edit(EditState),
}

enum ConfirmDialog {
    ResetScheduling { node_id: usize },
    DeclareDataRace { node_id: usize },
}

struct EditState {
    parent_id: usize,
    thread_id: usize,
    graph: EditableExecutionGraph,
    selected_event: Option<usize>,
    commands: Vec<String>,
}

struct GraphColors {
    po: egui::Color32,
    rf: egui::Color32,
    co: egui::Color32,
}

struct Obstacle {
    id: usize,
    center: egui::Pos2,
    radius: f32,
}

const NODE_SIZE: egui::Vec2 = egui::vec2(90.0, 44.0);

impl GuiApp {
    fn new(app: AppState) -> Self {
        let selected_node = app.current_leaf;
        Self {
            app,
            ui_state: UiState {
                selected_node,
                selected_child_preview: None,
                selected_action_thread: None,
                mode: Mode::View,
                status: None,
                confirm_dialog: None,
            },
        }
    }
}

impl eframe::App for GuiApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let colors = GraphColors {
            po: egui::Color32::from_gray(90),
            rf: egui::Color32::from_rgb(0, 170, 0),
            co: egui::Color32::from_rgb(230, 140, 0),
        };
        let mut pending_mode: Option<Mode> = None;
        let mut pending_status: Option<Option<String>> = None;
        let mut pending_selected_node: Option<usize> = None;
        let mut pending_current_leaf: Option<usize> = None;
        let mut pending_selected_action_thread: Option<Option<usize>> = None;
        let mut pending_confirm_dialog: Option<Option<ConfirmDialog>> = None;
        let mut pending_child_preview: Option<Option<usize>> = None;
        let mut pending_data_race: Option<(usize, bool)> = None;

        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("mc-play GUI");
                ui.separator();
                ui.label(format!("selected node: {}", self.ui_state.selected_node));
                if let Mode::Edit(edit) = &self.ui_state.mode {
                    ui.separator();
                    ui.label(format!(
                        "editing child for node {} (t{})",
                        edit.parent_id, edit.thread_id
                    ));
                }
            });
            if let Some(status) = &self.ui_state.status {
                ui.label(status);
            }
        });

        egui::SidePanel::left("tree_panel")
            .resizable(true)
            .show(ctx, |ui| {
                ui.heading("Graph Tree");
                ui.separator();
                let editing = matches!(self.ui_state.mode, Mode::Edit(_));
                draw_tree(ui, &self.app, 0, 0, &mut self.ui_state, editing);
            });

        egui::SidePanel::right("control_panel")
            .resizable(true)
            .show(ctx, |ui| {
                ui.heading("Controls");
                ui.separator();

                if let Some(node) = self.app.nodes.get(self.ui_state.selected_node) {
                    if node.data_race {
                        ui.label("data-race terminal node");
                        ui.separator();
                    }
                }

                match &mut self.ui_state.mode {
                    Mode::View => {
                        ui.label("Actions");
                        ui.separator();
                        show_actions(ui, &mut self.app, &mut self.ui_state);
                    }
                    Mode::Edit(edit) => {
                        ui.label("Editing");
                        ui.separator();
                        let mut commit = false;
                        let mut discard = false;
                        ui.horizontal(|ui| {
                            if ui.button("Commit child").clicked() {
                                commit = true;
                            }
                            if ui.button("Discard").clicked() {
                                discard = true;
                            }
                        });
                        if commit {
                            let errors = edit.graph.validate();
                            if errors.is_empty() {
                                if let Some(graph) = edit.graph.clone().into_complete() {
                                    if self.app
                                        .add_child_from_graph(
                                            edit.parent_id,
                                            edit.thread_id,
                                            graph,
                                            edit.commands.clone(),
                                        )
                                        .is_some()
                                    {
                                        pending_selected_action_thread = Some(None);
                                        pending_child_preview = Some(None);
                                    }
                                    pending_mode = Some(Mode::View);
                                    pending_status = Some(None);
                                } else {
                                    pending_status =
                                        Some(Some("graph invalid: unable to materialize".to_string()));
                                }
                            } else {
                                pending_status =
                                    Some(Some(format!("graph invalid: {}", errors.join("; "))));
                            }
                        }
                        if discard {
                            pending_mode = Some(Mode::View);
                            pending_status = Some(None);
                        }

                        ui.separator();
                        show_edit_controls(ui, edit);
                    }
                }

                ui.separator();
                ui.label("Existing children");
                ui.separator();
                show_children_list(ui, &self.app, &mut self.ui_state);

                if let Some(child_id) = self.ui_state.selected_child_preview {
                    if let Some(node) = self.app.nodes.get(child_id) {
                        ui.separator();
                        egui::CollapsingHeader::new(format!("Child {}", child_id))
                            .default_open(true)
                            .show(ui, |ui| {
                                draw_execution_graph(
                                    ui,
                                    &node.graph,
                                    None,
                                    &colors,
                                    "child_preview_graph",
                                );
                            });
                    }
                }
            });

        egui::CentralPanel::default().show(ctx, |ui| match &mut self.ui_state.mode {
            Mode::View => {
                if let Some(node) = self.app.nodes.get(self.ui_state.selected_node) {
                    draw_execution_graph(
                        ui,
                        &node.graph,
                        None,
                        &colors,
                        "main_graph",
                    );
                }
            }
            Mode::Edit(edit) => {
                if ctx.input(|i| i.key_pressed(egui::Key::Delete)) {
                    if let Some(selected) = edit.selected_event {
                        if edit.graph.remove_event(selected) {
                            edit.commands.push(format!("del {}", selected));
                            edit.selected_event = None;
                        }
                    }
                }
                let clicked = draw_editable_graph(
                    ui,
                    &edit.graph,
                    edit.selected_event,
                    &colors,
                    "edit_graph",
                );
                if let Some(clicked_id) = clicked {
                    handle_edit_click(edit, clicked_id);
                }
            }
        });

        if let Some(dialog) = self.ui_state.confirm_dialog.as_ref() {
            let mut confirm = false;
            let mut cancel = false;
            egui::Window::new("Confirm")
                .collapsible(false)
                .resizable(false)
                .anchor(egui::Align2::CENTER_CENTER, egui::vec2(0.0, 0.0))
                .show(ctx, |ui| {
                    match dialog {
                        ConfirmDialog::ResetScheduling { .. } => {
                            ui.label("Reset scheduling and delete all children for this node?");
                        }
                        ConfirmDialog::DeclareDataRace { .. } => {
                            ui.label("Declare data race and delete all children for this node?");
                        }
                    }
                    ui.separator();
                    ui.horizontal(|ui| {
                        if ui.button("Confirm").clicked() {
                            confirm = true;
                        }
                        if ui.button("Cancel").clicked() {
                            cancel = true;
                        }
                    });
                });
            if confirm {
                match dialog {
                    ConfirmDialog::ResetScheduling { node_id } => {
                        self.app.prune_children(*node_id);
                        pending_selected_action_thread = Some(None);
                        pending_child_preview = Some(None);
                        pending_data_race = Some((*node_id, false));
                        if let Some(node) = self.app.nodes.get_mut(*node_id) {
                            node.children_complete = false;
                        }
                    }
                    ConfirmDialog::DeclareDataRace { node_id } => {
                        self.app.prune_children(*node_id);
                        pending_selected_action_thread = Some(None);
                        pending_child_preview = Some(None);
                        pending_data_race = Some((*node_id, true));
                        if let Some(node) = self.app.nodes.get_mut(*node_id) {
                            node.children_complete = false;
                        }
                    }
                }
                pending_confirm_dialog = Some(None);
            }
            if cancel {
                pending_confirm_dialog = Some(None);
            }
        }

        if let Some(mode) = pending_mode {
            self.ui_state.mode = mode;
        }
        if let Some(status) = pending_status {
            self.ui_state.status = status;
        }
        if let Some(node_id) = pending_selected_node {
            self.ui_state.selected_node = node_id;
        }
        if let Some(node_id) = pending_current_leaf {
            self.app.current_leaf = node_id;
        }
        if let Some(thread) = pending_selected_action_thread {
            self.ui_state.selected_action_thread = thread;
        }
        if let Some(dialog) = pending_confirm_dialog {
            self.ui_state.confirm_dialog = dialog;
        }
        if let Some(preview) = pending_child_preview {
            self.ui_state.selected_child_preview = preview;
        }
        if let Some((node_id, value)) = pending_data_race {
            if let Some(node) = self.app.nodes.get_mut(node_id) {
                node.data_race = value;
            }
        }
    }
}

fn draw_tree(
    ui: &mut egui::Ui,
    app: &AppState,
    id: usize,
    depth: usize,
    state: &mut UiState,
    editing: bool,
) {
    let Some(node) = app.nodes.get(id) else {
        return;
    };
    if !node.alive {
        return;
    }
    ui.horizontal(|ui| {
        if depth > 0 {
            ui.add_space(depth as f32 * 12.0);
        }
        let label = format!("node {}", id);
        let text = if node.children_complete {
            egui::RichText::new(label).color(egui::Color32::from_gray(120))
        } else {
            egui::RichText::new(label)
        };
        if ui
            .add_enabled(
                !editing,
                egui::SelectableLabel::new(state.selected_node == id, text),
            )
            .clicked()
        {
            state.selected_node = id;
            state.selected_child_preview = None;
            state.selected_action_thread = node.scheduled_thread;
        }
    });
    for child in app.alive_children(id) {
        draw_tree(ui, app, child, depth + 1, state, editing);
    }
}

fn show_actions(ui: &mut egui::Ui, app: &mut AppState, state: &mut UiState) {
    let (actions, scheduled_thread, has_children, data_race_exists, children_complete) = {
        let Some(node) = app.nodes.get(state.selected_node) else {
            ui.label("no node selected");
            return;
        };
        let children = app.alive_children(state.selected_node);
        let has_children = !children.is_empty();
        let data_race_exists = node.data_race;
        (
            node.next_actions.clone(),
            node.scheduled_thread,
            has_children,
            data_race_exists,
            node.children_complete,
        )
    };

    if let Some(locked) = scheduled_thread {
        ui.horizontal(|ui| {
            ui.label(format!("scheduled thread: t{}", locked));
            if ui.button("Reset scheduling").clicked() {
                if has_children {
                    state.confirm_dialog = Some(ConfirmDialog::ResetScheduling {
                        node_id: state.selected_node,
                    });
                } else {
                    app.prune_children(state.selected_node);
                    state.selected_action_thread = None;
                    state.selected_child_preview = None;
                    if let Some(node) = app.nodes.get_mut(state.selected_node) {
                        node.data_race = false;
                        node.children_complete = false;
                    }
                }
            }
        });
    } else {
        ui.label("select one action to schedule");
    }
    ui.separator();

    let selected_thread = scheduled_thread.or(state.selected_action_thread);
    let mut schedule_thread: Option<usize> = None;
    for (tid, action) in actions.iter().enumerate() {
        let selected = selected_thread == Some(tid);
        let selectable = ui.add_enabled(
            scheduled_thread.is_none(),
            egui::SelectableLabel::new(selected, format!("t{}: {}", tid, format_action(action))),
        );
        if scheduled_thread.is_none() && selectable.clicked() {
            schedule_thread = Some(tid);
        }
    }
    if let Some(tid) = schedule_thread {
        if let Some(node) = app.nodes.get_mut(state.selected_node) {
            node.scheduled_thread = Some(tid);
            node.children_complete = false;
        }
        state.selected_action_thread = Some(tid);
    }

    ui.separator();
    let mut add_child_clicked = false;
    let editable = selected_thread
        .and_then(|tid| actions.get(tid))
        .is_some_and(|action| matches!(action, NextAction::Read { .. } | NextAction::Write { .. }));
    let add_child_allowed =
        editable && selected_thread.is_some() && !data_race_exists && !children_complete;
    if ui
        .add_enabled(add_child_allowed, egui::Button::new("Add child"))
        .clicked()
    {
        add_child_clicked = true;
    }

    if scheduled_thread.is_some() {
        let mut complete = children_complete;
        if ui
            .checkbox(&mut complete, "All children added")
            .clicked()
        {
            if let Some(node) = app.nodes.get_mut(state.selected_node) {
                node.children_complete = complete;
            }
        }
        let mut checked = data_race_exists;
        let checkbox = ui.checkbox(&mut checked, "Declare data race");
        if checkbox.clicked() {
            if checked {
                if has_children {
                    state.confirm_dialog = Some(ConfirmDialog::DeclareDataRace {
                        node_id: state.selected_node,
                    });
                } else if let Some(node) = app.nodes.get_mut(state.selected_node) {
                    node.data_race = true;
                }
            } else if let Some(node) = app.nodes.get_mut(state.selected_node) {
                node.data_race = false;
            }
        }
    }

    if add_child_clicked {
        if let Some(tid) = selected_thread {
            if let Some(action) = actions.get(tid) {
                state.mode = Mode::Edit(make_edit_state(app, state.selected_node, tid, action));
                state.status = None;
            }
        }
    }
}

fn make_edit_state(
    app: &AppState,
    parent_id: usize,
    thread_id: usize,
    action: &NextAction,
) -> EditState {
    let mut graph = EditableExecutionGraph::from_complete(&app.nodes[parent_id].graph);
    match action {
        NextAction::Read { location, order } => {
            graph.add_read_unset(thread_id, *location, *order);
        }
        NextAction::Write {
            location,
            value,
            order,
        } => {
            graph.add_write_unset(thread_id, *location, *value, *order);
        }
        _ => {}
    }
    EditState {
        parent_id,
        thread_id,
        graph,
        selected_event: None,
        commands: Vec::new(),
    }
}

fn show_edit_controls(ui: &mut egui::Ui, edit: &mut EditState) {
    let Some(selected) = edit.selected_event else {
        ui.label("select an event to edit");
        return;
    };
    let Some(event_kind) = edit.graph.events.get(selected).map(|e| e.kind.clone()) else {
        ui.label("selected event not available");
        return;
    };
    ui.label(format!("selected event: e{}", selected));
    if ui.button("Delete event").clicked() {
        if edit.graph.remove_event(selected) {
            edit.commands.push(format!("del {}", selected));
            edit.selected_event = None;
        }
    }

    match &event_kind {
        EditableEventKind::Read { .. } => {
            ui.label("rf: click a write event to set");
        }
        EditableEventKind::Write { location, .. } => {
            ui.separator();
            ui.label(format!("co order for s{}", location));
            if let Some(list) = edit.graph.co.get(*location).cloned() {
                let mut set_index: Option<usize> = None;
                for (idx, event_id) in list.iter().enumerate().skip(1) {
                    ui.horizontal(|ui| {
                        ui.label(format!("index {} -> e{}", idx, event_id));
                        if ui.button("set here").clicked() {
                            set_index = Some(idx);
                        }
                    });
                }
                let append_index = list.len();
                if ui
                    .button(format!("append at index {}", append_index))
                    .clicked()
                {
                    set_index = Some(append_index);
                }
                if let Some(idx) = set_index {
                    if edit.graph.set_write_co(selected, idx) {
                        edit.commands.push(format!("set-co {} {}", selected, idx));
                    }
                }
            }
        }
        EditableEventKind::Init => {}
    }
}

fn show_children_list(ui: &mut egui::Ui, app: &AppState, state: &mut UiState) {
    let children = app.alive_children(state.selected_node);
    if children.is_empty() {
        ui.label("no children");
        return;
    }
    for child_id in children {
        ui.horizontal(|ui| {
            let selected = state.selected_child_preview == Some(child_id);
            if ui
                .selectable_label(selected, format!("child {}", child_id))
                .clicked()
            {
                state.selected_child_preview = Some(child_id);
            }
        });
        if let Some(child) = app.nodes.get(child_id) {
            if !child.creation_commands.is_empty() {
                ui.label(format!("commands: {}", child.creation_commands.join("; ")));
            }
        }
    }
}

fn handle_edit_click(edit: &mut EditState, clicked_id: usize) {
    if let Some(selected) = edit.selected_event {
        if selected == clicked_id {
            edit.selected_event = None;
            return;
        }
        if try_set_rf(edit, selected, clicked_id) {
            return;
        }
    }
    edit.selected_event = Some(clicked_id);
}

fn try_set_rf(edit: &mut EditState, read_id: usize, rf_id: usize) -> bool {
    let is_read = matches!(
        edit.graph.events.get(read_id).map(|e| &e.kind),
        Some(EditableEventKind::Read { .. })
    );
    let is_write = matches!(
        edit.graph.events.get(rf_id).map(|e| &e.kind),
        Some(EditableEventKind::Write { .. }) | Some(EditableEventKind::Init)
    );
    if is_read && is_write {
        if edit.graph.set_read_rf(read_id, rf_id) {
            edit.commands.push(format!("set-rf {} {}", read_id, rf_id));
            return true;
        }
    }
    false
}

fn draw_execution_graph(
    ui: &mut egui::Ui,
    graph: &ExecutionGraph,
    selected_event: Option<usize>,
    colors: &GraphColors,
    id_prefix: &str,
) {
    let alive = |id: usize| graph.events.get(id).is_some_and(|e| e.alive);
    let (positions, size) = compute_layout(&graph.thread_events, alive);
    egui::ScrollArea::both().show(ui, |ui| {
        let (rect, _response) = ui.allocate_exact_size(size, egui::Sense::hover());
        let painter = ui.painter_at(rect);
        let obstacles = build_obstacles(rect, &positions, NODE_SIZE.length() * 0.5, 4.0);
        draw_lane_labels(&painter, rect, graph.thread_events.len());
        draw_po_edges(&painter, rect, &positions, &graph.thread_events, &colors.po);
        draw_init_po_edges(&painter, rect, &positions, &graph.thread_events, &colors.po);
        draw_rf_edges(&painter, rect, &positions, graph, &obstacles, &colors.rf);
        draw_co_edges(&painter, rect, &positions, &graph.co, &obstacles, &colors.co);
        draw_nodes_execution(
            ui,
            &painter,
            rect,
            graph,
            &positions,
            selected_event,
            id_prefix,
        );
    });
}

fn draw_editable_graph(
    ui: &mut egui::Ui,
    graph: &EditableExecutionGraph,
    selected_event: Option<usize>,
    colors: &GraphColors,
    id_prefix: &str,
) -> Option<usize> {
    let alive = |id: usize| graph.events.get(id).is_some_and(|e| e.alive);
    let (positions, size) = compute_layout(&graph.thread_events, alive);
    let mut clicked = None;
    egui::ScrollArea::both().show(ui, |ui| {
        let (rect, _response) = ui.allocate_exact_size(size, egui::Sense::hover());
        let painter = ui.painter_at(rect);
        let obstacles = build_obstacles(rect, &positions, NODE_SIZE.length() * 0.5, 4.0);
        draw_lane_labels(&painter, rect, graph.thread_events.len());
        draw_po_edges(&painter, rect, &positions, &graph.thread_events, &colors.po);
        draw_init_po_edges(&painter, rect, &positions, &graph.thread_events, &colors.po);
        draw_rf_edges_editable(&painter, rect, &positions, graph, &obstacles, &colors.rf);
        draw_co_edges(&painter, rect, &positions, &graph.co, &obstacles, &colors.co);
        clicked = draw_nodes_editable(
            ui,
            &painter,
            rect,
            graph,
            &positions,
            selected_event,
            id_prefix,
        );
    });
    clicked
}

fn compute_layout(
    thread_events: &[Vec<usize>],
    alive: impl Fn(usize) -> bool,
) -> (HashMap<usize, egui::Pos2>, egui::Vec2) {
    let lane_height = 110.0;
    let node_spacing = 130.0;
    let margin = 40.0;
    let max_events = thread_events.iter().map(|t| t.len()).max().unwrap_or(0);
    let width = margin * 2.0 + node_spacing * (max_events as f32 + 1.0);
    let height = margin * 2.0 + lane_height * (thread_events.len() as f32 + 1.0);
    let mut positions = HashMap::new();

    if alive(0) {
        positions.insert(0, egui::pos2(margin, margin));
    }
    for (tid, events) in thread_events.iter().enumerate() {
        let y = margin + lane_height * (tid as f32 + 1.0);
        for (idx, event_id) in events.iter().enumerate() {
            if alive(*event_id) {
                let x = margin + node_spacing * (idx as f32 + 1.0);
                positions.insert(*event_id, egui::pos2(x, y));
            }
        }
    }
    (positions, egui::vec2(width, height))
}

fn draw_lane_labels(painter: &egui::Painter, rect: egui::Rect, threads: usize) {
    let lane_height = 80.0;
    let margin = 40.0;
    for tid in 0..threads {
        let y = rect.min.y + margin + lane_height * (tid as f32 + 1.0);
        painter.text(
            egui::pos2(rect.min.x + 5.0, y - 14.0),
            egui::Align2::LEFT_TOP,
            format!("t{}", tid),
            egui::FontId::monospace(12.0),
            egui::Color32::GRAY,
        );
    }
    painter.text(
        egui::pos2(rect.min.x + 5.0, rect.min.y + 5.0),
        egui::Align2::LEFT_TOP,
        "init",
        egui::FontId::monospace(12.0),
        egui::Color32::GRAY,
    );
}

fn draw_po_edges(
    painter: &egui::Painter,
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    thread_events: &[Vec<usize>],
    color: &egui::Color32,
) {
    for events in thread_events {
        for window in events.windows(2) {
            if let [a, b] = window {
                if let (Some(start), Some(end)) = (positions.get(a), positions.get(b)) {
                    draw_arrow(
                        painter,
                        to_abs(rect, *start),
                        to_abs(rect, *end),
                        egui::Stroke::new(1.5, *color),
                        8.0,
                        6.0,
                        14.0,
                    );
                }
            }
        }
    }
}

fn draw_init_po_edges(
    painter: &egui::Painter,
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    thread_events: &[Vec<usize>],
    color: &egui::Color32,
) {
    let Some(init_pos) = positions.get(&0) else {
        return;
    };
    for events in thread_events {
        let first_alive = events.iter().find(|id| positions.contains_key(id));
        if let Some(first) = first_alive {
            if let Some(target) = positions.get(first) {
                draw_arrow(
                    painter,
                    to_abs(rect, *init_pos),
                    to_abs(rect, *target),
                    egui::Stroke::new(1.0, *color),
                    8.0,
                    6.0,
                    14.0,
                );
            }
        }
    }
}

fn draw_rf_edges(
    painter: &egui::Painter,
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    graph: &ExecutionGraph,
    obstacles: &[Obstacle],
    color: &egui::Color32,
) {
    for (id, event) in graph.events.iter().enumerate() {
        if !event.alive {
            continue;
        }
        if let EventKind::Read { rf, .. } = event.kind {
            if let (Some(start), Some(end)) = (positions.get(&rf), positions.get(&id)) {
                draw_routed_arrow(
                    painter,
                    to_abs(rect, *start),
                    to_abs(rect, *end),
                    obstacles,
                    rf,
                    id,
                    egui::Stroke::new(2.0, *color),
                    10.0,
                    7.0,
                    14.0,
                );
            }
        }
    }
}

fn draw_rf_edges_editable(
    painter: &egui::Painter,
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    graph: &EditableExecutionGraph,
    obstacles: &[Obstacle],
    color: &egui::Color32,
) {
    for (id, event) in graph.events.iter().enumerate() {
        if !event.alive {
            continue;
        }
        if let EditableEventKind::Read { rf: Some(rf), .. } = event.kind {
            if let (Some(start), Some(end)) = (positions.get(&rf), positions.get(&id)) {
                draw_routed_arrow(
                    painter,
                    to_abs(rect, *start),
                    to_abs(rect, *end),
                    obstacles,
                    rf,
                    id,
                    egui::Stroke::new(2.0, *color),
                    10.0,
                    7.0,
                    14.0,
                );
            }
        }
    }
}

fn draw_co_edges(
    painter: &egui::Painter,
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    co: &[Vec<usize>],
    obstacles: &[Obstacle],
    color: &egui::Color32,
) {
    for events in co {
        for window in events.windows(2) {
            if let [a, b] = window {
                if let (Some(start), Some(end)) = (positions.get(a), positions.get(b)) {
                    draw_routed_arrow(
                        painter,
                        to_abs(rect, *start),
                        to_abs(rect, *end),
                        obstacles,
                        *a,
                        *b,
                        egui::Stroke::new(2.0, *color),
                        10.0,
                        7.0,
                        14.0,
                    );
                }
            }
        }
    }
}

fn draw_nodes_execution(
    ui: &mut egui::Ui,
    painter: &egui::Painter,
    rect: egui::Rect,
    graph: &ExecutionGraph,
    positions: &HashMap<usize, egui::Pos2>,
    selected_event: Option<usize>,
    id_prefix: &str,
) {
    for (id, event) in graph.events.iter().enumerate() {
        if !event.alive {
            continue;
        }
        let Some(pos) = positions.get(&id) else {
            continue;
        };
        let (label, fill) = match &event.kind {
            EventKind::Init => ("init".to_string(), egui::Color32::from_rgb(120, 140, 180)),
            EventKind::Read { location, order, .. } => (
                format!("R(s{}) ({})", location, format_order(*order)),
                egui::Color32::from_rgb(120, 190, 120),
            ),
            EventKind::Write {
                location,
                value,
                order,
            } => (
                format!("W(s{}, {}) ({})", location, value, format_order(*order)),
                egui::Color32::from_rgb(190, 150, 120),
            ),
        };
        let center = to_abs(rect, *pos);
        let node_rect = egui::Rect::from_center_size(center, NODE_SIZE);
        painter.rect_filled(node_rect, 6.0, fill);
        if selected_event == Some(id) {
            painter.rect_stroke(
                node_rect.expand(3.0),
                6.0,
                egui::Stroke::new(2.0, egui::Color32::YELLOW),
            );
        }
        let response = ui.interact(
            node_rect,
            ui.make_persistent_id((id_prefix, id)),
            egui::Sense::click(),
        );
        painter.text(
            egui::pos2(node_rect.center().x, node_rect.top() + 4.0),
            egui::Align2::CENTER_TOP,
            format!("{}", id),
            egui::FontId::monospace(10.0),
            egui::Color32::BLACK,
        );
        painter.text(
            node_rect.center() + egui::vec2(0.0, 6.0),
            egui::Align2::CENTER_CENTER,
            label,
            egui::FontId::monospace(11.0),
            egui::Color32::BLACK,
        );
        let _ = response;
    }
}

fn draw_nodes_editable(
    ui: &mut egui::Ui,
    painter: &egui::Painter,
    rect: egui::Rect,
    graph: &EditableExecutionGraph,
    positions: &HashMap<usize, egui::Pos2>,
    selected_event: Option<usize>,
    id_prefix: &str,
) -> Option<usize> {
    let mut clicked = None;
    for (id, event) in graph.events.iter().enumerate() {
        if !event.alive {
            continue;
        }
        let Some(pos) = positions.get(&id) else {
            continue;
        };
        let (label, fill) = match &event.kind {
            EditableEventKind::Init => {
                ("init".to_string(), egui::Color32::from_rgb(120, 140, 180))
            }
            EditableEventKind::Read { location, order, .. } => (
                format!("R(s{}) ({})", location, format_order(*order)),
                egui::Color32::from_rgb(120, 190, 120),
            ),
            EditableEventKind::Write {
                location,
                value,
                order,
            } => (
                format!("W(s{}, {}) ({})", location, value, format_order(*order)),
                egui::Color32::from_rgb(190, 150, 120),
            ),
        };
        let center = to_abs(rect, *pos);
        let node_rect = egui::Rect::from_center_size(center, NODE_SIZE);
        painter.rect_filled(node_rect, 6.0, fill);
        if selected_event == Some(id) {
            painter.rect_stroke(
                node_rect.expand(3.0),
                6.0,
                egui::Stroke::new(2.0, egui::Color32::YELLOW),
            );
        }
        let response = ui.interact(
            node_rect,
            ui.make_persistent_id((id_prefix, id)),
            egui::Sense::click(),
        );
        if response.clicked() {
            clicked = Some(id);
        }
        painter.text(
            egui::pos2(node_rect.center().x, node_rect.top() + 4.0),
            egui::Align2::CENTER_TOP,
            format!("{}", id),
            egui::FontId::monospace(10.0),
            egui::Color32::BLACK,
        );
        painter.text(
            node_rect.center() + egui::vec2(0.0, 6.0),
            egui::Align2::CENTER_CENTER,
            label,
            egui::FontId::monospace(11.0),
            egui::Color32::BLACK,
        );
    }
    clicked
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

fn format_order(order: MemoryOrder) -> &'static str {
    match order {
        MemoryOrder::RelaxedAtomic => "RA",
        MemoryOrder::NonAtomic => "NA",
    }
}

fn to_abs(rect: egui::Rect, pos: egui::Pos2) -> egui::Pos2 {
    egui::pos2(rect.min.x + pos.x, rect.min.y + pos.y)
}

fn build_obstacles(
    rect: egui::Rect,
    positions: &HashMap<usize, egui::Pos2>,
    radius: f32,
    padding: f32,
) -> Vec<Obstacle> {
    positions
        .iter()
        .map(|(id, pos)| Obstacle {
            id: *id,
            center: to_abs(rect, *pos),
            radius: radius + padding,
        })
        .collect()
}

fn draw_arrow(
    painter: &egui::Painter,
    start: egui::Pos2,
    end: egui::Pos2,
    stroke: egui::Stroke,
    head_len: f32,
    head_width: f32,
    end_padding: f32,
) {
    let dir = end - start;
    let len = dir.length();
    if len < 1.0 || len <= end_padding + head_len {
        return;
    }
    let dir_norm = dir / len;
    let tip = end - dir_norm * end_padding;
    let head_base = tip - dir_norm * head_len;
    let ortho = egui::vec2(-dir_norm.y, dir_norm.x) * (head_width * 0.5);
    let left = head_base + ortho;
    let right = head_base - ortho;

    painter.line_segment([start, head_base], stroke);
    painter.add(egui::Shape::convex_polygon(
        vec![tip, left, right],
        stroke.color,
        egui::Stroke::NONE,
    ));
}

fn draw_routed_arrow(
    painter: &egui::Painter,
    start: egui::Pos2,
    end: egui::Pos2,
    obstacles: &[Obstacle],
    start_id: usize,
    end_id: usize,
    stroke: egui::Stroke,
    head_len: f32,
    head_width: f32,
    end_padding: f32,
) {
    if is_segment_clear(start, end, obstacles, start_id, end_id) {
        draw_arrow(painter, start, end, stroke, head_len, head_width, end_padding);
        return;
    }

    let dir = end - start;
    let len = dir.length();
    if len < 1.0 {
        return;
    }
    let dir_norm = dir / len;
    let perp = egui::vec2(-dir_norm.y, dir_norm.x);
    let mid = start + dir * 0.5;
    let offsets = [30.0, 60.0, 90.0, 120.0];
    let mut best_control = None;
    let mut best_score = usize::MAX;

    for offset in offsets {
        for sign in [-1.0, 1.0] {
            let control = mid + perp * (offset * sign);
            let score = curve_intersections(start, control, end, obstacles, start_id, end_id);
            if score == 0 {
                best_control = Some(control);
                best_score = 0;
                break;
            }
            if score < best_score {
                best_score = score;
                best_control = Some(control);
            }
        }
        if best_score == 0 {
            break;
        }
    }

    if let Some(control) = best_control {
        draw_quadratic_arrow(
            painter,
            start,
            control,
            end,
            stroke,
            head_len,
            head_width,
            end_padding,
        );
    } else {
        draw_arrow(painter, start, end, stroke, head_len, head_width, end_padding);
    }
}

fn is_segment_clear(
    start: egui::Pos2,
    end: egui::Pos2,
    obstacles: &[Obstacle],
    start_id: usize,
    end_id: usize,
) -> bool {
    for obs in obstacles {
        if obs.id == start_id || obs.id == end_id {
            continue;
        }
        if segment_intersects_circle(start, end, obs.center, obs.radius) {
            return false;
        }
    }
    true
}

fn segment_intersects_circle(
    start: egui::Pos2,
    end: egui::Pos2,
    center: egui::Pos2,
    radius: f32,
) -> bool {
    let seg = end - start;
    let len2 = seg.length_sq();
    if len2 <= f32::EPSILON {
        return (center - start).length() <= radius;
    }
    let t = ((center - start).dot(seg) / len2).clamp(0.0, 1.0);
    let proj = start + seg * t;
    (center - proj).length() <= radius
}

fn curve_intersections(
    start: egui::Pos2,
    control: egui::Pos2,
    end: egui::Pos2,
    obstacles: &[Obstacle],
    start_id: usize,
    end_id: usize,
) -> usize {
    let samples = 16;
    let mut count = 0;
    let mut prev = start;
    for i in 1..=samples {
        let t = i as f32 / samples as f32;
        let point = quadratic_point(start, control, end, t);
        for obs in obstacles {
            if obs.id == start_id || obs.id == end_id {
                continue;
            }
            if segment_intersects_circle(prev, point, obs.center, obs.radius) {
                count += 1;
                break;
            }
        }
        prev = point;
    }
    count
}

fn quadratic_point(
    start: egui::Pos2,
    control: egui::Pos2,
    end: egui::Pos2,
    t: f32,
) -> egui::Pos2 {
    let one = 1.0 - t;
    let s = start.to_vec2() * (one * one);
    let c = control.to_vec2() * (2.0 * one * t);
    let e = end.to_vec2() * (t * t);
    egui::Pos2::new(s.x + c.x + e.x, s.y + c.y + e.y)
}

fn draw_quadratic_arrow(
    painter: &egui::Painter,
    start: egui::Pos2,
    control: egui::Pos2,
    end: egui::Pos2,
    stroke: egui::Stroke,
    head_len: f32,
    head_width: f32,
    end_padding: f32,
) {
    let tangent = end - control;
    let tangent_len = tangent.length();
    if tangent_len < 1.0 {
        draw_arrow(painter, start, end, stroke, head_len, head_width, end_padding);
        return;
    }
    let dir = tangent / tangent_len;
    let tip = end - dir * end_padding;
    let head_base = tip - dir * head_len;
    let mut points = Vec::new();
    let samples = 20;
    for i in 0..=samples {
        let t = i as f32 / samples as f32;
        let mut point = quadratic_point(start, control, end, t);
        if i == samples {
            point = head_base;
        }
        points.push(point);
    }
    for window in points.windows(2) {
        if let [a, b] = window {
            painter.line_segment([*a, *b], stroke);
        }
    }
    let ortho = egui::vec2(-dir.y, dir.x) * (head_width * 0.5);
    let left = head_base + ortho;
    let right = head_base - ortho;
    painter.add(egui::Shape::convex_polygon(
        vec![tip, left, right],
        stroke.color,
        egui::Stroke::NONE,
    ));
}
