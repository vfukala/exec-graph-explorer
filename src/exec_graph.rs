use crate::model::{MemoryOrder, Program};

pub type EventId = usize;

#[derive(Clone, Debug)]
pub struct ExecutionGraph {
    pub init_values: Vec<i64>,
    pub events: Vec<Event>,
    pub thread_events: Vec<Vec<EventId>>,
    pub event_order: Vec<EventId>,
    pub co: Vec<Vec<EventId>>,
}

#[derive(Clone, Debug)]
pub struct Event {
    pub kind: EventKind,
    pub thread: Option<usize>,
    pub alive: bool,
}

#[derive(Clone, Debug)]
pub enum EventKind {
    Init,
    Read {
        location: usize,
        rf: EventId,
        order: MemoryOrder,
    },
    Write {
        location: usize,
        value: i64,
        order: MemoryOrder,
    },
}

impl ExecutionGraph {
    pub fn new(program: &Program) -> Self {
        let init_values = program.shared.iter().map(|v| v.init).collect();
        let init_event = Event {
            kind: EventKind::Init,
            thread: None,
            alive: true,
        };
        let mut co = Vec::with_capacity(program.shared.len());
        for _ in 0..program.shared.len() {
            co.push(vec![0]);
        }
        Self {
            init_values,
            events: vec![init_event],
            thread_events: vec![Vec::new(); program.threads.len()],
            event_order: vec![0],
            co,
        }
    }

    pub fn value_of_event(&self, event_id: EventId, location: usize) -> Option<i64> {
        let event = self.events.get(event_id)?;
        if !event.alive {
            return None;
        }
        match event.kind {
            EventKind::Init => self.init_values.get(location).copied(),
            EventKind::Write { value, location: loc, .. } => {
                if loc == location {
                    Some(value)
                } else {
                    None
                }
            }
            EventKind::Read { .. } => None,
        }
    }

    pub fn add_read(
        &mut self,
        thread: usize,
        location: usize,
        rf: EventId,
        order: MemoryOrder,
    ) -> EventId {
        let id = self.events.len();
        let event = Event {
            kind: EventKind::Read {
                location,
                rf,
                order,
            },
            thread: Some(thread),
            alive: true,
        };
        self.events.push(event);
        self.thread_events[thread].push(id);
        self.event_order.push(id);
        id
    }

    pub fn add_write(
        &mut self,
        thread: usize,
        location: usize,
        value: i64,
        order: MemoryOrder,
    ) -> EventId {
        let id = self.events.len();
        let event = Event {
            kind: EventKind::Write {
                location,
                value,
                order,
            },
            thread: Some(thread),
            alive: true,
        };
        self.events.push(event);
        self.thread_events[thread].push(id);
        self.event_order.push(id);
        self.co[location].push(id);
        id
    }

    pub fn add_write_at(
        &mut self,
        thread: usize,
        location: usize,
        value: i64,
        order: MemoryOrder,
        co_index: usize,
    ) -> Option<EventId> {
        if co_index == 0 || co_index > self.co.get(location)?.len() {
            return None;
        }
        let id = self.events.len();
        let event = Event {
            kind: EventKind::Write {
                location,
                value,
                order,
            },
            thread: Some(thread),
            alive: true,
        };
        self.events.push(event);
        self.thread_events[thread].push(id);
        self.event_order.push(id);
        self.co[location].insert(co_index, id);
        Some(id)
    }

    pub fn remove_last_event(&mut self, thread: usize) -> Option<EventId> {
        let id = self.thread_events.get_mut(thread)?.pop()?;
        if let Some(event) = self.events.get_mut(id) {
            event.alive = false;
        }
        self.event_order.retain(|&evt| evt != id);
        for list in &mut self.co {
            list.retain(|&evt| evt != id);
        }
        Some(id)
    }

    pub fn remove_event(&mut self, event_id: EventId) -> bool {
        if event_id == 0 {
            return false;
        }
        let Some(event) = self.events.get_mut(event_id) else {
            return false;
        };
        if !event.alive {
            return false;
        }
        event.alive = false;
        if let Some(thread) = event.thread {
            if let Some(list) = self.thread_events.get_mut(thread) {
                list.retain(|&evt| evt != event_id);
            }
        }
        self.event_order.retain(|&evt| evt != event_id);
        for list in &mut self.co {
            list.retain(|&evt| evt != event_id);
        }
        true
    }

    pub fn set_read_rf(&mut self, event_id: EventId, rf: EventId) -> bool {
        let Some(event) = self.events.get_mut(event_id) else {
            return false;
        };
        if !event.alive {
            return false;
        }
        match &mut event.kind {
            EventKind::Read { rf: current, .. } => {
                *current = rf;
                true
            }
            _ => false,
        }
    }

    pub fn move_write_co(&mut self, event_id: EventId, new_index: usize) -> bool {
        if event_id == 0 || new_index == 0 {
            return false;
        }
        let location = match self.events.get(event_id) {
            Some(Event {
                kind: EventKind::Write { location, .. },
                alive: true,
                ..
            }) => *location,
            _ => return false,
        };
        let Some(list) = self.co.get_mut(location) else {
            return false;
        };
        let Some(pos) = list.iter().position(|&id| id == event_id) else {
            return false;
        };
        list.remove(pos);
        if new_index > list.len() {
            list.insert(list.len(), event_id);
            return true;
        }
        list.insert(new_index, event_id);
        true
    }

    pub fn writes_for_location(&self, location: usize) -> Vec<EventId> {
        self.co
            .get(location)
            .map(|list| {
                list.iter()
                    .copied()
                    .filter(|id| self.events.get(*id).is_some_and(|e| e.alive))
                    .collect()
            })
            .unwrap_or_default()
    }
}

#[derive(Clone, Debug)]
pub struct EditableExecutionGraph {
    pub init_values: Vec<i64>,
    pub events: Vec<EditableEvent>,
    pub thread_events: Vec<Vec<EventId>>,
    pub event_order: Vec<EventId>,
    pub co: Vec<Vec<EventId>>,
}

#[derive(Clone, Debug)]
pub struct EditableEvent {
    pub kind: EditableEventKind,
    pub thread: Option<usize>,
    pub alive: bool,
}

#[derive(Clone, Debug)]
pub enum EditableEventKind {
    Init,
    Read {
        location: usize,
        rf: Option<EventId>,
        order: MemoryOrder,
    },
    Write {
        location: usize,
        value: i64,
        order: MemoryOrder,
    },
}

impl EditableExecutionGraph {
    pub fn from_complete(graph: &ExecutionGraph) -> Self {
        let events = graph
            .events
            .iter()
            .map(|event| EditableEvent {
                kind: match &event.kind {
                    EventKind::Init => EditableEventKind::Init,
                    EventKind::Read {
                        location,
                        rf,
                        order,
                    } => EditableEventKind::Read {
                        location: *location,
                        rf: Some(*rf),
                        order: *order,
                    },
                    EventKind::Write {
                        location,
                        value,
                        order,
                    } => EditableEventKind::Write {
                        location: *location,
                        value: *value,
                        order: *order,
                    },
                },
                thread: event.thread,
                alive: event.alive,
            })
            .collect();
        Self {
            init_values: graph.init_values.clone(),
            events,
            thread_events: graph.thread_events.clone(),
            event_order: graph.event_order.clone(),
            co: graph.co.clone(),
        }
    }

    pub fn add_read_unset(
        &mut self,
        thread: usize,
        location: usize,
        order: MemoryOrder,
    ) -> EventId {
        let id = self.events.len();
        let event = EditableEvent {
            kind: EditableEventKind::Read {
                location,
                rf: None,
                order,
            },
            thread: Some(thread),
            alive: true,
        };
        self.events.push(event);
        self.thread_events[thread].push(id);
        self.event_order.push(id);
        id
    }

    pub fn add_write_unset(
        &mut self,
        thread: usize,
        location: usize,
        value: i64,
        order: MemoryOrder,
    ) -> EventId {
        let id = self.events.len();
        let event = EditableEvent {
            kind: EditableEventKind::Write {
                location,
                value,
                order,
            },
            thread: Some(thread),
            alive: true,
        };
        self.events.push(event);
        self.thread_events[thread].push(id);
        self.event_order.push(id);
        id
    }

    pub fn set_read_rf(&mut self, event_id: EventId, rf: EventId) -> bool {
        let (read_location, order) = match self.events.get(event_id) {
            Some(EditableEvent {
                kind: EditableEventKind::Read { location, order, .. },
                alive: true,
                ..
            }) => (*location, *order),
            _ => return false,
        };
        let rf_ok = match self.events.get(rf) {
            Some(EditableEvent {
                kind: EditableEventKind::Init,
                alive: true,
                ..
            }) => true,
            Some(EditableEvent {
                kind: EditableEventKind::Write { location, .. },
                alive: true,
                ..
            }) if *location == read_location => true,
            _ => false,
        };
        if !rf_ok {
            return false;
        }
        if let Some(event) = self.events.get_mut(event_id) {
            event.kind = EditableEventKind::Read {
                location: read_location,
                rf: Some(rf),
                order,
            };
            return true;
        }
        false
    }

    pub fn set_write_co(&mut self, event_id: EventId, index: usize) -> bool {
        if event_id == 0 || index == 0 {
            return false;
        }
        let location = match self.events.get(event_id) {
            Some(EditableEvent {
                kind: EditableEventKind::Write { location, .. },
                alive: true,
                ..
            }) => *location,
            _ => return false,
        };
        let Some(list) = self.co.get_mut(location) else {
            return false;
        };
        if let Some(pos) = list.iter().position(|&id| id == event_id) {
            list.remove(pos);
        }
        if index > list.len() {
            list.push(event_id);
        } else {
            list.insert(index, event_id);
        }
        true
    }

    pub fn remove_event(&mut self, event_id: EventId) -> bool {
        if event_id == 0 {
            return false;
        }
        let Some(event) = self.events.get_mut(event_id) else {
            return false;
        };
        if !event.alive {
            return false;
        }
        event.alive = false;
        if let Some(thread) = event.thread {
            if let Some(list) = self.thread_events.get_mut(thread) {
                list.retain(|&evt| evt != event_id);
            }
        }
        self.event_order.retain(|&evt| evt != event_id);
        for list in &mut self.co {
            list.retain(|&evt| evt != event_id);
        }
        for event in &mut self.events {
            if !event.alive {
                continue;
            }
            if let EditableEventKind::Read {
                location,
                rf,
                order,
            } = &event.kind
            {
                if *rf == Some(event_id) {
                    event.kind = EditableEventKind::Read {
                        location: *location,
                        rf: None,
                        order: *order,
                    };
                }
            };
        }
        true
    }

    pub fn validate(&self) -> Vec<String> {
        use std::collections::HashSet;

        let mut errors = Vec::new();
        match self.events.get(0) {
            Some(EditableEvent {
                kind: EditableEventKind::Init,
                alive: true,
                ..
            }) => {}
            _ => errors.push("missing live init event e0".to_string()),
        }

        for (id, event) in self.events.iter().enumerate() {
            if !event.alive {
                continue;
            }
            match &event.kind {
                EditableEventKind::Read { location, rf, .. } => match rf {
                    Some(rf_id) => match self.events.get(*rf_id) {
                        Some(EditableEvent {
                            kind: EditableEventKind::Init,
                            alive: true,
                            ..
                        }) => {}
                        Some(EditableEvent {
                            kind: EditableEventKind::Write { location: rf_loc, .. },
                            alive: true,
                            ..
                        }) if *rf_loc == *location => {}
                        _ => errors.push(format!(
                            "e{}: rf must point to a live init or write for s{}",
                            id, location
                        )),
                    },
                    None => errors.push(format!("e{}: rf is unset", id)),
                },
                EditableEventKind::Write { .. } => {}
                EditableEventKind::Init => {}
            }
        }

        for (loc, list) in self.co.iter().enumerate() {
            let mut seen = HashSet::new();
            for &id in list {
                if !seen.insert(id) {
                    errors.push(format!("co for s{} has duplicate e{}", loc, id));
                    continue;
                }
                match self.events.get(id) {
                    Some(EditableEvent {
                        kind: EditableEventKind::Init,
                        alive: true,
                        ..
                    }) if id == 0 => {}
                    Some(EditableEvent {
                        kind: EditableEventKind::Write { location, .. },
                        alive: true,
                        ..
                    }) if *location == loc => {}
                    _ => errors.push(format!("co for s{} has invalid entry e{}", loc, id)),
                }
            }
            if !seen.contains(&0) {
                errors.push(format!("co for s{} is missing init e0", loc));
            }
            for (id, event) in self.events.iter().enumerate() {
                if !event.alive {
                    continue;
                }
                if let EditableEventKind::Write { location, .. } = event.kind {
                    if location == loc && !seen.contains(&id) {
                        errors.push(format!("co for s{} is missing e{}", loc, id));
                    }
                }
            }
        }

        errors
    }

    pub fn into_complete(self) -> Option<ExecutionGraph> {
        let events = self
            .events
            .into_iter()
            .map(|event| {
                let kind = match event.kind {
                    EditableEventKind::Init => EventKind::Init,
                    EditableEventKind::Read {
                        location,
                        rf,
                        order,
                    } => {
                        let rf = rf?;
                        EventKind::Read {
                            location,
                            rf,
                            order,
                        }
                    }
                    EditableEventKind::Write {
                        location,
                        value,
                        order,
                    } => EventKind::Write {
                        location,
                        value,
                        order,
                    },
                };
                Some(Event {
                    kind,
                    thread: event.thread,
                    alive: event.alive,
                })
            })
            .collect::<Option<Vec<_>>>()?;
        Some(ExecutionGraph {
            init_values: self.init_values,
            events,
            thread_events: self.thread_events,
            event_order: self.event_order,
            co: self.co,
        })
    }
}
