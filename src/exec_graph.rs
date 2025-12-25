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
