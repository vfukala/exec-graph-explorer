use crate::exec_graph::{EventKind, ExecutionGraph};
use crate::model::{Assign, BinOp, Expr, MemoryOrder, Program, Stmt};

#[derive(Clone, Debug)]
pub enum NextAction {
    Read {
        location: usize,
        order: MemoryOrder,
    },
    Write {
        location: usize,
        value: i64,
        order: MemoryOrder,
    },
    AssertionViolation,
    Blocked,
    Finished,
    Inconsistent(String),
}

pub fn next_actions(program: &Program, graph: &ExecutionGraph) -> Vec<NextAction> {
    (0..program.threads.len())
        .map(|thread_id| next_action_for_thread(program, graph, thread_id))
        .collect()
}

fn next_action_for_thread(
    program: &Program,
    graph: &ExecutionGraph,
    thread_id: usize,
) -> NextAction {
    let thread = &program.threads[thread_id];
    let mut locals = vec![0i64; thread.locals.len()];
    let mut event_index = 0usize;
    let thread_events = &graph.thread_events[thread_id];

    let mut ctx = EvalContext {
        locals: &mut locals,
        graph,
        thread_events,
        event_index: &mut event_index,
    };

    match eval_stmt(&thread.stmt, &mut ctx) {
        EvalOutcome::Action(action) => action,
        EvalOutcome::Continue => NextAction::Finished,
    }
}

struct EvalContext<'a> {
    locals: &'a mut [i64],
    graph: &'a ExecutionGraph,
    thread_events: &'a [usize],
    event_index: &'a mut usize,
}

enum EvalOutcome {
    Continue,
    Action(NextAction),
}

fn eval_stmt(stmt: &Stmt, ctx: &mut EvalContext<'_>) -> EvalOutcome {
    match stmt {
        Stmt::NoOp => EvalOutcome::Continue,
        Stmt::Assert(cond) => {
            if eval_expr(cond, ctx.locals) == 0 {
                EvalOutcome::Action(NextAction::AssertionViolation)
            } else {
                EvalOutcome::Continue
            }
        }
        Stmt::Assume(cond) => {
            if eval_expr(cond, ctx.locals) == 0 {
                EvalOutcome::Action(NextAction::Blocked)
            } else {
                EvalOutcome::Continue
            }
        }
        Stmt::Assign(assign) => eval_assign(assign, ctx),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            if eval_expr(cond, ctx.locals) != 0 {
                eval_stmt(then_branch, ctx)
            } else {
                eval_stmt(else_branch, ctx)
            }
        }
        Stmt::While { cond, body } => {
            while eval_expr(cond, ctx.locals) != 0 {
                match eval_stmt(body, ctx) {
                    EvalOutcome::Continue => {}
                    action @ EvalOutcome::Action(_) => return action,
                }
            }
            EvalOutcome::Continue
        }
        Stmt::Seq { first, second } => match eval_stmt(first, ctx) {
            EvalOutcome::Continue => eval_stmt(second, ctx),
            action @ EvalOutcome::Action(_) => action,
        },
    }
}

fn eval_assign(assign: &Assign, ctx: &mut EvalContext<'_>) -> EvalOutcome {
    match assign {
        Assign::Local { dst, value } => {
            let val = eval_expr(value, ctx.locals);
            if let Some(slot) = ctx.locals.get_mut(*dst) {
                *slot = val;
            }
            EvalOutcome::Continue
        }
        Assign::Store {
            location,
            value,
            order,
        } => {
            let val = eval_expr(value, ctx.locals);
            match next_or_validate_event(ctx, *location, None, *order, true, val) {
                Ok(Some(action)) => EvalOutcome::Action(action),
                Ok(None) => EvalOutcome::Continue,
                Err(action) => EvalOutcome::Action(action),
            }
        }
        Assign::Load {
            location,
            dst,
            order,
        } => match next_or_validate_event(ctx, *location, Some(*dst), *order, false, 0) {
            Ok(Some(action)) => EvalOutcome::Action(action),
            Ok(None) => EvalOutcome::Continue,
            Err(action) => EvalOutcome::Action(action),
        },
    }
}

fn next_or_validate_event(
    ctx: &mut EvalContext<'_>,
    location: usize,
    dst: Option<usize>,
    order: MemoryOrder,
    is_write: bool,
    write_value: i64,
) -> Result<Option<NextAction>, NextAction> {
    let Some(&event_id) = ctx.thread_events.get(*ctx.event_index) else {
        return Ok(Some(if is_write {
            NextAction::Write {
                location,
                value: write_value,
                order,
            }
        } else {
            NextAction::Read { location, order }
        }));
    };

    let event = ctx
        .graph
        .events
        .get(event_id)
        .ok_or_else(|| NextAction::Inconsistent("missing event".to_string()))?;
    if !event.alive {
        return Err(NextAction::Inconsistent("event removed".to_string()));
    }

    match (&event.kind, is_write) {
        (
            EventKind::Write {
                location: ev_loc,
                value: ev_val,
                order: ev_order,
            },
            true,
        ) => {
            if *ev_loc != location || *ev_val != write_value || *ev_order != order {
                return Err(NextAction::Inconsistent("write event mismatch".to_string()));
            }
        }
        (
            EventKind::Read {
                location: ev_loc,
                rf,
                order: ev_order,
            },
            false,
        ) => {
            if *ev_loc != location || *ev_order != order {
                return Err(NextAction::Inconsistent("read event mismatch".to_string()));
            }
            if let Some(dst) = dst {
                let value = ctx
                    .graph
                    .value_of_event(*rf, location)
                    .ok_or_else(|| NextAction::Inconsistent("rf value missing".to_string()))?;
                if let Some(slot) = ctx.locals.get_mut(dst) {
                    *slot = value;
                }
            }
        }
        _ => {
            return Err(NextAction::Inconsistent(
                "event kind mismatch".to_string(),
            ))
        }
    }

    *ctx.event_index += 1;
    Ok(None)
}

fn eval_expr(expr: &Expr, locals: &[i64]) -> i64 {
    match expr {
        Expr::Local(id) => locals.get(*id).copied().unwrap_or(0),
        Expr::Const(val) => *val,
        Expr::BinOp { op, left, right } => {
            let l = eval_expr(left, locals);
            let r = eval_expr(right, locals);
            match op {
                BinOp::Add => l.saturating_add(r),
                BinOp::Sub => l.saturating_sub(r),
                BinOp::Mul => l.saturating_mul(r),
                BinOp::Div => {
                    if r == 0 {
                        0
                    } else {
                        l / r
                    }
                }
            }
        }
    }
}
