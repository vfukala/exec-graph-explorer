use crate::model::{Assign, BinOp, Expr, Program, Stmt, Thread};
use std::collections::HashSet;

pub fn optimize_program(program: Program) -> Program {
    let threads = program
        .threads
        .into_iter()
        .map(optimize_thread)
        .filter(|thread| !matches!(thread.stmt, Stmt::NoOp))
        .collect();
    let (shared, threads) = remove_unused_shared(program.shared, threads);
    Program { shared, threads }
}

fn optimize_thread(thread: Thread) -> Thread {
    let mut env = vec![Some(0); thread.locals.len()];
    let stmt = simplify_stmt_with_env(thread.stmt, &mut env);
    let stmt = eliminate_dead_locals(stmt, thread.locals.len());
    let stmt = simplify_stmt(stmt);
    remove_unused_locals(Thread {
        locals: thread.locals,
        stmt,
    })
}

fn simplify_stmt_with_env(stmt: Stmt, env: &mut Vec<Option<i64>>) -> Stmt {
    match stmt {
        Stmt::NoOp => Stmt::NoOp,
        Stmt::Assert(expr) => {
            let expr = simplify_expr_with_env(expr, env);
            match const_eval(&expr) {
                Some(value) if value != 0 => Stmt::NoOp,
                _ => Stmt::Assert(expr),
            }
        }
        Stmt::Assume(expr) => {
            let expr = simplify_expr_with_env(expr, env);
            match const_eval(&expr) {
                Some(value) if value != 0 => Stmt::NoOp,
                _ => Stmt::Assume(expr),
            }
        }
        Stmt::Assign(assign) => Stmt::Assign(simplify_assign_with_env(assign, env)),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = simplify_expr_with_env(cond, env);
            if let Some(value) = const_eval(&cond) {
                if value != 0 {
                    simplify_stmt_with_env(*then_branch, env)
                } else {
                    simplify_stmt_with_env(*else_branch, env)
                }
            } else {
                let mut then_env = env.clone();
                let mut else_env = env.clone();
                let then_branch = simplify_stmt_with_env(*then_branch, &mut then_env);
                let else_branch = simplify_stmt_with_env(*else_branch, &mut else_env);
                *env = merge_env(&then_env, &else_env);
                Stmt::If {
                    cond,
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                }
            }
        }
        Stmt::While { cond, body } => {
            let assigned = assigned_locals(&body);
            let mut loop_env = env.clone();
            for id in &assigned {
                loop_env[*id] = None;
            }
            let cond = simplify_expr_with_env(cond, &mut loop_env);
            let body = simplify_stmt_with_env(*body, &mut loop_env);
            if let Some(value) = const_eval(&cond) {
                if value == 0 {
                    Stmt::NoOp
                } else {
                    for id in &assigned {
                        env[*id] = None;
                    }
                    Stmt::While {
                        cond,
                        body: Box::new(body),
                    }
                }
            } else {
                for id in &assigned {
                    env[*id] = None;
                }
                Stmt::While {
                    cond,
                    body: Box::new(body),
                }
            }
        }
        Stmt::Seq { first, second } => {
            let first = simplify_stmt_with_env(*first, env);
            let second = simplify_stmt_with_env(*second, env);
            match (&first, &second) {
                (Stmt::NoOp, _) => second,
                (_, Stmt::NoOp) => first,
                _ => Stmt::Seq {
                    first: Box::new(first),
                    second: Box::new(second),
                },
            }
        }
    }
}

fn simplify_stmt(stmt: Stmt) -> Stmt {
    match stmt {
        Stmt::NoOp => Stmt::NoOp,
        Stmt::Assert(expr) => Stmt::Assert(simplify_expr(expr)),
        Stmt::Assume(expr) => Stmt::Assume(simplify_expr(expr)),
        Stmt::Assign(assign) => Stmt::Assign(simplify_assign(assign)),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = simplify_expr(cond);
            let then_branch = simplify_stmt(*then_branch);
            let else_branch = simplify_stmt(*else_branch);
            if let Some(value) = const_eval(&cond) {
                if value != 0 {
                    then_branch
                } else {
                    else_branch
                }
            } else if matches!((&then_branch, &else_branch), (Stmt::NoOp, Stmt::NoOp)) {
                Stmt::NoOp
            } else {
                Stmt::If {
                    cond,
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                }
            }
        }
        Stmt::While { cond, body } => {
            let cond = simplify_expr(cond);
            let body = simplify_stmt(*body);
            if let Some(value) = const_eval(&cond) {
                if value == 0 {
                    Stmt::NoOp
                } else {
                    Stmt::While {
                        cond,
                        body: Box::new(body),
                    }
                }
            } else if matches!(body, Stmt::NoOp) {
                Stmt::NoOp
            } else {
                Stmt::While {
                    cond,
                    body: Box::new(body),
                }
            }
        }
        Stmt::Seq { first, second } => {
            let first = simplify_stmt(*first);
            let second = simplify_stmt(*second);
            match (&first, &second) {
                (Stmt::NoOp, _) => second,
                (_, Stmt::NoOp) => first,
                _ => Stmt::Seq {
                    first: Box::new(first),
                    second: Box::new(second),
                },
            }
        }
    }
}

fn simplify_assign_with_env(assign: Assign, env: &mut Vec<Option<i64>>) -> Assign {
    match assign {
        Assign::Local { dst, value } => {
            let value = simplify_expr_with_env(value, env);
            match const_eval(&value) {
                Some(val) => env[dst] = Some(val),
                None => env[dst] = None,
            }
            Assign::Local { dst, value }
        }
        Assign::Store {
            location,
            value,
            order,
        } => Assign::Store {
            location,
            value: simplify_expr_with_env(value, env),
            order,
        },
        Assign::Load {
            location,
            dst,
            order,
        } => {
            env[dst] = None;
            Assign::Load {
                location,
                dst,
                order,
            }
        }
    }
}

fn simplify_assign(assign: Assign) -> Assign {
    match assign {
        Assign::Local { dst, value } => Assign::Local {
            dst,
            value: simplify_expr(value),
        },
        Assign::Store {
            location,
            value,
            order,
        } => Assign::Store {
            location,
            value: simplify_expr(value),
            order,
        },
        Assign::Load {
            location,
            dst,
            order,
        } => Assign::Load {
            location,
            dst,
            order,
        },
    }
}

fn simplify_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Const(_) | Expr::Local(_) => expr,
        Expr::BinOp { op, left, right } => {
            let left = simplify_expr(*left);
            let right = simplify_expr(*right);
            simplify_binop(op, left, right)
        }
    }
}

fn simplify_expr_with_env(expr: Expr, env: &mut Vec<Option<i64>>) -> Expr {
    let expr = match expr {
        Expr::Local(id) => env.get(id).and_then(|v| *v).map(Expr::Const).unwrap_or(Expr::Local(id)),
        Expr::Const(_) => expr,
        Expr::BinOp { op, left, right } => {
            let left = simplify_expr_with_env(*left, env);
            let right = simplify_expr_with_env(*right, env);
            simplify_binop(op, left, right)
        }
    };
    expr
}

fn simplify_binop(op: BinOp, left: Expr, right: Expr) -> Expr {
    if let (Some(l), Some(r)) = (const_eval(&left), const_eval(&right)) {
        return Expr::Const(fold_binop(op, l, r));
    }
    match (op, const_eval(&left), const_eval(&right)) {
        (BinOp::Add, Some(0), _) => right,
        (BinOp::Add, _, Some(0)) => left,
        (BinOp::Sub, _, Some(0)) => left,
        (BinOp::Mul, Some(0), _) => Expr::Const(0),
        (BinOp::Mul, _, Some(0)) => Expr::Const(0),
        (BinOp::Mul, Some(1), _) => right,
        (BinOp::Mul, _, Some(1)) => left,
        (BinOp::Div, Some(0), _) => Expr::Const(0),
        (BinOp::Div, _, Some(1)) => left,
        (BinOp::Div, _, Some(0)) => Expr::Const(0),
        _ => Expr::BinOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    }
}

fn const_eval(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Const(value) => Some(*value),
        Expr::Local(_) => None,
        Expr::BinOp { op, left, right } => {
            let l = const_eval(left)?;
            let r = const_eval(right)?;
            Some(fold_binop(*op, l, r))
        }
    }
}

fn fold_binop(op: BinOp, left: i64, right: i64) -> i64 {
    match op {
        BinOp::Add => left.saturating_add(right),
        BinOp::Sub => left.saturating_sub(right),
        BinOp::Mul => left.saturating_mul(right),
        BinOp::Div => {
            if right == 0 {
                0
            } else {
                left / right
            }
        }
        BinOp::Lt => (left < right) as i64,
        BinOp::Le => (left <= right) as i64,
        BinOp::Gt => (left > right) as i64,
        BinOp::Ge => (left >= right) as i64,
        BinOp::Eq => (left == right) as i64,
    }
}

fn merge_env(left: &[Option<i64>], right: &[Option<i64>]) -> Vec<Option<i64>> {
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| match (l, r) {
            (Some(a), Some(b)) if a == b => Some(*a),
            _ => None,
        })
        .collect()
}

fn assigned_locals(stmt: &Stmt) -> HashSet<usize> {
    let mut assigned = HashSet::new();
    collect_assigned_locals(stmt, &mut assigned);
    assigned
}

fn collect_assigned_locals(stmt: &Stmt, assigned: &mut HashSet<usize>) {
    match stmt {
        Stmt::NoOp | Stmt::Assert(_) | Stmt::Assume(_) => {}
        Stmt::Assign(assign) => match assign {
            Assign::Local { dst, .. } => {
                assigned.insert(*dst);
            }
            Assign::Load { dst, .. } => {
                assigned.insert(*dst);
            }
            Assign::Store { .. } => {}
        },
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            collect_assigned_locals(then_branch, assigned);
            collect_assigned_locals(else_branch, assigned);
        }
        Stmt::While { body, .. } => {
            collect_assigned_locals(body, assigned);
        }
        Stmt::Seq { first, second } => {
            collect_assigned_locals(first, assigned);
            collect_assigned_locals(second, assigned);
        }
    }
}

fn eliminate_dead_locals(stmt: Stmt, local_count: usize) -> Stmt {
    let live_after = vec![false; local_count];
    let (stmt, _) = prune_dead_locals(stmt, &live_after);
    stmt
}

fn prune_dead_locals(stmt: Stmt, live_after: &[bool]) -> (Stmt, Vec<bool>) {
    match stmt {
        Stmt::NoOp => (Stmt::NoOp, live_after.to_vec()),
        Stmt::Assert(expr) => {
            let mut live = live_after.to_vec();
            collect_used_locals_expr(&expr, &mut live);
            (Stmt::Assert(expr), live)
        }
        Stmt::Assume(expr) => {
            let mut live = live_after.to_vec();
            collect_used_locals_expr(&expr, &mut live);
            (Stmt::Assume(expr), live)
        }
        Stmt::Assign(assign) => match assign {
            Assign::Local { dst, value } => {
                if !live_after.get(dst).copied().unwrap_or(false) {
                    return (Stmt::NoOp, live_after.to_vec());
                }
                let mut live = live_after.to_vec();
                live[dst] = false;
                collect_used_locals_expr(&value, &mut live);
                (Stmt::Assign(Assign::Local { dst, value }), live)
            }
            Assign::Load {
                location,
                dst,
                order,
            } => {
                let mut live = live_after.to_vec();
                if dst < live.len() {
                    live[dst] = false;
                }
                (
                    Stmt::Assign(Assign::Load {
                        location,
                        dst,
                        order,
                    }),
                    live,
                )
            }
            Assign::Store {
                location,
                value,
                order,
            } => {
                let mut live = live_after.to_vec();
                collect_used_locals_expr(&value, &mut live);
                (
                    Stmt::Assign(Assign::Store {
                        location,
                        value,
                        order,
                    }),
                    live,
                )
            }
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let (then_branch, live_then) = prune_dead_locals(*then_branch, live_after);
            let (else_branch, live_else) = prune_dead_locals(*else_branch, live_after);
            let mut live = merge_live(&live_then, &live_else);
            collect_used_locals_expr(&cond, &mut live);
            (
                Stmt::If {
                    cond,
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                live,
            )
        }
        Stmt::While { cond, body } => {
            let mut live_before = live_after.to_vec();
            collect_used_locals_expr(&cond, &mut live_before);
            collect_used_locals_stmt(&body, &mut live_before);
            let (body, _) = prune_dead_locals(*body, &live_before);
            (
                Stmt::While {
                    cond,
                    body: Box::new(body),
                },
                live_before,
            )
        }
        Stmt::Seq { first, second } => {
            let (second, live_mid) = prune_dead_locals(*second, live_after);
            let (first, live_before) = prune_dead_locals(*first, &live_mid);
            let stmt = match (&first, &second) {
                (Stmt::NoOp, _) => second,
                (_, Stmt::NoOp) => first,
                _ => Stmt::Seq {
                    first: Box::new(first),
                    second: Box::new(second),
                },
            };
            (stmt, live_before)
        }
    }
}

fn collect_used_locals_stmt(stmt: &Stmt, used: &mut Vec<bool>) {
    match stmt {
        Stmt::NoOp => {}
        Stmt::Assert(expr) | Stmt::Assume(expr) => collect_used_locals_expr(expr, used),
        Stmt::Assign(assign) => match assign {
            Assign::Local { value, .. } => collect_used_locals_expr(value, used),
            Assign::Store { value, .. } => collect_used_locals_expr(value, used),
            Assign::Load { .. } => {}
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_used_locals_expr(cond, used);
            collect_used_locals_stmt(then_branch, used);
            collect_used_locals_stmt(else_branch, used);
        }
        Stmt::While { cond, body } => {
            collect_used_locals_expr(cond, used);
            collect_used_locals_stmt(body, used);
        }
        Stmt::Seq { first, second } => {
            collect_used_locals_stmt(first, used);
            collect_used_locals_stmt(second, used);
        }
    }
}

fn collect_used_locals_expr(expr: &Expr, used: &mut Vec<bool>) {
    match expr {
        Expr::Local(id) => {
            if *id < used.len() {
                used[*id] = true;
            }
        }
        Expr::Const(_) => {}
        Expr::BinOp { left, right, .. } => {
            collect_used_locals_expr(left, used);
            collect_used_locals_expr(right, used);
        }
    }
}

fn merge_live(left: &[bool], right: &[bool]) -> Vec<bool> {
    left.iter()
        .zip(right.iter())
        .map(|(a, b)| *a || *b)
        .collect()
}

fn remove_unused_locals(thread: Thread) -> Thread {
    let used = collect_referenced_locals(&thread.stmt, thread.locals.len());
    if used.iter().all(|u| *u) {
        return thread;
    }
    let mut mapping = vec![None; used.len()];
    let mut new_locals = Vec::new();
    for (idx, name) in thread.locals.iter().enumerate() {
        if used.get(idx).copied().unwrap_or(false) {
            let new_id = new_locals.len();
            new_locals.push(name.clone());
            mapping[idx] = Some(new_id);
        }
    }
    let stmt = remap_stmt_locals(thread.stmt, &mapping);
    Thread {
        locals: new_locals,
        stmt,
    }
}

fn collect_referenced_locals(stmt: &Stmt, local_count: usize) -> Vec<bool> {
    let mut used = vec![false; local_count];
    collect_refs_stmt(stmt, &mut used);
    used
}

fn collect_refs_stmt(stmt: &Stmt, used: &mut Vec<bool>) {
    match stmt {
        Stmt::NoOp => {}
        Stmt::Assert(expr) | Stmt::Assume(expr) => collect_refs_expr(expr, used),
        Stmt::Assign(assign) => match assign {
            Assign::Local { dst, value } => {
                if *dst < used.len() {
                    used[*dst] = true;
                }
                collect_refs_expr(value, used);
            }
            Assign::Load { dst, .. } => {
                if *dst < used.len() {
                    used[*dst] = true;
                }
            }
            Assign::Store { value, .. } => collect_refs_expr(value, used),
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_refs_expr(cond, used);
            collect_refs_stmt(then_branch, used);
            collect_refs_stmt(else_branch, used);
        }
        Stmt::While { cond, body } => {
            collect_refs_expr(cond, used);
            collect_refs_stmt(body, used);
        }
        Stmt::Seq { first, second } => {
            collect_refs_stmt(first, used);
            collect_refs_stmt(second, used);
        }
    }
}

fn collect_refs_expr(expr: &Expr, used: &mut Vec<bool>) {
    match expr {
        Expr::Local(id) => {
            if *id < used.len() {
                used[*id] = true;
            }
        }
        Expr::Const(_) => {}
        Expr::BinOp { left, right, .. } => {
            collect_refs_expr(left, used);
            collect_refs_expr(right, used);
        }
    }
}

fn remap_stmt_locals(stmt: Stmt, mapping: &[Option<usize>]) -> Stmt {
    match stmt {
        Stmt::NoOp => Stmt::NoOp,
        Stmt::Assert(expr) => Stmt::Assert(remap_expr_locals(expr, mapping)),
        Stmt::Assume(expr) => Stmt::Assume(remap_expr_locals(expr, mapping)),
        Stmt::Assign(assign) => Stmt::Assign(remap_assign_locals(assign, mapping)),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => Stmt::If {
            cond: remap_expr_locals(cond, mapping),
            then_branch: Box::new(remap_stmt_locals(*then_branch, mapping)),
            else_branch: Box::new(remap_stmt_locals(*else_branch, mapping)),
        },
        Stmt::While { cond, body } => Stmt::While {
            cond: remap_expr_locals(cond, mapping),
            body: Box::new(remap_stmt_locals(*body, mapping)),
        },
        Stmt::Seq { first, second } => Stmt::Seq {
            first: Box::new(remap_stmt_locals(*first, mapping)),
            second: Box::new(remap_stmt_locals(*second, mapping)),
        },
    }
}

fn remap_assign_locals(assign: Assign, mapping: &[Option<usize>]) -> Assign {
    match assign {
        Assign::Local { dst, value } => Assign::Local {
            dst: mapping.get(dst).and_then(|v| *v).unwrap_or(dst),
            value: remap_expr_locals(value, mapping),
        },
        Assign::Store {
            location,
            value,
            order,
        } => Assign::Store {
            location,
            value: remap_expr_locals(value, mapping),
            order,
        },
        Assign::Load {
            location,
            dst,
            order,
        } => Assign::Load {
            location,
            dst: mapping.get(dst).and_then(|v| *v).unwrap_or(dst),
            order,
        },
    }
}

fn remap_expr_locals(expr: Expr, mapping: &[Option<usize>]) -> Expr {
    match expr {
        Expr::Local(id) => Expr::Local(mapping.get(id).and_then(|v| *v).unwrap_or(id)),
        Expr::Const(_) => expr,
        Expr::BinOp { op, left, right } => Expr::BinOp {
            op,
            left: Box::new(remap_expr_locals(*left, mapping)),
            right: Box::new(remap_expr_locals(*right, mapping)),
        },
    }
}

fn remove_unused_shared(shared: Vec<crate::model::SharedVar>, threads: Vec<Thread>) -> (Vec<crate::model::SharedVar>, Vec<Thread>) {
    let used = collect_used_shared(&threads, shared.len());
    if used.iter().all(|u| *u) {
        return (shared, threads);
    }
    let mut mapping = vec![None; used.len()];
    let mut new_shared = Vec::new();
    for (idx, var) in shared.into_iter().enumerate() {
        if used.get(idx).copied().unwrap_or(false) {
            let new_id = new_shared.len();
            new_shared.push(var);
            mapping[idx] = Some(new_id);
        }
    }
    let threads = threads
        .into_iter()
        .map(|thread| Thread {
            locals: thread.locals,
            stmt: remap_stmt_shared(thread.stmt, &mapping),
        })
        .collect();
    (new_shared, threads)
}

fn collect_used_shared(threads: &[Thread], shared_count: usize) -> Vec<bool> {
    let mut used = vec![false; shared_count];
    for thread in threads {
        collect_used_shared_stmt(&thread.stmt, &mut used);
    }
    used
}

fn collect_used_shared_stmt(stmt: &Stmt, used: &mut Vec<bool>) {
    match stmt {
        Stmt::NoOp => {}
        Stmt::Assert(expr) | Stmt::Assume(expr) => collect_used_shared_expr(expr, used),
        Stmt::Assign(assign) => match assign {
            Assign::Local { value, .. } => collect_used_shared_expr(value, used),
            Assign::Store { location, value, .. } => {
                if *location < used.len() {
                    used[*location] = true;
                }
                collect_used_shared_expr(value, used);
            }
            Assign::Load { location, .. } => {
                if *location < used.len() {
                    used[*location] = true;
                }
            }
        },
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_used_shared_expr(cond, used);
            collect_used_shared_stmt(then_branch, used);
            collect_used_shared_stmt(else_branch, used);
        }
        Stmt::While { cond, body } => {
            collect_used_shared_expr(cond, used);
            collect_used_shared_stmt(body, used);
        }
        Stmt::Seq { first, second } => {
            collect_used_shared_stmt(first, used);
            collect_used_shared_stmt(second, used);
        }
    }
}

fn collect_used_shared_expr(expr: &Expr, _used: &mut Vec<bool>) {
    match expr {
        Expr::Local(_) | Expr::Const(_) => {}
        Expr::BinOp { left, right, .. } => {
            collect_used_shared_expr(left, _used);
            collect_used_shared_expr(right, _used);
        }
    }
}

fn remap_stmt_shared(stmt: Stmt, mapping: &[Option<usize>]) -> Stmt {
    match stmt {
        Stmt::NoOp => Stmt::NoOp,
        Stmt::Assert(expr) => Stmt::Assert(remap_expr_shared(expr, mapping)),
        Stmt::Assume(expr) => Stmt::Assume(remap_expr_shared(expr, mapping)),
        Stmt::Assign(assign) => Stmt::Assign(remap_assign_shared(assign, mapping)),
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => Stmt::If {
            cond: remap_expr_shared(cond, mapping),
            then_branch: Box::new(remap_stmt_shared(*then_branch, mapping)),
            else_branch: Box::new(remap_stmt_shared(*else_branch, mapping)),
        },
        Stmt::While { cond, body } => Stmt::While {
            cond: remap_expr_shared(cond, mapping),
            body: Box::new(remap_stmt_shared(*body, mapping)),
        },
        Stmt::Seq { first, second } => Stmt::Seq {
            first: Box::new(remap_stmt_shared(*first, mapping)),
            second: Box::new(remap_stmt_shared(*second, mapping)),
        },
    }
}

fn remap_assign_shared(assign: Assign, mapping: &[Option<usize>]) -> Assign {
    match assign {
        Assign::Local { dst, value } => Assign::Local {
            dst,
            value: remap_expr_shared(value, mapping),
        },
        Assign::Store {
            location,
            value,
            order,
        } => Assign::Store {
            location: mapping.get(location).and_then(|v| *v).unwrap_or(location),
            value: remap_expr_shared(value, mapping),
            order,
        },
        Assign::Load {
            location,
            dst,
            order,
        } => Assign::Load {
            location: mapping.get(location).and_then(|v| *v).unwrap_or(location),
            dst,
            order,
        },
    }
}

fn remap_expr_shared(expr: Expr, mapping: &[Option<usize>]) -> Expr {
    match expr {
        Expr::Local(_) | Expr::Const(_) => expr,
        Expr::BinOp { op, left, right } => Expr::BinOp {
            op,
            left: Box::new(remap_expr_shared(*left, mapping)),
            right: Box::new(remap_expr_shared(*right, mapping)),
        },
    }
}
