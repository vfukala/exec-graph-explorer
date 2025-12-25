use crate::model::{Assign, BinOp, Expr, Program, Stmt, Thread};

pub fn optimize_program(program: Program) -> Program {
    let threads = program
        .threads
        .into_iter()
        .map(|thread| Thread {
            locals: thread.locals,
            stmt: simplify_stmt(thread.stmt),
        })
        .collect();
    Program {
        shared: program.shared,
        threads,
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
            match (const_eval(&left), const_eval(&right)) {
                (Some(l), Some(r)) => Expr::Const(fold_binop(op, l, r)),
                _ => Expr::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            }
        }
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
    }
}
