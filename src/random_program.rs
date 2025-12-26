use crate::model::{Assign, BinOp, Expr, MemoryOrder, Program, SharedVar, Stmt, Thread};
use crate::optimizer::optimize_program;
use rand::Rng;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RandomOptions {
    pub allow_assume: bool,
    pub allow_assert: bool,
    pub allow_non_atomic: bool,
}

impl Default for RandomOptions {
    fn default() -> Self {
        Self {
            allow_assume: true,
            allow_assert: true,
            allow_non_atomic: true,
        }
    }
}

pub fn random_program() -> Program {
    random_program_with_options(RandomOptions::default())
}

pub fn random_program_with_options(options: RandomOptions) -> Program {
    let mut rng = rand::thread_rng();
    let shared_count = rng.gen_range(1..=3);
    let thread_count = rng.gen_range(2..=4);

    let shared = (0..shared_count)
        .map(|i| SharedVar {
            name: format!("shared{}", i),
            init: rng.gen_range(0..=3),
        })
        .collect();

    let threads = (0..thread_count)
        .map(|i| random_thread(&mut rng, i, shared_count, options))
        .collect();

    optimize_program(Program { shared, threads })
}

fn random_thread(
    rng: &mut impl Rng,
    index: usize,
    shared_count: usize,
    options: RandomOptions,
) -> Thread {
    let local_count = rng.gen_range(1..=3);
    let locals = (0..local_count).map(|i| format!("l{}_{}", index, i)).collect();
    let stmt_count = rng.gen_range(3..=7);
    let mut stmts = Vec::new();
    for _ in 0..stmt_count {
        stmts.push(random_stmt(rng, local_count, shared_count, options));
    }
    let stmt = chain_seq(stmts);
    Thread { locals, stmt }
}

fn random_stmt(
    rng: &mut impl Rng,
    local_count: usize,
    shared_count: usize,
    options: RandomOptions,
) -> Stmt {
    let mut choices = Vec::new();
    choices.push((50, "assign"));
    choices.push((20, "if"));
    if options.allow_assert {
        choices.push((10, "assert"));
    }
    if options.allow_assume {
        choices.push((10, "assume"));
    }
    choices.push((10, "noop"));

    let total: u32 = choices.iter().map(|(w, _)| *w).sum();
    let mut roll = rng.gen_range(0..total);
    let mut pick = "noop";
    for (weight, name) in choices {
        if roll < weight {
            pick = name;
            break;
        }
        roll -= weight;
    }

    match pick {
        "assign" => Stmt::Assign(random_assign(rng, local_count, shared_count, options)),
        "if" => {
            let cond = random_expr(rng, local_count);
            let then_branch = Box::new(random_stmt(rng, local_count, shared_count, options));
            let else_branch = Box::new(random_stmt(rng, local_count, shared_count, options));
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            }
        }
        "assert" => Stmt::Assert(random_expr(rng, local_count)),
        "assume" => Stmt::Assume(random_expr(rng, local_count)),
        _ => Stmt::NoOp,
    }
}

fn random_assign(
    rng: &mut impl Rng,
    local_count: usize,
    shared_count: usize,
    options: RandomOptions,
) -> Assign {
    let roll = rng.gen_range(0..100);
    let order = if options.allow_non_atomic {
        if rng.gen_bool(0.5) {
            MemoryOrder::RelaxedAtomic
        } else {
            MemoryOrder::NonAtomic
        }
    } else {
        MemoryOrder::RelaxedAtomic
    };
    if roll < 40 {
        Assign::Local {
            dst: rng.gen_range(0..local_count),
            value: random_expr(rng, local_count),
        }
    } else if roll < 70 {
        Assign::Load {
            location: rng.gen_range(0..shared_count),
            dst: rng.gen_range(0..local_count),
            order,
        }
    } else {
        Assign::Store {
            location: rng.gen_range(0..shared_count),
            value: random_expr(rng, local_count),
            order,
        }
    }
}

fn random_expr(rng: &mut impl Rng, local_count: usize) -> Expr {
    let roll = rng.gen_range(0..100);
    match roll {
        0..=39 => Expr::Const(rng.gen_range(0..=5)),
        40..=69 => Expr::Local(rng.gen_range(0..local_count)),
        _ => {
            let op = match rng.gen_range(0..4) {
                0 => BinOp::Add,
                1 => BinOp::Sub,
                2 => BinOp::Mul,
                _ => BinOp::Div,
            };
            let left = Box::new(random_expr(rng, local_count));
            let right = if matches!(op, BinOp::Div) {
                Box::new(Expr::Const(rng.gen_range(1..=5)))
            } else {
                Box::new(random_expr(rng, local_count))
            };
            Expr::BinOp { op, left, right }
        }
    }
}

fn chain_seq(mut stmts: Vec<Stmt>) -> Stmt {
    if stmts.is_empty() {
        return Stmt::NoOp;
    }
    let mut current = stmts.remove(0);
    for stmt in stmts {
        current = Stmt::Seq {
            first: Box::new(current),
            second: Box::new(stmt),
        };
    }
    current
}
