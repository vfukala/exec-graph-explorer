#[derive(Clone, Debug)]
pub struct Program {
    pub shared: Vec<SharedVar>,
    pub threads: Vec<Thread>,
}

#[derive(Clone, Debug)]
pub struct SharedVar {
    pub name: String,
    pub init: i64,
}

#[derive(Clone, Debug)]
pub struct Thread {
    pub locals: Vec<String>,
    pub stmt: Stmt,
}

pub type SharedVarId = usize;
pub type LocalVarId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryOrder {
    RelaxedAtomic,
    NonAtomic,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    NoOp,
    If {
        cond: Expr,
        then_branch: Box<Stmt>,
        else_branch: Box<Stmt>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    Seq {
        first: Box<Stmt>,
        second: Box<Stmt>,
    },
    Assign(Assign),
    Assert(Expr),
    Assume(Expr),
}

#[derive(Clone, Debug)]
pub enum Assign {
    Store {
        location: SharedVarId,
        value: Expr,
        order: MemoryOrder,
    },
    Load {
        location: SharedVarId,
        dst: LocalVarId,
        order: MemoryOrder,
    },
    Local {
        dst: LocalVarId,
        value: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Local(LocalVarId),
    Const(i64),
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
