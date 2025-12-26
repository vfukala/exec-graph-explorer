use crate::model::{Assign, BinOp, Expr, MemoryOrder, Program, SharedVar, Stmt, Thread};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct Token {
    kind: TokenKind,
    line: usize,
    col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TokenKind {
    Ident(String),
    Number(i64),
    Keyword(Keyword),
    Assign(Option<String>),
    Op(Op),
    LBrace,
    RBrace,
    LParen,
    RParen,
    Comma,
    Semi,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Keyword {
    Shared,
    Thread,
    Local,
    If,
    Then,
    Else,
    While,
    Do,
    Assert,
    Assume,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
}

pub fn parse_program_from_file(path: &str) -> Result<Program, String> {
    let src = std::fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path, err))?;
    parse_program_from_str(&src)
}

pub fn parse_program_from_str(src: &str) -> Result<Program, String> {
    let tokens = Lexer::new(src).lex()?;
    Parser::new(tokens).parse_program()
}

struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            chars: src.char_indices().peekable(),
            line: 1,
            col: 1,
        }
    }

    fn lex(mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        while let Some((idx, ch)) = self.chars.peek().copied() {
            if ch.is_whitespace() {
                self.bump(ch);
                continue;
            }
            if ch == '/' {
                if let Some((_, next)) = self.peek_next_char() {
                    if next == '/' {
                        self.bump(ch);
                        self.bump(next);
                        self.skip_line_comment();
                        continue;
                    }
                }
            }
            if ch == '#' {
                self.bump(ch);
                self.skip_line_comment();
                continue;
            }
            if ch.is_ascii_alphabetic() || ch == '_' {
                let start_col = self.col;
                let ident = self.consume_while(|c| c.is_ascii_alphanumeric() || c == '_');
                let kind = match ident.as_str() {
                    "shared" => TokenKind::Keyword(Keyword::Shared),
                    "thread" => TokenKind::Keyword(Keyword::Thread),
                    "local" => TokenKind::Keyword(Keyword::Local),
                    "if" => TokenKind::Keyword(Keyword::If),
                    "then" => TokenKind::Keyword(Keyword::Then),
                    "else" => TokenKind::Keyword(Keyword::Else),
                    "while" => TokenKind::Keyword(Keyword::While),
                    "do" => TokenKind::Keyword(Keyword::Do),
                    "assert" => TokenKind::Keyword(Keyword::Assert),
                    "assume" => TokenKind::Keyword(Keyword::Assume),
                    _ => TokenKind::Ident(ident),
                };
                tokens.push(Token {
                    kind,
                    line: self.line,
                    col: start_col,
                });
                continue;
            }
            if ch.is_ascii_digit() {
                let start_col = self.col;
                let number = self.consume_while(|c| c.is_ascii_digit());
                let value = number.parse::<i64>().map_err(|err| {
                    format!("invalid number {} at {}:{}: {}", number, self.line, start_col, err)
                })?;
                tokens.push(Token {
                    kind: TokenKind::Number(value),
                    line: self.line,
                    col: start_col,
                });
                continue;
            }

            let start_col = self.col;
            match ch {
                '{' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::LBrace, start_col));
                }
                '}' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::RBrace, start_col));
                }
                '(' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::LParen, start_col));
                }
                ')' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::RParen, start_col));
                }
                ',' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Comma, start_col));
                }
                ';' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Semi, start_col));
                }
                '+' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Op(Op::Add), start_col));
                }
                '-' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Op(Op::Sub), start_col));
                }
                '*' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Op(Op::Mul), start_col));
                }
                '/' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Op(Op::Div), start_col));
                }
                '<' => {
                    self.bump(ch);
                    if self.consume_char('=') {
                        tokens.push(self.simple_token(TokenKind::Op(Op::Le), start_col));
                    } else {
                        tokens.push(self.simple_token(TokenKind::Op(Op::Lt), start_col));
                    }
                }
                '>' => {
                    self.bump(ch);
                    if self.consume_char('=') {
                        tokens.push(self.simple_token(TokenKind::Op(Op::Ge), start_col));
                    } else {
                        tokens.push(self.simple_token(TokenKind::Op(Op::Gt), start_col));
                    }
                }
                '=' => {
                    self.bump(ch);
                    tokens.push(self.simple_token(TokenKind::Op(Op::Eq), start_col));
                }
                ':' => {
                    self.bump(ch);
                    if !self.consume_char('=') {
                        return Err(format!("unexpected ':' at {}:{}", self.line, start_col));
                    }
                    let order = self.consume_while(|c| c.is_ascii_alphabetic());
                    let order = if order.is_empty() { None } else { Some(order) };
                    tokens.push(Token {
                        kind: TokenKind::Assign(order),
                        line: self.line,
                        col: start_col,
                    });
                }
                _ => {
                    return Err(format!(
                        "unexpected '{}' at {}:{} (byte {})",
                        ch, self.line, start_col, idx
                    ));
                }
            }
        }
        Ok(tokens)
    }

    fn simple_token(&self, kind: TokenKind, col: usize) -> Token {
        Token {
            kind,
            line: self.line,
            col,
        }
    }

    fn bump(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        let _ = self.chars.next();
    }

    fn peek_next_char(&mut self) -> Option<(usize, char)> {
        let mut iter = self.chars.clone();
        iter.next()?;
        iter.next()
    }

    fn skip_line_comment(&mut self) {
        while let Some((_, ch)) = self.chars.peek().copied() {
            self.bump(ch);
            if ch == '\n' {
                break;
            }
        }
    }

    fn consume_char(&mut self, expected: char) -> bool {
        if let Some((_, ch)) = self.chars.peek().copied() {
            if ch == expected {
                self.bump(ch);
                return true;
            }
        }
        false
    }

    fn consume_while<F>(&mut self, mut predicate: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut out = String::new();
        while let Some((_, ch)) = self.chars.peek().copied() {
            if !predicate(ch) {
                break;
            }
            self.bump(ch);
            out.push(ch);
        }
        out
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let shared = if self.peek_keyword(Keyword::Shared) {
            self.next_token();
            self.parse_shared_list()?
        } else {
            Vec::new()
        };
        let shared_map = shared
            .iter()
            .enumerate()
            .map(|(idx, var)| (var.name.clone(), idx))
            .collect::<HashMap<_, _>>();

        let mut threads = Vec::new();
        while !self.is_eof() {
            threads.push(self.parse_thread(&shared_map)?);
        }
        Ok(Program { shared, threads })
    }

    fn parse_shared_list(&mut self) -> Result<Vec<SharedVar>, String> {
        let mut shared = Vec::new();
        loop {
            let name = self.expect_ident()?;
            let init = if self.consume_op(Op::Eq)? {
                self.expect_number()?
            } else {
                0
            };
            shared.push(SharedVar { name, init });
            if self.consume_token(TokenKind::Comma)? {
                continue;
            }
            self.expect_token(TokenKind::Semi)?;
            break;
        }
        Ok(shared)
    }

    fn parse_thread(&mut self, shared: &HashMap<String, usize>) -> Result<Thread, String> {
        self.expect_keyword(Keyword::Thread)?;
        let _name = self.expect_ident()?;
        self.expect_token(TokenKind::LBrace)?;
        let mut locals = Vec::new();
        let mut local_map = HashMap::new();
        let mut stmts = Vec::new();
        while !self.consume_token(TokenKind::RBrace)? {
            if self.peek_keyword(Keyword::Local) {
                self.next_token();
                let names = self.parse_ident_list()?;
                for name in names {
                    if local_map.contains_key(&name) {
                        return Err(format!("duplicate local {}", name));
                    }
                    let id = locals.len();
                    local_map.insert(name.clone(), id);
                    locals.push(name);
                }
                self.expect_token(TokenKind::Semi)?;
                continue;
            }
            let stmt = self.parse_stmt(shared, &local_map)?;
            stmts.push(stmt);
            let _ = self.consume_token(TokenKind::Semi)?;
        }

        let stmt = seq_from_vec(stmts);
        Ok(Thread { locals, stmt })
    }

    fn parse_ident_list(&mut self) -> Result<Vec<String>, String> {
        let mut names = Vec::new();
        loop {
            names.push(self.expect_ident()?);
            if self.consume_token(TokenKind::Comma)? {
                continue;
            }
            break;
        }
        Ok(names)
    }

    fn parse_stmt(
        &mut self,
        shared: &HashMap<String, usize>,
        locals: &HashMap<String, usize>,
    ) -> Result<Stmt, String> {
        if self.peek_keyword(Keyword::If) {
            return self.parse_if(shared, locals);
        }
        if self.peek_keyword(Keyword::While) {
            return self.parse_while(shared, locals);
        }
        if self.peek_keyword(Keyword::Assert) {
            self.next_token();
            let expr = self.parse_expr(locals)?;
            return Ok(Stmt::Assert(expr));
        }
        if self.peek_keyword(Keyword::Assume) {
            self.next_token();
            let expr = self.parse_expr(locals)?;
            return Ok(Stmt::Assume(expr));
        }
        let name = self.expect_ident()?;
        let assign = self.expect_assign()?;
        let assign = match assign {
            Some(order) => Some(order),
            None => self.maybe_consume_order_ident(),
        };
        match assign {
            Some(order) => {
                let order = parse_order(&order)?;
                if let Some(&location) = shared.get(&name) {
                    let value = self.parse_expr(locals)?;
                    Ok(Stmt::Assign(Assign::Store {
                        location,
                        value,
                        order,
                    }))
                } else if let Some(&dst) = locals.get(&name) {
                    let shared_name = self.expect_ident()?;
                    let Some(&location) = shared.get(&shared_name) else {
                        return Err(format!(
                            "unknown shared '{}' for load",
                            shared_name
                        ));
                    };
                    Ok(Stmt::Assign(Assign::Load {
                        location,
                        dst,
                        order,
                    }))
                } else {
                    Err(format!("unknown variable '{}'", name))
                }
            }
            None => {
                let Some(&dst) = locals.get(&name) else {
                    return Err(format!("unknown local '{}'", name));
                };
                let value = self.parse_expr(locals)?;
                Ok(Stmt::Assign(Assign::Local { dst, value }))
            }
        }
    }

    fn parse_if(
        &mut self,
        shared: &HashMap<String, usize>,
        locals: &HashMap<String, usize>,
    ) -> Result<Stmt, String> {
        self.expect_keyword(Keyword::If)?;
        let cond = self.parse_expr(locals)?;
        self.expect_keyword(Keyword::Then)?;
        let then_branch = self.parse_block(shared, locals)?;
        let else_branch = if self.peek_keyword(Keyword::Else) {
            self.next_token();
            self.parse_block(shared, locals)?
        } else {
            Stmt::NoOp
        };
        Ok(Stmt::If {
            cond,
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_while(
        &mut self,
        shared: &HashMap<String, usize>,
        locals: &HashMap<String, usize>,
    ) -> Result<Stmt, String> {
        self.expect_keyword(Keyword::While)?;
        let cond = self.parse_expr(locals)?;
        self.expect_keyword(Keyword::Do)?;
        let body = self.parse_block(shared, locals)?;
        Ok(Stmt::While {
            cond,
            body: Box::new(body),
        })
    }

    fn parse_block(
        &mut self,
        shared: &HashMap<String, usize>,
        locals: &HashMap<String, usize>,
    ) -> Result<Stmt, String> {
        self.expect_token(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while !self.consume_token(TokenKind::RBrace)? {
            let stmt = self.parse_stmt(shared, locals)?;
            stmts.push(stmt);
            let _ = self.consume_token(TokenKind::Semi)?;
        }
        Ok(seq_from_vec(stmts))
    }

    fn parse_expr(&mut self, locals: &HashMap<String, usize>) -> Result<Expr, String> {
        self.parse_cmp(locals)
    }

    fn parse_cmp(&mut self, locals: &HashMap<String, usize>) -> Result<Expr, String> {
        let mut expr = self.parse_add(locals)?;
        while let Some(op) = self.peek_op_cmp() {
            self.next_token();
            let right = self.parse_add(locals)?;
            let binop = match op {
                Op::Lt => BinOp::Lt,
                Op::Le => BinOp::Le,
                Op::Gt => BinOp::Gt,
                Op::Ge => BinOp::Ge,
                Op::Eq => BinOp::Eq,
                _ => unreachable!(),
            };
            expr = Expr::BinOp {
                op: binop,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_add(&mut self, locals: &HashMap<String, usize>) -> Result<Expr, String> {
        let mut expr = self.parse_mul(locals)?;
        loop {
            let op = match self.peek_op() {
                Some(Op::Add) => BinOp::Add,
                Some(Op::Sub) => BinOp::Sub,
                _ => break,
            };
            self.next_token();
            let right = self.parse_mul(locals)?;
            expr = Expr::BinOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_mul(&mut self, locals: &HashMap<String, usize>) -> Result<Expr, String> {
        let mut expr = self.parse_unary(locals)?;
        loop {
            let op = match self.peek_op() {
                Some(Op::Mul) => BinOp::Mul,
                Some(Op::Div) => BinOp::Div,
                _ => break,
            };
            self.next_token();
            let right = self.parse_unary(locals)?;
            expr = Expr::BinOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self, locals: &HashMap<String, usize>) -> Result<Expr, String> {
        if self.consume_op(Op::Sub)? {
            let expr = self.parse_unary(locals)?;
            return Ok(Expr::BinOp {
                op: BinOp::Sub,
                left: Box::new(Expr::Const(0)),
                right: Box::new(expr),
            });
        }
        if self.consume_token(TokenKind::LParen)? {
            let expr = self.parse_expr(locals)?;
            self.expect_token(TokenKind::RParen)?;
            return Ok(expr);
        }
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Number(value),
                ..
            }) => Ok(Expr::Const(value)),
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) => {
                let Some(&id) = locals.get(&name) else {
                    return Err(format!("unknown local '{}'", name));
                };
                Ok(Expr::Local(id))
            }
            Some(token) => Err(self.err_at(&token, "expected expression")),
            None => Err("unexpected end of input while parsing expression".to_string()),
        }
    }

    fn peek_keyword(&self, kw: Keyword) -> bool {
        matches!(
            self.tokens.get(self.index),
            Some(Token {
                kind: TokenKind::Keyword(k),
                ..
            }) if *k == kw
        )
    }

    fn peek_op(&self) -> Option<Op> {
        match self.tokens.get(self.index) {
            Some(Token {
                kind: TokenKind::Op(op),
                ..
            }) => Some(*op),
            _ => None,
        }
    }

    fn peek_op_cmp(&self) -> Option<Op> {
        match self.peek_op() {
            Some(Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq) => self.peek_op(),
            _ => None,
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<(), String> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Keyword(found),
                ..
            }) if found == kw => Ok(()),
            Some(token) => Err(self.err_at(&token, "unexpected token")),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) => Ok(name),
            Some(token) => Err(self.err_at(&token, "expected identifier")),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn expect_number(&mut self) -> Result<i64, String> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Number(value),
                ..
            }) => Ok(value),
            Some(token) => Err(self.err_at(&token, "expected number")),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn expect_assign(&mut self) -> Result<Option<String>, String> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Assign(order),
                ..
            }) => Ok(order),
            Some(token) => Err(self.err_at(&token, "expected assignment operator")),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn maybe_consume_order_ident(&mut self) -> Option<String> {
        match self.tokens.get(self.index) {
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) if is_order_ident(name) => {
                let name = name.clone();
                self.index += 1;
                Some(name)
            }
            _ => None,
        }
    }

    fn consume_op(&mut self, op: Op) -> Result<bool, String> {
        match self.tokens.get(self.index) {
            Some(Token {
                kind: TokenKind::Op(found),
                ..
            }) if *found == op => {
                self.index += 1;
                Ok(true)
            }
            Some(_) => Ok(false),
            None => Ok(false),
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<(), String> {
        match self.next_token() {
            Some(token) if token.kind == kind => Ok(()),
            Some(token) => Err(self.err_at(&token, "unexpected token")),
            None => Err("unexpected end of input".to_string()),
        }
    }

    fn consume_token(&mut self, kind: TokenKind) -> Result<bool, String> {
        match self.tokens.get(self.index) {
            Some(token) if token.kind == kind => {
                self.index += 1;
                Ok(true)
            }
            Some(_) => Ok(false),
            None => Ok(false),
        }
    }

    fn is_eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn next_token(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }
        token
    }

    fn err_at(&self, token: &Token, msg: &str) -> String {
        format!("{} at {}:{}", msg, token.line, token.col)
    }
}

fn parse_order(order: &str) -> Result<MemoryOrder, String> {
    match order {
        "rl" | "aq" | "ra" => Ok(MemoryOrder::RelaxedAtomic),
        "na" => Ok(MemoryOrder::NonAtomic),
        _ => Err(format!("unknown memory order '{}'", order)),
    }
}

fn is_order_ident(ident: &str) -> bool {
    matches!(ident, "rl" | "aq" | "ra" | "na")
}

fn seq_from_vec(stmts: Vec<Stmt>) -> Stmt {
    let mut iter = stmts.into_iter();
    let Some(mut acc) = iter.next() else {
        return Stmt::NoOp;
    };
    for stmt in iter {
        acc = Stmt::Seq {
            first: Box::new(acc),
            second: Box::new(stmt),
        };
    }
    acc
}
