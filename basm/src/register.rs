#[derive(Debug, Clone)]
pub enum NameRef {
    /// Ident
    Ident { name: String },
    /// Attribute access like `a.b`
    Attribute { value: Box<NameRef>, attr: String },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    AssignOpConst {
        target: NameRef,
        value: i32,
    },
    AssignOp {
        target: NameRef,
        source: NameRef,
        factor: i32,
    },
    Point {
        target: NameRef,
    },
    While {
        conditions: Vec<Stmt>,
        body: Vec<Stmt>,
    },
    OpenClose {
        open: Box<Stmt>,
        body: Vec<Stmt>,
        close: Box<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<NameDef>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct NameDef {
    pub name: String,
    pub typ: String,
}
