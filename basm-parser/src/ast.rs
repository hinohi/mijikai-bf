#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    /// Number like `10`, `'0'`
    Number(i32),
    /// Char like `'0'`
    Char(u8),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Variable {
    /// Ident
    Ident(String),
    /// Ident set like `(a|b)`
    Set(Vec<Variable>),
    /// Attribute access like `a.b`
    Attribute { target: Box<Variable>, attr: String },
    /// Dereference like `*a`
    Deref(Box<Variable>),
    /// Index access like `a[1]`
    Index { target: Box<Variable>, index: i32 },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    Literal(Literal),
    Variable(Variable),
}

impl Literal {
    pub fn term(self) -> Term {
        Term::Literal(self)
    }
}

impl Variable {
    pub fn ident<S: Into<String>>(name: S) -> Variable {
        Variable::Ident(name.into())
    }

    pub fn attr<S: Into<String>>(self, attr: S) -> Variable {
        Variable::Attribute {
            target: Box::new(self),
            attr: attr.into(),
        }
    }

    pub fn dereference(self) -> Variable {
        Variable::Deref(Box::new(self))
    }

    pub fn index(self, index: i32) -> Variable {
        Variable::Index {
            target: Box::new(self),
            index,
        }
    }

    pub fn term(self) -> Term {
        Term::Variable(self)
    }

    pub fn expr(self) -> Expr {
        Expr::Variable(self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Variable reference
    Variable(Variable),
    /// Region like `{a += 1; b}`
    Region { body: Vec<Stmt>, ret: Box<Expr> },
}

impl Expr {
    pub fn stmt(self) -> Stmt {
        Stmt::Expr(self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    /// Expression
    Expr(Expr),
    /// Call function like `f(a, b)`
    Call { name: String, args: Vec<Variable> },
    /// Assign add like `a += b;`
    AssignAdd {
        target: Variable,
        value: Term,
        factor: i32,
    },
    /// Assign sub like `a -= b;`
    AssignSub {
        target: Variable,
        value: Term,
        factor: i32,
    },
    /// While statement like `while a { a += 1 }`
    While { condition: Expr, body: Vec<Stmt> },
    /// Bra-ket like `bra a { b += 1; c } ket (a|c);`
    Braket {
        bra: Variable,
        body: Vec<Stmt>,
        ret: Expr,
        ket: Variable,
    },
    /// Move like `move {a -> b; c -> d;}`
    Move(Vec<(Variable, Variable)>),
}

impl Stmt {
    pub fn call<S: Into<String>>(name: S, args: Vec<Variable>) -> Stmt {
        Stmt::Call {
            name: name.into(),
            args,
        }
    }
}

pub enum Def {
    /// Function definition
    FuncDef(FuncDef),
    /// Struct definition
    StructDef(StructDef),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<TypedDeclaration>,
    pub body: Vec<Stmt>,
}

impl FuncDef {
    pub fn new<S: Into<String>>(name: S, args: Vec<TypedDeclaration>, body: Vec<Stmt>) -> FuncDef {
        FuncDef {
            name: name.into(),
            args,
            body,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<TypedDeclaration>,
}

impl StructDef {
    pub fn new<S: Into<String>>(name: S, fields: Vec<TypedDeclaration>) -> StructDef {
        StructDef {
            name: name.into(),
            fields,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypedDeclaration {
    pub name: String,
    pub typ: Type,
}

impl TypedDeclaration {
    pub fn new<S: Into<String>>(name: S, typ: Type) -> TypedDeclaration {
        TypedDeclaration {
            name: name.into(),
            typ,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Value(String),
    Array(String),
}

impl Type {
    pub fn value<S: Into<String>>(name: S) -> Type {
        Type::Value(name.into())
    }

    pub fn array<S: Into<String>>(name: S) -> Type {
        Type::Array(name.into())
    }
}
