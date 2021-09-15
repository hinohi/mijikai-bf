#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    /// Number like `10`, `'0'`
    Number(i32),
    /// Ident
    Ident(String),
    /// Ident set like `(a|b)`
    Set(Vec<Term>),
    /// Attribute access like `a.b`
    Attribute { target: Box<Term>, attr: String },
    /// Dereference like `*a`
    Deref(Box<Term>),
    /// Index access like `a[1]`
    Index { target: Box<Term>, index: i32 },
}

impl Term {
    pub fn ident<S: Into<String>>(name: S) -> Term {
        Term::Ident(name.into())
    }

    pub fn expr(self) -> Expr {
        Expr::Term(self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Term
    Term(Term),
    /// Region like `{a += 1; b}`
    Region { body: Vec<Stmt>, ret: Box<Expr> },
    /// Call function like `f(a, b)`
    Call { name: String, args: Vec<Term> },
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
    /// Assign add like `a += b;`
    AssignAdd {
        target: Box<Term>,
        value: Box<Expr>,
        factor: i32,
    },
    /// Assign sub like `a -= b;`
    AssignSub {
        target: Box<Term>,
        value: Box<Expr>,
        factor: i32,
    },
    /// While statement like `while a { b += 1 }`
    While {
        condition: Box<Expr>,
        body: Vec<Stmt>,
    },
    /// Bra-ket like `bra a { b += 1; c } ket c;`
    Braket {
        bra: Box<Term>,
        body: Vec<Stmt>,
        ret: Box<Expr>,
        ket: Box<Term>,
    },
    /// Move like `move {a -> b; c -> d;}`
    Move(Vec<(Term, Term)>),
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
