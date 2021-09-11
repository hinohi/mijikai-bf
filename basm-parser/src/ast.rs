#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    /// Number like `10`, `'0'`
    Number { value: i32 },
    /// Ident
    Ident { name: String },
    /// Ident set like `(a|b)`
    Set { values: Vec<Term> },
    /// Attribute access like `a.b`
    Attribute { value: Box<Term>, attr: String },
    /// Dereference like `*a`
    Deref { target: Box<Term> },
    /// Index access like `a[1]`
    Index { target: Box<Term>, index: i32 },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    /// Term
    Term { term: Term },
    /// Region like `{a += 1; b}`
    Region { body: Vec<Expr> },
    /// Call function like `f(a, b)`
    Call { name: String, args: Vec<Expr> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    /// Assign add like `a += b`
    AssignAdd {
        target: Box<Term>,
        value: Box<Expr>,
        factor: i32,
    },
    /// Assign sub like `a -= b`
    AssignSub {
        target: Box<Term>,
        value: Box<Expr>,
        factor: i32,
    },
    /// While statement like `while a { b += 1 }`
    While {
        condition: Box<Expr>,
        body: Vec<Expr>,
    },
    /// Bra-ket like `bra a { b += 1; c } ket c;`
    Braket {
        bra: Box<Expr>,
        body: Vec<Expr>,
        ket: Box<Expr>,
    },
    /// Function definition
    FuncDef(FuncDef),
    /// Struct definition
    StructDef(StructDef),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<TypedDeclaration>,
    pub body: Vec<Expr>,
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
