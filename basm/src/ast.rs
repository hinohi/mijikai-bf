use crate::control;

#[derive(Debug, Clone)]
pub enum Ast {
    /// Number like `10`, `'0'`
    Number { value: i32 },
    /// Ident
    Ident { name: String },
    /// Ident set like `(a|b)`
    Set { values: Vec<Ast> },
    /// Attribute access like `a.b`
    Attribute { value: Box<Ast>, attr: String },
    /// Assign add like `a += b`
    AssignAdd { target: Box<Ast>, value: Box<Ast> },
    /// Assign sub like `a -= b`
    AssignSub { target: Box<Ast>, value: Box<Ast> },
    /// Add like `a + b`
    Add { left: Box<Ast>, right: Box<Ast> },
    /// Subtraction like `a - b`
    Sub { left: Box<Ast>, right: Box<Ast> },
    /// Multiplication like `a * 3`
    Mul { left: Box<Ast>, right: i32 },
    /// Dereference like `*a`
    Deref { target: Box<Ast> },
    /// Reference like `&a`
    Ref { target: Box<Ast> },
    /// Call function like `f a b`
    Call { name: String, args: Vec<Ast> },
    /// While statement like `while a { b += 1 }`
    While {
        conditions: Vec<Ast>,
        body: Vec<Ast>,
    },
    /// Open-Close like `a [ b += 1; c ] c`
    OpenClose {
        open: Box<Ast>,
        body: Vec<Ast>,
        close: Box<Ast>,
    },
    /// Function definition
    FuncDef {
        name: String,
        args: Vec<NameDef>,
        input_conditions: Vec<control::Ast>,
        output_conditions: Vec<control::Ast>,
        body: Vec<Ast>,
    },
    /// Struct definition
    StructDef { name: String, fields: Vec<NameDef> },
}

#[derive(Debug, Clone)]
pub struct NameDef {
    pub name: String,
    pub typ: String,
    pub reference: bool,
}
