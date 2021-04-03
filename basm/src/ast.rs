#[derive(Debug, Clone)]
pub enum Ast {
    /// Number like `10`, `'0'`
    Number { value: u8 },
    /// ident
    Ident { name: String },
    /// Set like `(a|b)`
    Set { values: Vec<Ast> },
    /// Attribute access like `a.b`
    Attribute { value: Box<Ast>, attr: String },
    /// Assign like `a = b`
    Assign { target: Box<Ast>, value: Box<Ast> },
    /// Assign add like `a += b`
    AssignAdd { target: Box<Ast>, value: Box<Ast> },
    /// Assign sub like `a -= b`
    AssignSub { target: Box<Ast>, value: Box<Ast> },
    /// Add like `a + b`
    Add { left: Box<Ast>, right: Box<Ast> },
    /// Subtraction like `a - b`
    Sub { left: Box<Ast>, right: Box<Ast> },
    /// Multiplication like `a * 3`
    Mul { left: Box<Ast>, right: u8 },
    /// Dereference like `*a`
    Deref { target: Box<Ast> },
    /// Reference like `&a`
    Ref { target: Box<Ast> },
    /// Call function like `f a b`
    Call { name: String, args: Vec<Ast> },
    /// While statement like `while a { b += 1 }`
    While { condition: Vec<Ast>, body: Vec<Ast> },
    /// Open-Close like `a [ b += 1; c ] c`
    OpenClose {
        open: Box<Ast>,
        close: Box<Ast>,
        body: Vec<Ast>,
    },
    /// Function definition
    FuncDef {
        name: String,
        args: Vec<NameDef>,
        input_conditions: Vec<Ast2>,
        output_conditions: Vec<Ast2>,
    },
    /// Struct definition
    StructDef { name: String, fields: Vec<NameDef> },
}

#[derive(Debug, Clone)]
pub struct NameDef {
    pub name: String,
    pub typ: String,
}

#[derive(Debug, Clone)]
pub enum Ast2 {
    /// Number like `10`, `'0'`
    Number { value: u8 },
    /// ident
    Ident { name: String },
    /// Reference like `&a`
    Ref { target: Box<Ast2> },
    /// Add like `a + b`
    Add { left: Box<Ast2>, right: Box<Ast2> },
    /// Subtraction like `a - b`
    Sub { left: Box<Ast2>, right: Box<Ast2> },
    /// Multiplication like `a * b`
    Mul { left: Box<Ast2>, right: Box<Ast2> },
    /// Division like `a / b`
    Div { left: Box<Ast2>, right: Box<Ast2> },
    /// mod like `a % b`
    Mod { left: Box<Ast2>, right: Box<Ast2> },
    /// Equal like `a == b`
    Eq { left: Box<Ast2>, right: Box<Ast2> },
    /// Not Equal like `a != b`
    Neq { left: Box<Ast2>, right: Box<Ast2> },
    /// Grater than like `a > b`
    Gt { left: Box<Ast2>, right: Box<Ast2> },
    /// Less than like `a < b`
    Lt { left: Box<Ast2>, right: Box<Ast2> },
    /// Grater than or equal like `a >= b`
    Ge { left: Box<Ast2>, right: Box<Ast2> },
    /// Less than or equal like `a <= b`
    Le { left: Box<Ast2>, right: Box<Ast2> },
}
