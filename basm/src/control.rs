#[derive(Debug, Clone)]
pub enum Ast {
    /// Number like `10`, `'0'`
    Number { value: i32 },
    /// ident
    Ident { name: String },
    /// Reference like `&a`
    Ref { target: Box<Ast> },
    /// Add like `a + b`
    Add { left: Box<Ast>, right: Box<Ast> },
    /// Subtraction like `a - b`
    Sub { left: Box<Ast>, right: Box<Ast> },
    /// Multiplication like `a * b`
    Mul { left: Box<Ast>, right: Box<Ast> },
    /// Division like `a / b`
    Div { left: Box<Ast>, right: Box<Ast> },
    /// mod like `a % b`
    Mod { left: Box<Ast>, right: Box<Ast> },
    /// Equal like `a == b`
    Eq { left: Box<Ast>, right: Box<Ast> },
    /// Not Equal like `a != b`
    Neq { left: Box<Ast>, right: Box<Ast> },
    /// Grater than like `a > b`
    Gt { left: Box<Ast>, right: Box<Ast> },
    /// Less than like `a < b`
    Lt { left: Box<Ast>, right: Box<Ast> },
    /// Grater than or equal like `a >= b`
    Ge { left: Box<Ast>, right: Box<Ast> },
    /// Less than or equal like `a <= b`
    Le { left: Box<Ast>, right: Box<Ast> },
}
