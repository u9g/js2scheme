use std::fmt::Display;

#[derive(Debug)]
pub struct Statements(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Function(FunctionStatement),
    Variable(VariableStatement),
    Expression(Expression),
    // Printed verbatim.
    VerbatimString(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    FunctionCall(FunctionCall),
    Number(String),
    Lambda(Box<Lambda>),
    Boolean(bool),
    // Should be printed directly, not as string
    String(String),
}

impl Expression {
    pub fn function_call(name: impl AsRef<str>, parameters: Vec<Expression>) -> Self {
        Expression::FunctionCall(FunctionCall {
            name: name.as_ref().to_string(),
            arguments: Some(parameters),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub parameters: Vec<String>,
    pub will_return: Expression,
}

#[derive(Debug)]
pub struct FunctionStatement {
    pub name: String,
    pub parameters: Vec<String>,
    pub will_return: Expression,
}

#[derive(Debug)]
pub struct VariableStatement {
    pub name: String,
    pub init: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Option<Vec<Expression>>,
}

impl Display for Statements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.0 {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Function(func) => {
                write!(
                    f,
                    "(define ({}{}{}) {})",
                    func.name,
                    if func.parameters.is_empty() { "" } else { " " },
                    func.parameters.join(" "),
                    func.will_return
                )
            }
            Statement::Variable(var) => write!(f, "(define {} {})", var.name, var.init),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::VerbatimString(s) => write!(f, "{}", s),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::FunctionCall(func) => {
                write!(
                    f,
                    "({}{}{})",
                    func.name,
                    if func.name.is_empty() { "" } else { " " },
                    func.arguments
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Expression::Number(num) => write!(f, "{}", num),
            Expression::Lambda(lambda) => {
                write!(
                    f,
                    "(lambda ({}) {})",
                    lambda.parameters.join(" "),
                    lambda.will_return
                )
            }
            Expression::Boolean(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expression::String(s) => write!(f, "{}", s),
        }
    }
}
