use std::fmt::Display;

#[derive(Debug)]
pub struct StatementsBuilder(pub Vec<Statement>);

#[derive(Debug)]
pub struct FinalizedStatements(pub Vec<Statement>);

fn patch_expression_with_destructed_array_parts(
    expr: &mut Expression,
    parts: &[DestructuredArrayPart],
) {
    let mut replace_expr_with_this = None;
    match expr {
        Expression::FunctionCall(ref mut func) => {
            let params = func.arguments.replace(vec![]).unwrap();
            let new_params = params
                .into_iter()
                .map(|mut x| {
                    patch_expression_with_destructed_array_parts(&mut x, parts);
                    x
                })
                .collect::<Vec<_>>();
            func.arguments.replace(new_params);
        }
        Expression::Lambda(ref mut lambda) => {
            patch_expression_with_destructed_array_parts(&mut lambda.will_return, parts);
        }
        Expression::String(ref str) => {
            for part in parts {
                if str.as_str() == part.identifier_name.as_str() {
                    match part.index {
                        DestructuredArrayPartIndex::Indexed(ix) => {
                            let mut base = Expression::String(part.base_array.clone());
                            if ix == 1 {
                                base = Expression::function_call("cadr", vec![base]);
                            } else {
                                for _ in 0..ix {
                                    base = Expression::function_call("cdr", vec![base]);
                                }
                                base = Expression::function_call("car", vec![base]);
                            }
                            replace_expr_with_this = Some(base);
                        }
                        DestructuredArrayPartIndex::Rest => {
                            let mut base = Expression::String(part.base_array.clone());
                            base = Expression::function_call("cdr", vec![base]);
                            replace_expr_with_this = Some(base);
                        }
                    }
                }
            }
        }
        Expression::Number(_) => {}
        Expression::Boolean(_) => {}
    };

    if let Some(new_expr) = replace_expr_with_this {
        *expr = new_expr;
    }
}

impl StatementsBuilder {
    pub fn finalize(mut self) -> FinalizedStatements {
        self.patch_destructed_arrays();
        FinalizedStatements(self.0)
    }

    fn patch_destructed_arrays(&mut self) {
        self.0.iter_mut().for_each(|stmt| {
            if let Statement::Function(ref mut fndecl) = stmt {
                patch_expression_with_destructed_array_parts(
                    &mut fndecl.will_return,
                    &fndecl.destructed_array_parts,
                );
            }
        });
    }
}

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
    pub destructed_array_parts: Vec<DestructuredArrayPart>,
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

#[derive(Debug)]
pub enum DestructuredArrayPartIndex {
    Indexed(usize),
    Rest,
}

#[derive(Debug)]
pub struct DestructuredArrayPart {
    pub index: DestructuredArrayPartIndex,
    pub identifier_name: String,
    pub base_array: String,
}

impl Display for FinalizedStatements {
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
