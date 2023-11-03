use miette::Diagnostic;
use oxc_allocator::Vec as BVec;
use oxc_ast::ast::{Expression, FunctionBody, Statement};
use oxc_span::Span;
use thiserror::Error;

#[derive(Debug)]
pub struct Scope<'borrow, 'ast> {
    pub components: Vec<ControlFlowComponent<'borrow, 'ast>>,
}

#[derive(Debug)]
pub enum ControlFlowComponent<'borrow, 'ast> {
    Return(ReturnComponent<'borrow, 'ast>),
    IfStatement(Box<IfStatementComponent<'borrow, 'ast>>),
    Scope(Scope<'borrow, 'ast>),
}

impl<'borrow, 'ast> ControlFlowComponent<'borrow, 'ast> {
    fn new_return(expr: &'borrow Expression<'ast>) -> Self {
        ControlFlowComponent::Return(ReturnComponent { expr })
    }

    fn new_if_stmt(
        test: &'borrow Expression<'ast>,
        consequent: ControlFlowComponent<'borrow, 'ast>,
    ) -> Self {
        ControlFlowComponent::IfStatement(Box::new(IfStatementComponent { test, consequent }))
    }

    fn new_scope(scope: Scope<'borrow, 'ast>) -> Self {
        ControlFlowComponent::Scope(scope)
    }
}

#[derive(Debug)]
pub struct ReturnComponent<'borrow, 'ast> {
    pub expr: &'borrow Expression<'ast>,
}

#[derive(Debug)]
pub struct IfStatementComponent<'borrow, 'ast> {
    pub test: &'borrow Expression<'ast>,
    pub consequent: ControlFlowComponent<'borrow, 'ast>,
}

#[derive(Error, Debug, Diagnostic)]
pub enum ControlFlowGraphError {
    #[error("Used return statement without argument.")]
    ReturnStatementWithoutArgument(
        #[source_code] String,
        #[label("Return with no returned value")] Span,
    ),
    #[error("All scopes must return.")]
    ScopeDoesNotUnconditionallyReturn(
        #[source_code] String,
        #[label("Scope that doesn't return")] Span,
    ),
}

fn make_scope_from_stmts<'borrow, 'ast>(
    stmts: &'borrow BVec<Statement<'ast>>,
    span: Span,
) -> Result<Scope<'borrow, 'ast>, ControlFlowGraphError> {
    let mut components = vec![];
    for statement in stmts {
        let component = cfg_component_from_stmt(statement)?;
        let matches = matches!(component, ControlFlowComponent::Return(_));
        components.push(component);
        if matches {
            return Ok(Scope { components });
        }
    }

    Err(ControlFlowGraphError::ScopeDoesNotUnconditionallyReturn(
        "".to_string(),
        span,
    ))
}

fn cfg_component_from_stmt<'a, 'b>(
    stmt: &'a Statement<'b>,
) -> Result<ControlFlowComponent<'a, 'b>, ControlFlowGraphError> {
    match stmt {
        Statement::ReturnStatement(ret_stmt) => {
            if let Some(returned_expr) = &ret_stmt.argument {
                Ok(ControlFlowComponent::new_return(returned_expr))
            } else {
                Err(ControlFlowGraphError::ReturnStatementWithoutArgument(
                    "".to_string(),
                    ret_stmt.span,
                ))
            }
        }
        Statement::BlockStatement(block) => Ok(ControlFlowComponent::new_scope(
            make_scope_from_stmts(&block.body, block.span)?,
        )),
        Statement::BreakStatement(_) => todo!(),
        Statement::ContinueStatement(_) => todo!(),
        Statement::DebuggerStatement(_) => todo!(),
        Statement::DoWhileStatement(_) => todo!(),
        Statement::EmptyStatement(_) => todo!(),
        Statement::ExpressionStatement(_) => todo!(),
        Statement::ForInStatement(_) => todo!(),
        Statement::ForOfStatement(_) => todo!(),
        Statement::ForStatement(_) => todo!(),
        Statement::IfStatement(if_stmt) => Ok(ControlFlowComponent::new_if_stmt(
            &if_stmt.test,
            cfg_component_from_stmt(&if_stmt.consequent)?,
        )),
        Statement::LabeledStatement(_) => todo!(),
        Statement::SwitchStatement(_) => todo!(),
        Statement::ThrowStatement(_) => todo!(),
        Statement::TryStatement(_) => todo!(),
        Statement::WhileStatement(_) => todo!(),
        Statement::WithStatement(_) => todo!(),
        Statement::ModuleDeclaration(_) => todo!(),
        Statement::Declaration(_) => todo!(),
    }
}

pub fn make_cfg<'borrow, 'ast>(
    fbody: &'borrow FunctionBody<'ast>,
) -> Result<Scope<'borrow, 'ast>, ControlFlowGraphError> {
    make_scope_from_stmts(&fbody.statements, fbody.span)
}
