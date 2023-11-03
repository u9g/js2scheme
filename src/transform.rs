use std::iter::Peekable;

use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, BindingPatternKind, Declaration, Expression,
        FormalParameters, FunctionBody, Statement,
    },
    AstKind,
};
use oxc_semantic::Semantic;
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

use crate::{
    cfg::{make_cfg, ControlFlowComponent, Scope, SingleVariableDeclaration},
    ir::{
        Expression as IRExpression, FunctionStatement, Lambda, Statement as IRStatement,
        Statements, VariableStatement,
    },
};

fn transform_formal_params(formal_params: &FormalParameters) -> Vec<String> {
    if formal_params.items.is_empty() {
        vec![]
    } else {
        let mut param_names = Vec::with_capacity(formal_params.items.len());
        for param in &formal_params.items {
            let BindingPatternKind::BindingIdentifier(param_identifier) = &param.pattern.kind else {unimplemented!()};
            param_names.push(param_identifier.name.to_string());
        }
        param_names
    }
}

fn transform_expr(expr: &Expression) -> IRExpression {
    match expr {
        Expression::BinaryExpression(expr) => {
            if expr.operator.is_equality()
                && matches!(&expr.right, Expression::NumberLiteral(nlit) if nlit.value == 0.)
            {
                if let Expression::BinaryExpression(binexpr) = &expr.left {
                    if let Expression::NumberLiteral(nlit) = &binexpr.right {
                        if nlit.raw == "2" {
                            return IRExpression::function_call(
                                "even?",
                                vec![transform_expr(&binexpr.left)],
                            );
                        }
                    }
                }
            }
            IRExpression::function_call(
                match expr.operator {
                    BinaryOperator::Multiplication => "*",
                    BinaryOperator::Addition => "+",
                    BinaryOperator::Subtraction => "-",
                    BinaryOperator::Division => "/",
                    BinaryOperator::LessEqualThan => "<=",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::GreaterEqualThan => ">=",
                    BinaryOperator::StrictEquality => "=",
                    BinaryOperator::StrictInequality => "!=",
                    _ => todo!(),
                },
                vec![transform_expr(&expr.left), transform_expr(&expr.right)],
            )
        }
        Expression::ConditionalExpression(cond_expr) => IRExpression::function_call(
            "if",
            vec![
                transform_expr(&cond_expr.test),
                transform_expr(&cond_expr.consequent),
                transform_expr(&cond_expr.alternate),
            ],
        ),
        Expression::UnaryExpression(unexpr) => IRExpression::function_call(
            match unexpr.operator {
                UnaryOperator::UnaryNegation => "-",
                _ => unimplemented!(),
            },
            vec![transform_expr(&unexpr.argument)],
        ),
        Expression::CallExpression(call_expr) => IRExpression::function_call(
            match &call_expr.callee {
                Expression::Identifier(ident) => &ident.name,
                Expression::StringLiteral(slit) => &slit.value,
                _ => unimplemented!(),
            },
            call_expr
                .arguments
                .iter()
                .map(|arg| {
                    transform_expr(match arg {
                        Argument::Expression(expr) => expr,
                        _ => unimplemented!(),
                    })
                })
                .collect::<Vec<_>>(),
        ),
        Expression::ArrayExpression(array_expr) => IRExpression::function_call(
            "list",
            array_expr
                .elements
                .iter()
                .map(|elem| {
                    transform_expr(match elem {
                        ArrayExpressionElement::Expression(expr) => expr,
                        _ => todo!(),
                    })
                })
                .collect::<Vec<_>>(),
        ),
        Expression::ArrowExpression(arrow_expr) => {
            assert!(arrow_expr.expression); // don't support non expression arrow expressions
            IRExpression::Lambda(Box::new(Lambda {
                parameters: transform_formal_params(&arrow_expr.params),
                will_return: transform_expr(
                    match &arrow_expr.body.statements.iter().next().unwrap() {
                        // TODO: support function statements which should be converted to lambdas here, maybe?
                        Statement::ExpressionStatement(expr_stmt) => &expr_stmt.expression,
                        _ => todo!(),
                    },
                ),
            }))
        }
        Expression::LogicalExpression(logical_expr) => IRExpression::function_call(
            match logical_expr.operator {
                LogicalOperator::And => "and",
                LogicalOperator::Or => "or",
                _ => todo!(),
            },
            vec![
                transform_expr(&logical_expr.left),
                transform_expr(&logical_expr.right),
            ],
        ),
        Expression::BooleanLiteral(blit) => IRExpression::Boolean(blit.value),
        Expression::StringLiteral(slit) => IRExpression::String(slit.value.to_string()),
        Expression::ParenthesizedExpression(paren_expr) => transform_expr(&paren_expr.expression),
        Expression::NumberLiteral(nlit) => IRExpression::Number(nlit.value.to_string()),
        Expression::Identifier(ident) => IRExpression::String(ident.name.to_string()),
        _ => todo!("{expr:#?}"),
    }
}

enum IfStatementCondition<'borrow, 'ast> {
    Condition(&'borrow Expression<'ast>),
    Else,
}

fn control_flow_to_expr(s: &ControlFlowComponent) -> IRExpression {
    match s {
        ControlFlowComponent::Return(ret) => transform_expr(ret.expr),
        ControlFlowComponent::IfStatement(_) => unimplemented!(),
        ControlFlowComponent::VariableDeclaration(_) => unimplemented!(), //can only happen if the if statement is just a variable declaration
        ControlFlowComponent::Scope(s) => scope_to_expression(s),
    }
}

// impl Iterator<Item = &'iter_item ControlFlowComponent<'borrow, 'ast>>

fn component_iterator_to_expr<'iter_item, 'borrow: 'iter_item, 'ast: 'borrow, I>(
    comps: Peekable<I>,
) -> IRExpression
where
    I: Iterator<Item = &'iter_item ControlFlowComponent<'borrow, 'ast>>,
{
    let mut iter = comps;
    let mut arguments = vec![];
    while iter.peek().is_some() {
        let comp = iter
            .next()
            .expect("our while loop tells us there is something here");
        match comp {
            ControlFlowComponent::Return(ret) => {
                arguments.push((IfStatementCondition::Else, transform_expr(ret.expr)))
            }
            ControlFlowComponent::IfStatement(ifs) => arguments.push((
                IfStatementCondition::Condition(ifs.test),
                control_flow_to_expr(&ifs.consequent),
            )),
            ControlFlowComponent::Scope(s) => {
                arguments.push((IfStatementCondition::Else, scope_to_expression(s)))
            }
            ControlFlowComponent::VariableDeclaration(vdecl) => {
                // slurp up all variable declarations into the same letrec block
                let mut vdecls = vec![vdecl];
                while let Some(ControlFlowComponent::VariableDeclaration(_)) = iter.peek() {
                    let Some(ControlFlowComponent::VariableDeclaration(vdecl)) =
                        iter.next() else {unreachable!("verified by while cond")};
                    vdecls.push(vdecl);
                }

                let mut arguments_to_let = vec![];

                for variable in vdecls.iter().flat_map(|it| &it.variables) {
                    match variable {
                        SingleVariableDeclaration::PrimitiveDeclaration(primitive_var) => {
                            arguments_to_let.push(IRExpression::function_call(
                                "",
                                vec![
                                    IRExpression::String(primitive_var.name.clone()),
                                    transform_expr(primitive_var.value),
                                ],
                            ));
                        }
                        SingleVariableDeclaration::ArrayIndexedDeclaration(aid) => {
                            let mut base = transform_expr(aid.array);
                            for _ in 0..aid.element_index {
                                base = IRExpression::function_call("cdr", vec![base])
                            }
                            arguments_to_let.push(IRExpression::function_call(
                                "",
                                vec![
                                    IRExpression::String(aid.name.clone()),
                                    IRExpression::function_call("car", vec![base]),
                                ],
                            ))
                        }
                    }
                }

                arguments.push((
                    IfStatementCondition::Else,
                    IRExpression::function_call(
                        "letrec",
                        vec![
                            IRExpression::function_call("", arguments_to_let),
                            component_iterator_to_expr(iter),
                        ],
                    ),
                ));
                break;
            }
        }
    }

    let mut iter = arguments.into_iter().peekable();

    // Don't generate cond for just a return.
    if let Some((IfStatementCondition::Else, _)) = iter.peek() {
        return iter.next().unwrap().1;
    }

    let mut branches = vec![];

    for condition_branch in iter {
        if let IfStatementCondition::Condition(cond) = condition_branch.0 {
            branches.push(IRExpression::function_call(
                "",
                vec![transform_expr(cond), condition_branch.1],
            ));
        } else {
            branches.push(IRExpression::function_call(
                "",
                vec![IRExpression::String("else".to_string()), condition_branch.1],
            ));
            break;
        };
    }

    IRExpression::function_call("cond", branches)
}

fn scope_to_expression(s: &Scope) -> IRExpression {
    component_iterator_to_expr(s.components.iter().peekable())
}

fn walk_cfg_to_transform(fn_body: &FunctionBody) -> IRExpression {
    let cfg = make_cfg(fn_body).unwrap();

    scope_to_expression(&cfg)
}

pub fn transform(semantic: Semantic<'_>) -> Statements {
    let mut stmts = vec![];
    match semantic.nodes().iter().next().unwrap().kind() {
        AstKind::Program(prog) => {
            for stmt in &prog.body {
                match stmt {
                    Statement::Declaration(decl) => match decl {
                        Declaration::FunctionDeclaration(fndecl) => {
                            stmts.push(IRStatement::Function(FunctionStatement {
                                name: fndecl.id.as_ref().unwrap().name.to_string(),
                                parameters: transform_formal_params(&fndecl.params),
                                will_return: walk_cfg_to_transform(fndecl.body.as_ref().unwrap()),
                            }))
                        }
                        Declaration::VariableDeclaration(variable_decl) => {
                            let decl = variable_decl.declarations.iter().next().unwrap();
                            stmts.push(IRStatement::Variable(VariableStatement {
                                name: match &decl.id.kind {
                                    BindingPatternKind::BindingIdentifier(id) => {
                                        id.name.to_string()
                                    }
                                    _ => todo!(),
                                },
                                init: transform_expr(decl.init.as_ref().unwrap()),
                            }))
                        }
                        _ => todo!(),
                    },
                    Statement::ExpressionStatement(expr) => {
                        if let Expression::TemplateLiteral(lit) = &expr.expression {
                            stmts.push(IRStatement::VerbatimString(
                                lit.quasi().unwrap().to_string(),
                            ));
                        } else {
                            stmts.push(IRStatement::Expression(transform_expr(&expr.expression)));
                        }
                    }
                    _ => todo!(),
                }
            }
        }
        _ => todo!(),
    }
    Statements(stmts)
}
