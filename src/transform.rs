use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, BindingPatternKind, Declaration, Expression,
        FormalParameters, FunctionBody, Statement,
    },
    AstKind,
};
use oxc_semantic::Semantic;
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

use crate::ir::{
    DestructuredArrayPart, DestructuredArrayPartIndex, Expression as IRExpression,
    FunctionStatement, Lambda, Statement as IRStatement, StatementsBuilder, VariableStatement,
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

fn expression_as_identifier(expression: &Expression) -> Option<String> {
    match expression {
        Expression::StringLiteral(slit) => Some(slit.value.to_string()),
        Expression::Identifier(ident) => Some(ident.name.to_string()),
        _ => None,
    }
}

fn parse_fn_body(fn_body: &FunctionBody) -> (Vec<DestructuredArrayPart>, IRExpression) {
    let mut destructured_arrays = vec![];
    let mut return_statement = None;
    for statement in &fn_body.statements {
        match statement {
            Statement::Declaration(decl) => match decl {
                Declaration::VariableDeclaration(var_decl) => {
                    for variable_declaration in &var_decl.declarations {
                        let array_name =
                            expression_as_identifier(variable_declaration.init.as_ref().unwrap())
                                .unwrap();
                        match &variable_declaration.id.kind {
                            BindingPatternKind::ArrayPattern(array) => {
                                for (ix, array_part) in array.elements.iter().enumerate() {
                                    match array_part.as_ref().map(|x| &x.kind) {
                                        Some(BindingPatternKind::BindingIdentifier(identifier)) => {
                                            destructured_arrays.push(DestructuredArrayPart {
                                                index: DestructuredArrayPartIndex::Indexed(ix),
                                                identifier_name: identifier.name.to_string(),
                                                base_array: array_name.clone(),
                                            });
                                        }
                                        None => {}
                                        _ => unimplemented!(),
                                    }
                                }
                                if let Some(rest) = &array.rest {
                                    match &rest.argument.kind {
                                        BindingPatternKind::BindingIdentifier(identifier) => {
                                            destructured_arrays.push(DestructuredArrayPart {
                                                index: DestructuredArrayPartIndex::Rest,
                                                identifier_name: identifier.name.to_string(),
                                                base_array: array_name.clone(),
                                            });
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
                _ => unimplemented!(),
            },
            Statement::ReturnStatement(found_return_stmt) => {
                return_statement = Some(found_return_stmt)
            }
            _ => unimplemented!(),
        }
    }
    (
        destructured_arrays,
        transform_expr(return_statement.unwrap().argument.as_ref().unwrap()),
    )
}

pub fn transform(semantic: Semantic<'_>) -> StatementsBuilder {
    let mut stmts = vec![];
    match semantic.nodes().iter().next().unwrap().kind() {
        AstKind::Program(prog) => {
            for stmt in &prog.body {
                match stmt {
                    Statement::Declaration(decl) => match decl {
                        Declaration::FunctionDeclaration(fndecl) => {
                            let (destructed_array_parts, will_return) =
                                parse_fn_body(fndecl.body.as_ref().unwrap());
                            stmts.push(IRStatement::Function(FunctionStatement {
                                name: fndecl.id.as_ref().unwrap().name.to_string(),
                                parameters: transform_formal_params(&fndecl.params),
                                will_return,
                                destructed_array_parts,
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
    StatementsBuilder(stmts)
}
