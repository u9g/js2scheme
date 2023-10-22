use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, BindingPatternKind, Declaration, Expression,
        FormalParameters, Statement,
    },
    AstKind,
};
use oxc_semantic::Semantic;
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

fn transform_formal_params(formal_params: &FormalParameters) -> String {
    if formal_params.items.is_empty() {
        String::new()
    } else {
        let params = formal_params.items.iter().filter_map(|formal_param| {
            let BindingPatternKind::BindingIdentifier(name) = &formal_param.pattern.kind else {
                return None;
            };
            Some(name.name.to_string())
        });
        params.collect::<Vec<String>>().join(" ")
    }
}

fn transform_expr(expr: &Expression) -> String {
    match expr {
        Expression::BinaryExpression(expr) => {
            if expr.operator.is_equality()
                && matches!(&expr.right, Expression::NumberLiteral(nlit) if nlit.value == 0.)
            {
                if let Expression::BinaryExpression(binexpr) = &expr.left {
                    if let Expression::NumberLiteral(nlit) = &binexpr.right {
                        if nlit.raw == "2" {
                            return format!("(even? {})", transform_expr(&binexpr.left));
                        }
                    }
                }
            }
            format!(
                "({} {} {})",
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
                transform_expr(&expr.left),
                transform_expr(&expr.right)
            )
        }
        Expression::ConditionalExpression(cond_expr) => {
            format!(
                "(if {} {} {})",
                transform_expr(&cond_expr.test),
                transform_expr(&cond_expr.consequent),
                transform_expr(&cond_expr.alternate)
            )
        }
        Expression::UnaryExpression(unexpr) => format!(
            "({} {})",
            match unexpr.operator {
                UnaryOperator::UnaryNegation => "-",
                _ => todo!(),
            },
            transform_expr(&unexpr.argument)
        ),
        Expression::CallExpression(call_expr) => format!(
            "({} {})",
            transform_expr(&call_expr.callee),
            call_expr
                .arguments
                .iter()
                .map(|arg| transform_expr(match arg {
                    Argument::Expression(expr) => expr,
                    _ => todo!(),
                }))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Expression::ArrayExpression(array_expr) => format!(
            "(list {})",
            array_expr
                .elements
                .iter()
                .map(|elem| transform_expr(match elem {
                    ArrayExpressionElement::Expression(expr) => expr,
                    _ => todo!(),
                }))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Expression::ArrowExpression(arrow_expr) => {
            assert!(arrow_expr.expression); // don't support non expression arrow expressions
            format!(
                "(lambda ({}) {})",
                transform_formal_params(&arrow_expr.params),
                transform_expr(match &arrow_expr.body.statements.iter().next().unwrap() {
                    Statement::ExpressionStatement(expr_stmt) => &expr_stmt.expression,
                    _ => todo!(),
                })
            )
        }
        Expression::LogicalExpression(logical_expr) => format!(
            "({} {} {})",
            match logical_expr.operator {
                LogicalOperator::And => "and",
                LogicalOperator::Or => "or",
                _ => todo!(),
            },
            transform_expr(&logical_expr.left),
            transform_expr(&logical_expr.right)
        ),
        Expression::BooleanLiteral(blit) => match blit.value {
            true => "#t".to_string(),
            false => "#f".to_string(),
        },
        Expression::StringLiteral(slit) => format!("{}", slit.value),
        Expression::ParenthesizedExpression(paren_expr) => transform_expr(&paren_expr.expression),
        Expression::NumberLiteral(nlit) => nlit.value.to_string(),
        Expression::Identifier(ident) => ident.name.to_string(),
        _ => todo!("{expr:#?}"),
    }
}

pub fn transform(semantic: Semantic<'_>, should_print_hashtag_racket: bool) -> String {
    let mut lines = vec![];
    if should_print_hashtag_racket {
        lines.push("#lang racket".to_string());
    }
    match semantic.nodes().iter().next().unwrap().kind() {
        AstKind::Program(prog) => {
            for stmt in &prog.body {
                match stmt {
                    Statement::Declaration(decl) => match decl {
                        Declaration::FunctionDeclaration(fndecl) => {
                            // let mut lines = vec![];
                            lines.push(format!(
                                "(define ({} {}) {})",
                                fndecl.id.as_ref().unwrap().name,
                                transform_formal_params(&fndecl.params),
                                transform_expr(
                                    fndecl
                                        .body
                                        .as_ref()
                                        .unwrap()
                                        .statements
                                        .iter()
                                        .find_map(|stmt| {
                                            let Statement::ReturnStatement(ret) = stmt else {
                                                return None;
                                            };
                                            ret.argument.as_ref()
                                        })
                                        .unwrap()
                                )
                            ));
                        }
                        Declaration::VariableDeclaration(variable_decl) => {
                            let decl = variable_decl.declarations.iter().next().unwrap();
                            lines.push(format!(
                                "(define {} {})",
                                match &decl.id.kind {
                                    BindingPatternKind::BindingIdentifier(id) =>
                                        id.name.to_string(),
                                    _ => todo!(),
                                },
                                transform_expr(decl.init.as_ref().unwrap())
                            ));
                        }
                        _ => todo!(),
                    },
                    Statement::ExpressionStatement(expr) => {
                        if let Expression::TemplateLiteral(lit) = &expr.expression {
                            lines.push(format!("{}", lit.quasi().unwrap()));
                        } else {
                            lines.push(transform_expr(&expr.expression));
                        }
                    }
                    _ => todo!(),
                }
            }
        }
        _ => todo!(),
    }
    lines.join("\n")
}
