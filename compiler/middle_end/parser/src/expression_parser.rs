use crate::input::ParserInput;
use crate::misc_parsers::{
    datatype_parser, identifier_parser, identifier_with_type_parameter_parser, token_parser,
};
use crate::{map, unspan_vec, ParserSpan};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, MethodCall, NewEnum, NewStruct,
    StructFieldAccess, Typecast, UnaryOp, UnaryOpType,
};
use ast::{ASTNode, UntypedAST};
use chumsky::extra::Full;
use chumsky::prelude::*;
use lexer::TokenType;
use source::types::{BytePos, Span};

/// Parses an expression
pub(crate) fn expression_parser<'src>() -> impl Parser<
    'src,
    ParserInput<'src>,
    ASTNode<Expression<UntypedAST>>,
    Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> + Clone {
    recursive(|expr| {
        let literal = choice((
            select_ref! {
                TokenType::Decimal(inner) => {
                            if inner.fract() == 0.0 {
                                format!("{:.1}", inner)
                            } else {
                                inner.to_string()
                            }
                        }
            }
            .spanned(),
            select_ref! {
                TokenType::Integer(inner) => inner.to_string(),
            }
            .spanned(),
            select_ref! {
                TokenType::CharLiteral(inner) => format!("'{}'", inner),
            }
            .spanned(),
            token_parser(TokenType::True).map(|input| map(input, |_| "true".to_string())),
            token_parser(TokenType::False).map(|input| map(input, |_| "false".to_string())),
        ))
        .map(|lit| map(lit, |lit| Expression::<UntypedAST>::Literal(lit)))
        .map(|lit| ASTNode::new(lit.inner, lit.span.0));
        let ident = identifier_parser();
        let ident_with_typ_param = identifier_with_type_parameter_parser();

        let function_call = ident_with_typ_param
            .clone()
            .then(
                expr.clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<ASTNode<Expression<UntypedAST>>>>()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    ),
            )
            .map(|((name, type_parameters), args)| {
                let pos = name
                    .span
                    .merge(
                        args.last()
                            .map(|to_map| to_map.position().clone().into())
                            .unwrap_or(name.span),
                    )
                    .unwrap();
                let type_parameters = type_parameters
                    .into_iter()
                    .map(|type_param| type_param.inner)
                    .collect::<Vec<_>>();
                ASTNode::new(
                    Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
                        (name.inner, type_parameters),
                        args,
                    )),
                    pos.0,
                )
            });

        let new_struct = token_parser(TokenType::New)
            .then(ident_with_typ_param.clone())
            .then_ignore(token_parser(TokenType::OpenScope))
            .then(
                identifier_parser()
                    .then_ignore(token_parser(TokenType::Assign))
                    .then(expr.clone())
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<_>>(),
            )
            .then(token_parser(TokenType::CloseScope))
            .map(|(((start, (name, type_parameters)), field_values), end)| {
                let pos = start.span.merge(end.span).unwrap();
                let fields = field_values
                    .into_iter()
                    .map(|field| (ASTNode::new(field.0.inner, field.0.span.into()), field.1))
                    .collect::<Vec<_>>();
                let type_parameters = type_parameters
                    .into_iter()
                    .map(|type_param| type_param.inner)
                    .collect::<Vec<_>>();
                ASTNode::new(
                    Expression::NewStruct(Box::new(NewStruct::new(
                        (name.inner, type_parameters),
                        fields,
                    ))),
                    pos.0,
                )
            });

        let new_enum = ident_with_typ_param
            .then_ignore(token_parser(TokenType::PathSeparator))
            .then(identifier_parser())
            .then(
                expr.clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    )
                    .or_not(),
            )
            .map(|(((name, type_parameters), variant_name), field_values)| {
                let pos = name
                    .span
                    .merge(
                        field_values
                            .as_ref()
                            .and_then(|fv| {
                                fv.last().map(|last_val| last_val.position().clone().into())
                            })
                            .unwrap_or(variant_name.span),
                    )
                    .unwrap();
                let type_parameters = unspan_vec(type_parameters);
                ASTNode::new(
                    Expression::NewEnum(Box::new(NewEnum::<UntypedAST>::new(
                        (name.inner, type_parameters),
                        variant_name.inner,
                        field_values.unwrap_or(Vec::new()),
                    ))),
                    pos.0,
                )
            });

        let base = choice((
            function_call,
            literal,
            // New struct / enum is before variable to prevent is being parsed as a variable
            new_struct,
            new_enum,
            ident.map(|input| ASTNode::new(Expression::Variable(input.inner), input.span.into())),
            expr.clone().delimited_by(
                token_parser(TokenType::OpenParen),
                token_parser(TokenType::CloseParen),
            ),
        ));

        let method_call = base
            .clone()
            .then_ignore(token_parser(TokenType::Dot))
            .then(identifier_with_type_parameter_parser())
            .then_ignore(token_parser(TokenType::OpenParen))
            .then(
                expr.clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<ASTNode<Expression<UntypedAST>>>>(),
            )
            .then(token_parser(TokenType::CloseParen))
            .map(|(((source, method), args), end)| {
                let pos = source.position().merge(end.span.0).unwrap();
                ASTNode::new(
                    Expression::<UntypedAST>::MethodCall(Box::new(MethodCall::new(
                        source,
                        (
                            method.0.inner,
                            method
                                .1
                                .into_iter()
                                .map(|type_param| type_param.inner)
                                .collect(),
                        ),
                        args,
                    ))),
                    pos,
                )
            });

        let sfa =
            base.clone()
                .then_ignore(token_parser(TokenType::Dot))
                .then(identifier_parser())
                .map(|(struct_expr, field)| {
                    let pos = struct_expr.position().merge(field.span.into()).unwrap();
                    ASTNode::new(
                        Expression::StructFieldAccess(Box::new(
                            StructFieldAccess::<UntypedAST>::new(struct_expr, field.inner),
                        )),
                        pos,
                    )
                });

        let simple_combined = choice((method_call, sfa, base));

        let typecast = simple_combined.foldl(
            token_parser(TokenType::As)
                .ignored()
                .then(datatype_parser())
                .repeated(),
            |expr, (_, new_type)| {
                let new_pos = expr.position().merge(new_type.span.into()).unwrap();
                ASTNode::new(
                    Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(new_type.inner)),
                        expr,
                    ))),
                    new_pos,
                )
            },
        );

        let unary_op = choice((
            token_parser(TokenType::Subtraction)
                .map(|token| unary_op_mapper(UnaryOpType::Negative, token.span.start())),
            token_parser(TokenType::Not)
                .map(|token| unary_op_mapper(UnaryOpType::Not, token.span.start())),
        ));
        let unary = unary_op.repeated().foldr(typecast, |op, rhs| op(rhs));

        let product = binary_operator_from_token_parser(
            unary,
            &[
                (TokenType::Multiplication, BinaryOpType::Multiplication),
                (TokenType::Slash, BinaryOpType::Division),
                (TokenType::Modulo, BinaryOpType::Modulo),
            ],
        )
        .boxed();

        let sum = binary_operator_from_token_parser(
            product,
            &[
                (TokenType::Addition, BinaryOpType::Addition),
                (TokenType::Subtraction, BinaryOpType::Subtraction),
            ],
        );

        let bitshift = binary_op_parser(
            sum,
            vec![
                (
                    token_parser(TokenType::LessThan).repeated().exactly(2),
                    BinaryOpType::LeftShift,
                ),
                (
                    token_parser(TokenType::GreaterThan).repeated().exactly(2),
                    BinaryOpType::RightShift,
                ),
            ]
            .into_iter(),
        )
        .boxed();

        let comparison = binary_operator_from_token_parser(
            bitshift,
            &[
                (TokenType::LessThan, BinaryOpType::Lesser),
                (TokenType::LessThanEqual, BinaryOpType::LesserEquals),
                (TokenType::GreaterThanEqual, BinaryOpType::GreaterEquals),
                (TokenType::GreaterThan, BinaryOpType::Greater),
            ],
        );

        let equals = binary_operator_from_token_parser(
            comparison,
            &[
                (TokenType::Comparison, BinaryOpType::Equals),
                (TokenType::NotEqual, BinaryOpType::NotEquals),
            ],
        )
        .boxed();

        let bitand = binary_operator_from_token_parser(
            equals,
            &[(TokenType::BitAnd, BinaryOpType::BitwiseAnd)],
        );

        let bitor = binary_operator_from_token_parser(
            bitand,
            &[(TokenType::BitOr, BinaryOpType::BitwiseOr)],
        )
        .boxed();

        let and = binary_operator_from_token_parser(bitor, &[(TokenType::And, BinaryOpType::And)]);

        binary_operator_from_token_parser(and, &[(TokenType::Or, BinaryOpType::Or)]).boxed()
    })
}

/// Parses binary ops
///
/// # Parameter
///
/// **ops**: The tokens to parse and what to parse them to
fn binary_operator_from_token_parser<'src>(
    input: impl Parser<
        'src,
        ParserInput<'src>,
        ASTNode<Expression<UntypedAST>>,
        Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
    > + Clone,
    ops: &[(TokenType, BinaryOpType)],
) -> impl Parser<
    'src,
    ParserInput<'src>,
    ASTNode<Expression<UntypedAST>>,
    Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> + Clone {
    let ops = ops.iter().map(|op| (token_parser(op.0.clone()), op.1));
    binary_op_parser(input, ops)
}

/// `Ignored` can be arbitrary
fn binary_op_parser<
    'src,
    Ignored,
    OpParser: Parser<'src, ParserInput<'src>, Ignored, Full<Rich<'src, TokenType, ParserSpan>, (), ()>>
        + Clone,
    Ops: Iterator<Item = (OpParser, BinaryOpType)>,
>(
    input: impl Parser<
        'src,
        ParserInput<'src>,
        ASTNode<Expression<UntypedAST>>,
        Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
    > + Clone,
    ops: Ops,
) -> impl Parser<
    'src,
    ParserInput<'src>,
    ASTNode<Expression<UntypedAST>>,
    Full<Rich<'src, TokenType, ParserSpan>, (), ()>,
> + Clone {
    input.clone().foldl(
        choice(
            ops.map(|(token, op)| token.map(move |_| binary_op_mapper(op)))
                .collect::<Vec<_>>(),
        )
        .then(input)
        .repeated(),
        |lhs, (op, rhs)| op(lhs, rhs),
    )
}

fn binary_op_mapper(
    token_type: BinaryOpType,
) -> impl Fn(
    ASTNode<Expression<UntypedAST>>,
    ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    move |lhs, rhs| map_binary_op(token_type, lhs, rhs)
}

fn map_binary_op(
    operator_type: BinaryOpType,
    lhs: ASTNode<Expression<UntypedAST>>,
    rhs: ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    let combined_pos = lhs
        .position()
        .merge(*rhs.position())
        .expect("This should never happen. lhs should always be before rhs");
    ASTNode::new(
        Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
            operator_type,
            lhs,
            rhs,
        ))),
        combined_pos,
    )
}

fn unary_op_mapper(
    token_type: UnaryOpType<UntypedAST>,
    op_start: BytePos,
) -> impl Fn(ASTNode<Expression<UntypedAST>>) -> ASTNode<Expression<UntypedAST>> {
    move |expr| map_unary_op(token_type.clone(), op_start, expr)
}

fn map_unary_op(
    operator_type: UnaryOpType<UntypedAST>,
    op_start: BytePos,
    input: ASTNode<Expression<UntypedAST>>,
) -> ASTNode<Expression<UntypedAST>> {
    let combined_pos = Span {
        start: op_start,
        end: input.position().end(),
        file_id: input.position().file_id,
    };
    ASTNode::new(
        Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
            operator_type.clone(),
            input,
        ))),
        combined_pos,
    )
}

#[cfg(test)]
mod tests {
    use crate::expression_parser::expression_parser;
    use crate::test_shared::{convert_nonempty_input, wrap_in_ast_node, wrap_token};
    use ast::data_type::UntypedDataType;
    use ast::expression::{
        BinaryOp, BinaryOpType, Expression, FunctionCall, Typecast, UnaryOp, UnaryOpType,
    };
    use ast::{SemanticEq, UntypedAST};
    use chumsky::Parser;
    use lexer::TokenType;

    #[test]
    fn parse() {
        let to_parse = [
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen, //TokenType::Integer(10), TokenType::As, TokenType::Identifier("f32".to_string())
        ]
        .map(wrap_token);

        let parser = expression_parser();

        let parsed = parser.parse(convert_nonempty_input(&to_parse)).unwrap();
        let expected = wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            ("test".to_string(), Vec::new()),
            vec![
                wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                        "f32".to_string(),
                        Vec::new(),
                    ))),
                    wrap_in_ast_node(Expression::Literal("5".to_string())),
                )))),
                wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_ast_node(Expression::Variable("test2".to_string())),
                    wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_ast_node(Expression::Literal("5.0".to_string())),
                        wrap_in_ast_node(Expression::Literal("10.0".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_eq(&expected));
    }

    #[test]
    fn parse_parens() {
        let to_parse = [
            TokenType::OpenParen,
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen,
            TokenType::CloseParen, //TokenType::Integer(10), TokenType::As, TokenType::Identifier("f32".to_string())
        ]
        .map(wrap_token);

        let parser = expression_parser();

        let parsed = parser.parse(convert_nonempty_input(&to_parse)).unwrap();
        let expected = wrap_in_ast_node(Expression::FunctionCall(FunctionCall::<UntypedAST>::new(
            ("test".to_string(), Vec::new()),
            vec![
                wrap_in_ast_node(Expression::UnaryOp(Box::new(UnaryOp::<UntypedAST>::new(
                    UnaryOpType::Typecast(Typecast::new(UntypedDataType::new(
                        "f32".to_string(),
                        Vec::new(),
                    ))),
                    wrap_in_ast_node(Expression::Literal("5".to_string())),
                )))),
                wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                    BinaryOpType::NotEquals,
                    wrap_in_ast_node(Expression::Variable("test2".to_string())),
                    wrap_in_ast_node(Expression::BinaryOp(Box::new(BinaryOp::<UntypedAST>::new(
                        BinaryOpType::Multiplication,
                        wrap_in_ast_node(Expression::Literal("5.0".to_string())),
                        wrap_in_ast_node(Expression::Literal("10.0".to_string())),
                    )))),
                )))),
            ],
        )));
        assert!(parsed.semantic_eq(&expected));
    }

    #[test]
    fn parse_just_identifier() {
        let to_parse = [TokenType::Identifier("test".to_string())].map(wrap_token);

        let parser = expression_parser();

        let expected = wrap_in_ast_node(Expression::Variable("test".to_string()));
        assert!(expected.semantic_eq(&parser.parse(convert_nonempty_input(&to_parse)).unwrap()));
    }

    #[test]
    fn parse_invalid() {
        let to_parse = [
            TokenType::Bool,
            TokenType::Identifier("var".to_string()),
            TokenType::Assign,
            TokenType::Identifier("test".to_string()),
            TokenType::OpenParen,
            TokenType::Integer(5),
            TokenType::As,
            TokenType::F32,
            TokenType::ArgumentSeparator,
            TokenType::Identifier("test2".to_string()),
            TokenType::NotEqual,
            TokenType::Decimal(5.0),
            TokenType::Multiplication,
            TokenType::Decimal(10.0),
            TokenType::CloseParen,
        ]
        .map(wrap_token);

        let parser = expression_parser();

        assert!(parser.parse(convert_nonempty_input(&to_parse)).has_errors());
    }
}
