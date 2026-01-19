use crate::misc_parsers::{datatype_parser, identifier_parser, token_parser, visibility_parser};
use crate::statement_parser::statement_parser;
use crate::{PosInfoWrapper, combine_code_areas_succeeding};
use ast::symbol::{FunctionSymbol, VariableSymbol};
use ast::top_level::Function;
use ast::visibility::Visibility;
use ast::{ASTNode, UntypedAST};
use chumsky::IterParser;
use chumsky::Parser;
use lexer::TokenType;
use std::rc::Rc;

/// Parses a single function
pub(crate) fn function_parser<'src>()
-> impl Parser<'src, &'src [PosInfoWrapper<TokenType>], ASTNode<Function<UntypedAST>>> {
    let statement = statement_parser();
    let data_type = datatype_parser();
    let ident = identifier_parser();
    let param = data_type
        .clone()
        .then(ident.clone())
        .map(|(data_type, name)| {
            PosInfoWrapper::new(
                Rc::new(VariableSymbol::new(name.inner, data_type.inner)),
                combine_code_areas_succeeding(&data_type.pos_info, &name.pos_info),
            )
        });

    visibility_parser()
        .then(
            token_parser(TokenType::Function).ignore_then(ident).then(
                param
                    .clone()
                    .separated_by(token_parser(TokenType::ArgumentSeparator))
                    .collect::<Vec<PosInfoWrapper<Rc<VariableSymbol<UntypedAST>>>>>()
                    .delimited_by(
                        token_parser(TokenType::OpenParen),
                        token_parser(TokenType::CloseParen),
                    ),
            ),
        )
        .then(
            token_parser(TokenType::Return)
                .ignore_then(data_type)
                .or_not(),
        )
        .then(statement)
        .map(
            |(((visibility, (name, params)), return_type), implementation)| {
                let pos = combine_code_areas_succeeding(visibility
                                                            .as_ref()
                                                            .map(|vis| vis.pos_info())
                                                            .unwrap_or(name.pos_info()), implementation.position());
                let visibility = visibility
                    .map(|_| Visibility::Public)
                    .unwrap_or(Visibility::Private);
                ASTNode::new(
                    Function::new(
                        Rc::new(FunctionSymbol::new(
                            name.inner,
                            return_type.map(|to_map| to_map.inner),
                            params.into_iter().map(|param| param.inner).collect(),
                        )),
                        implementation,
                        visibility
                    ),
                    pos,
                )
            },
        )
}
