use crate::error_sa::SemanticError;
use crate::mics_sa::{
    analyze_data_type, analyze_struct_usage_from_typed_type_parameters,
    analyze_type_parameters_declaration,
};
use crate::symbol::syntax_element_map::{SingleSyntaxElementMap, SyntaxElementMap};
use crate::top_level_sa::{analyze_enum, analyze_function};
use ast::composite::Enum;
use ast::data_type::DataType;
use ast::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    SymbolWithTypeParameter, VariableSymbol,
};
use ast::top_level::Function;
use ast::traversal::FunctionContainer;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use std::rc::Rc;

pub mod function_symbol_mapper;
pub mod global_system_collector;
pub(crate) mod syntax_element_map;
mod syntax_element_with_type_parameter_guard;

/// Contains all relevant information about type parameters for a syntax element
#[derive(Clone)]
pub(crate) struct TypeParameterContext {
    parent: Option<Rc<Self>>,
    current: Rc<[TypedTypeParameter]>,
}

impl TypeParameterContext {
    pub fn lookup_typed_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter> {
        self.current
            .iter()
            .find(|ttp| ttp.name() == to_lookup)
            .or_else(|| self.parent.as_ref()?.lookup_typed_type_parameter(to_lookup))
    }

    pub fn current_owned(&self) -> Rc<[TypedTypeParameter]> {
        self.current.clone()
    }

    pub fn new(parent: Option<Rc<Self>>, current: Rc<[TypedTypeParameter]>) -> Self {
        Self { parent, current }
    }
}

/// Contains all relevant information for semantic analysis of a syntax element
pub(crate) struct SyntaxContext<'a, 'b, ASTReference: Clone> {
    pub global_elements: &'a SyntaxElementMap<'b>,
    pub type_parameter_context: Rc<TypeParameterContext>,
    pub ast_reference: ASTReference,
}

impl<'a, 'b, ASTReference: Clone> SyntaxContext<'a, 'b, ASTReference> {
    pub fn new(
        global_elements: &'a SyntaxElementMap<'b>,
        type_parameter_context: Rc<TypeParameterContext>,
        ast_reference: ASTReference,
    ) -> Self {
        Self {
            global_elements,
            type_parameter_context,
            ast_reference,
        }
    }
}

impl<'a, 'b, 'c, ASTReference: 'c + Clone> SyntaxContext<'a, 'b, ASTReference> {
    pub fn with_ast_reference<NewElement: Clone>(
        &'c self,
        new: NewElement,
    ) -> SyntaxContext<'c, 'b, NewElement> {
        SyntaxContext {
            global_elements: self.global_elements,
            type_parameter_context: self.type_parameter_context.clone(),
            ast_reference: new,
        }
    }
}

/// A syntax element that can be semantically analyzed
///
/// Instantiating implementors of this is not intended
///
/// See [`SingleSyntaxElementMap`] for more information
pub(crate) trait AnalyzableSyntaxElementWithTypeParameter {
    type Symbol<Type: ASTType>: SymbolWithTypeParameter<Type>;
    type PreImplementation: Clone;
    /// The implementation, e.g.: `Function<TypedAST>`
    type Implementation;
    /// A reference to the AST part of this syntax element
    type ASTReference<'a, 'b>: Clone
    where
        'b: 'a;
    type SubAnalyzables<'a>;

    /// This uses a Rc as the symbol is loaded from the untyped AST and not generated
    fn load_untyped_symbol<'b>(from: &Self::ASTReference<'_, 'b>) -> Rc<Self::Symbol<UntypedAST>>;

    /// This uses a direct symbol as it is generated
    fn generate_typed_symbol<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Result<Rc<Self::Symbol<TypedAST>>, SemanticError>;

    fn generate_pre_implementation<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Result<Self::PreImplementation, SemanticError>;

    fn generate_implementation<'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Result<Self::Implementation, SemanticError>;

    fn init_subanalyzables<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'b, 'b>>,
    ) -> Self::SubAnalyzables<'b>;
}

pub(crate) struct AnalyzableFunction;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableFunction {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b> = &'a FunctionTraversalHelper<'a, 'b, UntypedAST>
    where 'b: 'a;
    type SubAnalyzables<'a> = ();

    fn load_untyped_symbol<'b>(from: &Self::ASTReference<'_, 'b>) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Rc<Self::Symbol<TypedAST>>, SemanticError> {
        convert_function_symbol(context)
    }

    fn generate_pre_implementation<'a, 'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::PreImplementation, SemanticError> {
        Ok(())
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::Implementation, SemanticError> {
        analyze_function(symbol, context)
    }

    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {}
}

pub(crate) struct AnalyzableEnum;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableEnum {
    type Symbol<Type: ASTType> = EnumSymbol<Type>;
    type PreImplementation = Vec<Rc<EnumVariantSymbol<TypedAST>>>;
    type Implementation = ASTNode<Enum<TypedAST>>;
    type ASTReference<'a, 'b> = &'a EnumTraversalHelper<'a, 'b, UntypedAST>
    where 'b: 'a;
    type SubAnalyzables<'a> = ();

    fn load_untyped_symbol<'b>(from: &Self::ASTReference<'_, 'b>) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Rc<Self::Symbol<TypedAST>>, SemanticError> {
        convert_enum_symbol(context)
    }

    fn generate_pre_implementation<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::PreImplementation, SemanticError> {
        convert_enum_pre_implementation(context)
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::Implementation, SemanticError> {
        Ok(analyze_enum(symbol, pre_implementation, context))
    }

    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {}
}

pub(crate) struct AnalyzableStruct;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableStruct {
    type Symbol<Type: ASTType> = StructSymbol<Type>;
    type PreImplementation = Vec<Rc<StructFieldSymbol<TypedAST>>>;
    type Implementation = (Rc<Self::Symbol<TypedAST>>, Self::PreImplementation);
    type ASTReference<'a, 'b> = &'a StructTraversalHelper<'a, 'b, UntypedAST>
    where 'b: 'a;
    type SubAnalyzables<'a> = SingleSyntaxElementMap<'a, AnalyzableMethod>;

    fn load_untyped_symbol<'b>(from: &Self::ASTReference<'_, 'b>) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Rc<Self::Symbol<TypedAST>>, SemanticError> {
        convert_struct_symbol(context)
    }

    fn generate_pre_implementation<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::PreImplementation, SemanticError> {
        convert_struct_pre_implementation(context)
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::Implementation, SemanticError> {
        Ok((symbol, pre_implementation))
    }

    fn init_subanalyzables<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'b, 'b>>,
    ) -> Self::SubAnalyzables<'b> {
        let mut methods = SingleSyntaxElementMap::new_child(context.type_parameter_context.clone());
        context.ast_reference.function_iterator().for_each(|func| {
            methods.insert_untyped_element((context.ast_reference, func));
        });
        methods
    }
}

pub(crate) struct AnalyzableMethod;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableMethod {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b> = (
        &'a StructTraversalHelper<'a, 'b, UntypedAST>,
        FunctionTraversalHelper<'a, 'b, UntypedAST>,
    )
    where 'b: 'a;
    type SubAnalyzables<'a> = ();

    fn load_untyped_symbol<'b>(from: &Self::ASTReference<'_, 'b>) -> Rc<Self::Symbol<UntypedAST>> {
        from.1.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Rc<Self::Symbol<TypedAST>>, SemanticError> {
        convert_method_symbol(context)
    }

    fn generate_pre_implementation<'a, 'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::PreImplementation, SemanticError> {
        Ok(())
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Result<Self::Implementation, SemanticError> {
        let context = context.with_ast_reference(&context.ast_reference.1);
        analyze_function(symbol, &context)
    }

    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {}
}

fn convert_function_symbol(
    context: &SyntaxContext<&FunctionTraversalHelper<UntypedAST>>,
) -> Result<Rc<FunctionSymbol<TypedAST>>, SemanticError> {
    let untyped = context.ast_reference.inner().declaration();
    let return_type = match untyped.return_type() {
        Some(type_name) => Some(analyze_data_type(type_name, context)?),
        None => None,
    };

    let mut typed_params = Vec::new();
    for param in untyped.params() {
        let dt = analyze_data_type(param.data_type(), context)?;
        let typed_param = Rc::new(VariableSymbol::new(param.name().to_string(), dt));
        typed_params.push(typed_param);
    }

    let typed_type_params =
        analyze_type_parameters_declaration(context, untyped.type_parameters().iter())?;

    Ok(Rc::new(FunctionSymbol::new(
        untyped.name().to_string(),
        return_type,
        typed_params,
        typed_type_params,
    )))
}

fn convert_method_symbol(
    context: &SyntaxContext<(
        &StructTraversalHelper<UntypedAST>,
        FunctionTraversalHelper<UntypedAST>,
    )>,
) -> Result<Rc<FunctionSymbol<TypedAST>>, SemanticError> {
    let internal_context = context.with_ast_reference(&context.ast_reference.1);
    let untyped = context.ast_reference.1.inner().declaration();
    let return_type = match untyped.return_type() {
        Some(type_name) => Some(analyze_data_type(type_name, &internal_context)?),
        None => None,
    };

    let type_params = context
        .type_parameter_context
        .parent
        .as_ref()
        .unwrap()
        .current_owned();

    let mut typed_params = Vec::new();
    let struct_use = analyze_struct_usage_from_typed_type_parameters(
        context.ast_reference.0.inner().symbol().name(),
        &type_params,
        &internal_context,
    )?;

    typed_params.push(Rc::new(VariableSymbol::new(
        "self".to_owned(),
        DataType::Struct(struct_use),
    )));

    for param in untyped.params() {
        let dt = analyze_data_type(param.data_type(), &internal_context)?;
        let typed_param = Rc::new(VariableSymbol::new(param.name().to_string(), dt));
        typed_params.push(typed_param);
    }

    let typed_type_params =
        analyze_type_parameters_declaration(&internal_context, untyped.type_parameters().iter())?;

    Ok(Rc::new(FunctionSymbol::new(
        untyped.name().to_string(),
        return_type,
        typed_params,
        typed_type_params,
    )))
}

fn convert_enum_symbol(
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
) -> Result<Rc<EnumSymbol<TypedAST>>, SemanticError> {
    let untyped = context.ast_reference.inner().symbol();
    let typed_type_params =
        analyze_type_parameters_declaration(context, untyped.type_parameters().iter())?;

    Ok(Rc::new(EnumSymbol::new(
        untyped.name().to_string(),
        typed_type_params,
    )))
}

fn convert_enum_pre_implementation(
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
) -> Result<Vec<Rc<EnumVariantSymbol<TypedAST>>>, SemanticError> {
    let untyped = context.ast_reference.inner().variants();

    untyped
        .iter()
        .map(|variant| variant.inner())
        .map(|variant| {
            let fields = variant
                .fields()
                .iter()
                .map(|field| analyze_data_type(field, context))
                .collect::<Result<Vec<_>, SemanticError>>()?;

            Ok(Rc::new(EnumVariantSymbol::<TypedAST>::new(
                variant.name().to_owned(),
                fields,
            )))
        })
        .collect()
}

fn convert_struct_symbol(
    context: &SyntaxContext<&StructTraversalHelper<UntypedAST>>,
) -> Result<Rc<StructSymbol<TypedAST>>, SemanticError> {
    let untyped = context.ast_reference.inner().symbol();
    let typed_type_params =
        analyze_type_parameters_declaration(context, untyped.type_parameters().iter())?;

    Ok(Rc::new(StructSymbol::new(
        untyped.name().to_string(),
        typed_type_params,
    )))
}

fn convert_struct_pre_implementation(
    context: &SyntaxContext<&StructTraversalHelper<UntypedAST>>,
) -> Result<Vec<Rc<StructFieldSymbol<TypedAST>>>, SemanticError> {
    let untyped = context.ast_reference.inner().fields();

    untyped
        .iter()
        .map(|variant| variant.inner())
        .map(|variant| {
            let dt = analyze_data_type(variant.data_type(), context)?;
            Ok(Rc::new(StructFieldSymbol::<TypedAST>::new(
                variant.name().to_owned(),
                dt,
            )))
        })
        .collect()
}