use std::rc::Rc;
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use ast::data_type::UntypedDataType;
use ast::symbol::{FunctionSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::top_level::Function;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use crate::mics_sa::{analyze_data_type, analyze_type_parameter_full};
use crate::symbol::syntax_element_map::SyntaxElementMap;

pub mod file_symbol_mapper;
pub mod function_symbol_mapper;
pub mod global_system_collector;
mod syntax_element_with_type_parameter_guard;
mod syntax_element_map;

pub(crate) trait TypeParameterContext: Clone {
    fn lookup_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter>;
    fn typed_type_parameters(&self) -> &[TypedTypeParameter];
    fn untyped_type_parameters(&self) -> &[UntypedDataType];
}

#[derive(Clone)]
pub(crate) struct RegularTypeParameterContext {
    typed_type_parameters: Rc<[TypedTypeParameter]>,
    untyped_type_parameters: Rc<[UntypedDataType]>
}

impl RegularTypeParameterContext {
    pub fn new(typed_type_parameters: Rc<[TypedTypeParameter]>, untyped_type_parameters: Rc<[UntypedDataType]>) -> Self {
        Self { typed_type_parameters, untyped_type_parameters }
    }
}

impl TypeParameterContext for RegularTypeParameterContext {
    fn lookup_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter> {
        self.typed_type_parameters.iter().find(|type_parameter| type_parameter.name() == to_lookup)
    }

    fn typed_type_parameters(&self) -> &[TypedTypeParameter] {
        &self.typed_type_parameters
    }

    fn untyped_type_parameters(&self) -> &[UntypedDataType] {
        todo!()
    }
}

pub(crate) struct SyntaxContext<'a, 'b, Context: TypeParameterContext, ASTReference> {
    pub global_elements: &'a mut SyntaxElementMap<'b>,
    pub type_parameter_context: Context,
    pub ast_reference: ASTReference
}

impl<'a, 'b, Context: TypeParameterContext, ASTReference> SyntaxContext<'a, 'b, Context, ASTReference> {
    pub fn new(global_elements: &'a mut SyntaxElementMap<'b>, type_parameter_context: Context, ast_reference: ASTReference) -> Self {
        Self { global_elements, type_parameter_context, ast_reference }
    }
}

impl<'a, 'b, 'c: 'd, 'd, Context: TypeParameterContext, ASTReference: 'c> SyntaxContext<'a, 'b, Context, ASTReference> {
    pub fn with_ast_reference<NewElement>(&'c mut self, ref_gen: impl FnOnce(&'d ASTReference) -> NewElement) -> SyntaxContext<'c, 'b, Context, NewElement> {
        SyntaxContext {
            global_elements: self.global_elements,
            type_parameter_context: self.type_parameter_context.clone(),
            ast_reference: ref_gen(&self.ast_reference),
        }
    }
}

pub(crate) trait AnalyzableSyntaxElementWithTypeParameter {
    type Symbol<Type: ASTType>: SymbolWithTypeParameter<Type>;
    type PreImplementation: Clone;
    type Implementation;
    type ASTReference<'a, 'b>: Clone where 'b: 'a;
    /// This uses a Rc as the symbol is loaded from the untyped AST and not generated
    // Rustrover is wrong, the lifetimes can't be elided
    fn load_untyped_symbol<'a, 'b>(from: &Self::ASTReference<'a, 'b>) -> Rc<Self::Symbol<UntypedAST>>;
    /// This uses a direct symbol as it is generated
    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'_, 'b>>) -> Option<Self::Symbol<TypedAST>>;
    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'_, 'b>>) -> Option<Self::PreImplementation>;
    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(pre_implementation: Self::PreImplementation, context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>) -> Option<Self::Implementation>;

}

pub(crate) struct AnalyzableFunction;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableFunction {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b> = FunctionTraversalHelper<'a, 'b, UntypedAST> where 'b: 'a;
    fn load_untyped_symbol<'a, 'b>(from: &Self::ASTReference<'a, 'b>) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>) -> Option<Self::Symbol<TypedAST>> {
        todo!()
    }

    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(pre_implementation: Self::PreImplementation, context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>) -> Option<Self::Implementation> {
        todo!()
    }
}

/// Converts an untyped function symbol (with String types) into a typed symbol (with Enum types).
///
/// This function validates that the types used in the return type and parameters actually exist
/// and converts them from their string representation to `DataType`.
///
/// # Parameters
/// * `untyped` - The untyped function symbol from the parser.
///
/// # Returns
/// * `Ok(Rc<FunctionSymbol<TypedAST>>)` - The newly created typed symbol wrapped in an Rc.
/// * `Err(String)` - If a type name cannot be resolved.
fn convert_function_symbol(
    untyped: &FunctionSymbol<UntypedAST>,
    context: &mut SyntaxContext<impl TypeParameterContext, impl AnalyzableSyntaxElementWithTypeParameter>
) -> Result<Rc<FunctionSymbol<TypedAST>>, String> {
    let return_type = match untyped.return_type() {
        Some(type_name) => {
            let dt = analyze_data_type(type_name, context).ok_or_else(|| {
                "Semantic Error: Unknown return type".to_string()
            })?;
            Some(dt)
        }
        None => None,
    };

    let mut typed_params = Vec::new();
    for param in untyped.params() {
        let param_type_name = param.data_type();
        let dt = analyze_data_type(param_type_name, context).ok_or_else(|| {
            "Semantic Error: Unknown parameter type".to_string()
        })?;

        let typed_param = Rc::new(VariableSymbol::new(param.name().to_string(), dt));
        typed_params.push(typed_param);
    }

    let typed_type_params = untyped.type_parameters().iter()
        .map(|tp| analyze_type_parameter_full(tp, context).cloned()).collect::<Option<Vec<_>>>().ok_or_else(|| "Unknown type parameter".to_string())?;

    Ok(Rc::new(FunctionSymbol::new(
        untyped.name().to_string(),
        return_type,
        typed_params,
        typed_type_params
    )))
}
