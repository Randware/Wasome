use crate::mics_sa::{
    analyze_data_type, analyze_type_parameter_full, analyze_type_parameter_providing,
    analyze_type_parameters_declaration,
};
use crate::symbol::syntax_element_map::{SingleSyntaxElementMap, SyntaxElementMap};
use crate::top_level_sa::{analyze_enum, analyze_function};
use ast::composite::{Enum, Struct};
use ast::data_type::UntypedDataType;
use ast::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    SymbolWithTypeParameter, VariableSymbol,
};
use ast::top_level::Function;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use std::rc::Rc;
use crate::symbol::syntax_element_with_type_parameter_guard::{SyntaxElementWithTypeParameterGuard, TypedSyntaxElement};

pub mod file_symbol_mapper;
pub mod function_symbol_mapper;
pub mod global_system_collector;
pub(crate) mod syntax_element_map;
mod syntax_element_with_type_parameter_guard;

pub(crate) trait TypeParameterContext: Clone {
    fn lookup_typed_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter>;
    fn current_typed_type_parameters(&self) -> &[TypedTypeParameter];
}

#[derive(Clone)]
pub(crate) struct RegularTypeParameterContext {
    typed_type_parameters: Rc<[TypedTypeParameter]>,
}

impl RegularTypeParameterContext {
    pub fn new(typed_type_parameters: Rc<[TypedTypeParameter]>) -> Self {
        Self {
            typed_type_parameters,
        }
    }
}

impl TypeParameterContext for RegularTypeParameterContext {
    fn lookup_typed_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter> {
        self.typed_type_parameters
            .iter()
            .find(|type_parameter| type_parameter.name() == to_lookup)
    }

    fn current_typed_type_parameters(&self) -> &[TypedTypeParameter] {
        &self.typed_type_parameters
    }
}

#[derive(Clone)]
pub(crate) struct TypeParameterContextWithParent<Parent: TypeParameterContext, Current: TypeParameterContext> {
    parent: Parent,
    current: Current,
}

impl<Parent: TypeParameterContext, Current: TypeParameterContext> TypeParameterContextWithParent<Parent, Current> {
    pub fn new(parent: Parent, current: Current) -> Self {
        Self { parent, current }
    }
}

impl<Parent: TypeParameterContext, Current: TypeParameterContext> TypeParameterContext for TypeParameterContextWithParent<Parent, Current> {
    fn lookup_typed_type_parameter(&self, to_lookup: &str) -> Option<&TypedTypeParameter> {
        self.current.lookup_typed_type_parameter(to_lookup)
            .or_else(|| self.parent.lookup_typed_type_parameter(to_lookup))
    }

    fn current_typed_type_parameters(&self) -> &[TypedTypeParameter] {
        &self.current.current_typed_type_parameters()
    }
}

pub(crate) struct SyntaxContext<'a, 'b, Context: TypeParameterContext, ASTReference: Clone> {
    pub global_elements: &'a mut SyntaxElementMap<'b>,
    pub type_parameter_context: Context,
    pub ast_reference: ASTReference,
}

impl<'a, 'b, Context: TypeParameterContext, ASTReference: Clone>
    SyntaxContext<'a, 'b, Context, ASTReference>
{
    pub fn new(
        global_elements: &'a mut SyntaxElementMap<'b>,
        type_parameter_context: Context,
        ast_reference: ASTReference,
    ) -> Self {
        Self {
            global_elements,
            type_parameter_context,
            ast_reference,
        }
    }
}

impl<'a, 'b, 'c: 'd, 'd, Context: TypeParameterContext, ASTReference: 'c + Clone>
    SyntaxContext<'a, 'b, Context, ASTReference>
{
    pub fn with_ast_reference<NewElement: Clone>(
        &'c mut self,
        ref_gen: impl FnOnce(&'d ASTReference) -> NewElement,
    ) -> SyntaxContext<'c, 'b, Context, NewElement> {
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
    type ASTReference<'a, 'b>: Clone
    where
        'b: 'a;
    type SubAnalyzables<'a>;
    /// This uses a Rc as the symbol is loaded from the untyped AST and not generated
    // Rustrover is wrong, the lifetimes can't be elided
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>>;
    /// This uses a direct symbol as it is generated
    fn generate_typed_symbol<'b, Context: TypeParameterContext>(
        context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'_, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>>;
    fn generate_pre_implementation<'b, Context: TypeParameterContext>(
        context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'_, 'b>>,
    ) -> Option<Self::PreImplementation>;
    fn generate_implementation<'b, Context: TypeParameterContext>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'_, 'b>>,
    ) -> Option<Self::Implementation>;
    fn init_subanalyzables<'a>(
        from: &Self::ASTReference<'a, 'a>,
    ) -> Self::SubAnalyzables<'a>;
}

pub(crate) struct AnalyzableFunction;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableFunction {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b>
        = FunctionTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = ();
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_function_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(
        _context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        analyze_function(symbol, &mut context)
    }

    fn init_subanalyzables<'a>(_from: &Self::ASTReference<'a, 'a>) -> Self::SubAnalyzables<'a> {
        ()
    }
}

pub(crate) struct AnalyzableEnum;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableEnum {
    type Symbol<Type: ASTType> = EnumSymbol<Type>;
    type PreImplementation = Vec<Rc<EnumVariantSymbol<TypedAST>>>;
    type Implementation = ASTNode<Enum<TypedAST>>;
    type ASTReference<'a, 'b>
        = EnumTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = ();
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_enum_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        convert_enum_pre_implementation(&mut context)
    }

    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        Some(analyze_enum(symbol, pre_implementation, &mut context))
    }
    fn init_subanalyzables<'a>(_from: &Self::ASTReference<'a, 'a>) -> Self::SubAnalyzables<'a> {
        ()
    }
}

pub(crate) struct AnalyzableStruct;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableStruct {
    type Symbol<Type: ASTType> = StructSymbol<Type>;
    type PreImplementation = Vec<Rc<StructFieldSymbol<TypedAST>>>;
    type Implementation = ASTNode<Struct<TypedAST>>;
    type ASTReference<'a, 'b>
        = StructTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = SingleSyntaxElementMap<'a, AnalyzableMethod>;
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_struct_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        convert_struct_pre_implementation(&mut context)
    }

    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        todo!()
        //analyze_function(&mut context)
    }

    fn init_subanalyzables<'a>(from: &Self::ASTReference<'a, 'a>) -> Self::SubAnalyzables<'a> {
        SingleSyntaxElementMap::new()
    }
}

pub(crate) struct AnalyzableMethod;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableMethod {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b>
    = (RegularTypeParameterContext, FunctionTraversalHelper<'a, 'b, UntypedAST>)
    where
        'b: 'a;
    type SubAnalyzables<'a> = ();
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.1.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b, Context: TypeParameterContext>(
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        let type_param_context = TypeParameterContextWithParent::new(context.ast_reference.0, context.type_parameter_context);
        let mut context = SyntaxContext::new(context.global_elements, type_param_context, context.ast_reference.1);
        convert_function_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b, Context: TypeParameterContext>(
        _context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation<'a, 'b, Context: TypeParameterContext>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: SyntaxContext<'_, 'b, Context, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        let type_param_context = TypeParameterContextWithParent::new(context.ast_reference.0, context.type_parameter_context);
        let mut context = SyntaxContext::new(context.global_elements, type_param_context, context.ast_reference.1);
        analyze_function(symbol, &mut context)
    }

    fn init_subanalyzables<'a>(_from: &Self::ASTReference<'a, 'a>) -> Self::SubAnalyzables<'a> {
        ()
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
    context: &mut SyntaxContext<impl TypeParameterContext, FunctionTraversalHelper<UntypedAST>>,
) -> Result<Rc<FunctionSymbol<TypedAST>>, String> {
    let untyped = context.ast_reference.inner().declaration();
    let return_type = match untyped.return_type() {
        Some(type_name) => {
            let dt = analyze_data_type(type_name, context)
                .ok_or_else(|| "Semantic Error: Unknown return type".to_string())?;
            Some(dt)
        }
        None => None,
    };

    let mut typed_params = Vec::new();
    for param in untyped.params() {
        let param_type_name = param.data_type();
        let dt = analyze_data_type(param_type_name, context)
            .ok_or_else(|| "Semantic Error: Unknown parameter type".to_string())?;

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

fn convert_enum_symbol(
    context: &mut SyntaxContext<impl TypeParameterContext, EnumTraversalHelper<UntypedAST>>,
) -> Result<Rc<EnumSymbol<TypedAST>>, String> {
    let untyped = context.ast_reference.inner().symbol();

    let typed_type_params =
        analyze_type_parameters_declaration(context, untyped.type_parameters().iter())?;

    Ok(Rc::new(EnumSymbol::new(
        untyped.name().to_string(),
        typed_type_params,
    )))
}

fn convert_enum_pre_implementation(
    context: &mut SyntaxContext<impl TypeParameterContext, EnumTraversalHelper<UntypedAST>>,
) -> Option<Vec<Rc<EnumVariantSymbol<TypedAST>>>> {
    let untyped = context.ast_reference.inner().variants();

    untyped
        .iter()
        .map(|variant| variant.inner())
        .map(|variant| {
            Some(Rc::new(EnumVariantSymbol::<TypedAST>::new(
                variant.name().to_owned(),
                variant
                    .fields()
                    .iter()
                    .map(|field| analyze_data_type(field, context))
                    .collect::<Option<Vec<_>>>()?,
            )))
        })
        .collect()
}

fn convert_struct_symbol(
    context: &mut SyntaxContext<impl TypeParameterContext, StructTraversalHelper<UntypedAST>>,
) -> Result<Rc<StructSymbol<TypedAST>>, String> {
    let untyped = context.ast_reference.inner().symbol();

    let typed_type_params =
        analyze_type_parameters_declaration(context, untyped.type_parameters().iter())?;

    Ok(Rc::new(StructSymbol::new(
        untyped.name().to_string(),
        typed_type_params,
    )))
}

fn convert_struct_pre_implementation(
    context: &mut SyntaxContext<impl TypeParameterContext, StructTraversalHelper<UntypedAST>>,
) -> Option<Vec<Rc<StructFieldSymbol<TypedAST>>>> {
    let untyped = context.ast_reference.inner().fields();

    untyped
        .iter()
        .map(|variant| variant.inner())
        .map(|variant| {
            Some(Rc::new(StructFieldSymbol::<TypedAST>::new(
                variant.name().to_owned(),
                analyze_data_type(variant.data_type(), context)?,
            )))
        })
        .collect()
}
