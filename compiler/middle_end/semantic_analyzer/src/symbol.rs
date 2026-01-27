use crate::mics_sa::{
    analyze_data_type, analyze_type_parameter_full, analyze_type_parameter_providing,
    analyze_type_parameters_declaration,
};
use crate::symbol::syntax_element_map::{SingleSyntaxElementMap, SyntaxElementMap};
use crate::symbol::syntax_element_with_type_parameter_guard::{
    SyntaxElementWithTypeParameterGuard, TypedSyntaxElement,
};
use crate::top_level_sa::{analyze_enum, analyze_function};
use ast::composite::{Enum, Struct};
use ast::data_type::UntypedDataType;
use ast::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    SymbolWithTypeParameter, VariableSymbol,
};
use ast::top_level::Function;
use ast::traversal::FunctionContainer;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::{TypedTypeParameter, UntypedTypeParameter};
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use std::rc::Rc;
use std::task::Context;

pub mod file_symbol_mapper;
pub mod function_symbol_mapper;
pub mod global_system_collector;
pub(crate) mod syntax_element_map;
mod syntax_element_with_type_parameter_guard;

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
    pub fn current(&self) -> &[TypedTypeParameter] {
        &self.current
    }

    pub fn current_owned(&self) -> Rc<[TypedTypeParameter]> {
        self.current.clone()
    }

    pub fn new_child(parent: Rc<Self>, current: Rc<[TypedTypeParameter]>) -> Self {
        Self {
            parent: Some(parent),
            current,
        }
    }

    pub fn new_root(current: Rc<[TypedTypeParameter]>) -> Self {
        Self {
            parent: None,
            current,
        }
    }

    pub fn new(parent: Option<Rc<Self>>, current: Rc<[TypedTypeParameter]>) -> Self {
        Self { parent, current }
    }
}

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

impl<'a, 'b, 'c: 'd, 'd, ASTReference: 'c + Clone> SyntaxContext<'a, 'b, ASTReference> {
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
    fn generate_typed_symbol<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>>;
    fn generate_pre_implementation<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Option<Self::PreImplementation>;
    fn generate_implementation<'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Option<Self::Implementation>;
    fn init_subanalyzables<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'b, 'b>>,
    ) -> Self::SubAnalyzables<'b>;
}

pub(crate) struct AnalyzableFunction;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableFunction {
    type Symbol<Type: ASTType> = FunctionSymbol<Type>;
    type PreImplementation = ();
    type Implementation = ASTNode<Function<TypedAST>>;
    type ASTReference<'a, 'b>
        = &'a FunctionTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = ();
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().declaration_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_function_symbol(context).ok()
    }

    fn generate_pre_implementation<'a, 'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        analyze_function(symbol, &mut context)
    }

    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {
        ()
    }
}

pub(crate) struct AnalyzableEnum;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableEnum {
    type Symbol<Type: ASTType> = EnumSymbol<Type>;
    type PreImplementation = Vec<Rc<EnumVariantSymbol<TypedAST>>>;
    type Implementation = ASTNode<Enum<TypedAST>>;
    type ASTReference<'a, 'b>
        = &'a EnumTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = ();
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_enum_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b>(
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        convert_enum_pre_implementation(&mut context)
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        pre_implementation: Self::PreImplementation,
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        Some(analyze_enum(symbol, pre_implementation, context))
    }
    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {
        ()
    }
}

pub(crate) struct AnalyzableStruct;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableStruct {
    type Symbol<Type: ASTType> = StructSymbol<Type>;
    type PreImplementation = Vec<Rc<StructFieldSymbol<TypedAST>>>;
    type Implementation = ASTNode<Struct<TypedAST>>;
    type ASTReference<'a, 'b>
        = &'a StructTraversalHelper<'a, 'b, UntypedAST>
    where
        'b: 'a;
    type SubAnalyzables<'a> = SingleSyntaxElementMap<'a, AnalyzableMethod>;
    fn load_untyped_symbol<'a, 'b>(
        from: &Self::ASTReference<'a, 'b>,
    ) -> Rc<Self::Symbol<UntypedAST>> {
        from.inner().symbol_owned()
    }

    fn generate_typed_symbol<'a, 'b>(
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        convert_struct_symbol(&mut context).ok()
    }

    fn generate_pre_implementation<'a, 'b>(
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        convert_struct_pre_implementation(&mut context)
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        todo!()
        //analyze_function(&mut context)
    }

    fn init_subanalyzables<'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'b, 'b>>,
    ) -> Self::SubAnalyzables<'b> {
        let mut methods = SingleSyntaxElementMap::new_child(context.type_parameter_context.clone());
        context.ast_reference.function_iterator().for_each(|func| {
            methods.insert_untyped_element(func);
        });
        methods
    }
}

pub(crate) struct AnalyzableMethod;
impl AnalyzableSyntaxElementWithTypeParameter for AnalyzableMethod {
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

    fn generate_typed_symbol<'a, 'b>(
        context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Rc<Self::Symbol<TypedAST>>> {
        todo!()
        //convert_function_symbol(context).ok()
    }

    fn generate_pre_implementation<'a, 'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation<'a, 'b>(
        symbol: Rc<Self::Symbol<TypedAST>>,
        _pre_implementation: Self::PreImplementation,
        mut context: &SyntaxContext<'_, 'b, Self::ASTReference<'a, 'b>>,
    ) -> Option<Self::Implementation> {
        todo!()
        //analyze_function(symbol, &mut context)
    }

    fn init_subanalyzables<'b>(
        _context: &SyntaxContext<'_, 'b, Self::ASTReference<'_, 'b>>,
    ) -> Self::SubAnalyzables<'b> {
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
    context: &SyntaxContext<&FunctionTraversalHelper<UntypedAST>>,
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
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
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
    context: &SyntaxContext<&EnumTraversalHelper<UntypedAST>>,
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
    context: &SyntaxContext<&StructTraversalHelper<UntypedAST>>,
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
    context: &SyntaxContext<&StructTraversalHelper<UntypedAST>>,
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
