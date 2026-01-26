use std::collections::HashMap;
use std::rc::Rc;
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use ast::data_type::UntypedDataType;
use ast::symbol::{FunctionSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::top_level::Function;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use crate::mics_sa::{analyze_data_type, analyze_type_parameter_full};
use crate::symbol_translation::{AnalyzableSyntaxElementWithTypeParameter, RegularTypeParameterContext, SyntaxContext, TypeParameterContext};
use crate::symbol_translation::syntax_element_map::SyntaxElementMap;

pub(crate) struct SyntaxElementWithTypeParameterGuard<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
    // We could use typed, this would be quicker to create and consume less memory, but would
    // require translating from untyped for each lookup
    // Potential performance improvement: Use a different HashMap for translating from untyped to typed
    // and then lookup with that
    typed: HashMap<Vec<UntypedDataType>, TypedSyntaxElement<Element>>,
    ast_reference: Element::ASTReference<'a, 'b>
}

impl<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter> SyntaxElementWithTypeParameterGuard<'a, 'b, Element> {
    pub fn new(untyped_symbol: Rc<Element::Symbol<UntypedAST>>, ast_reference: Element::ASTReference<'a, 'b>) -> Self {
        Self { untyped_symbol, typed: HashMap::new(), ast_reference }
    }

    pub fn untyped_symbol(&self) -> &Rc<Element::Symbol<UntypedAST>> {
        &self.untyped_symbol
    }

    pub fn typed_variant(&self, type_parameter: &[UntypedDataType]) -> Option<&TypedSyntaxElement<Element>> {
        Some(self.typed.get(type_parameter)?)
    }

    pub fn typed_variant_mut(&mut self, type_parameter: &[UntypedDataType]) -> Option<&mut TypedSyntaxElement<Element>> {
        Some(self.typed.get_mut(type_parameter)?)
    }

    pub fn insert_typed_variant(&mut self, to_insert: TypedSyntaxElement<Element>, untyped_type_parameters: Vec<UntypedDataType>) -> Option<()> {
        let to_insert_type_parameters = to_insert.symbol().type_parameters();
        if self.cnt_type_params() != to_insert_type_parameters.len() {
            return None;
        }
        if self.cnt_type_params() != untyped_type_parameters.len() {
            return None;
        }
        // Prevent overriding
        if self.typed_variant(&untyped_type_parameters).is_some() {
            return None;
        }
        self.typed.insert(untyped_type_parameters, to_insert);
        Some(())
    }
    fn cnt_type_params(&self) -> usize {
        self.untyped_symbol.type_parameters().len()
    }

    pub fn ast_reference(&self) -> &Element::ASTReference<'a, 'b> {
        &self.ast_reference
    }
}



pub(crate) struct TypedSyntaxElement<Element: AnalyzableSyntaxElementWithTypeParameter> {
    typed_type_parameters: Rc<[TypedTypeParameter]>,
    untyped_type_parameters: Rc<[UntypedDataType]>,
    symbol: Rc<Element::Symbol<TypedAST>>,
    pre_implementation: Option<Element::PreImplementation>,
    implementation: Option<Element::Implementation>
}

impl<Element: AnalyzableSyntaxElementWithTypeParameter> TypedSyntaxElement<Element> {
    pub fn new<'c, 'a: 'c, 'b: 'a>(typed_type_parameters: &'c [TypedTypeParameter], untyped_typed_parameters: &'c [UntypedDataType], from: <Element as AnalyzableSyntaxElementWithTypeParameter>::ASTReference<'a, 'b>, global_elements: &'c mut SyntaxElementMap<'b>) -> Option<Self> {
        let typed_type_parameters: Rc<[TypedTypeParameter]> = Rc::from(typed_type_parameters);
        let untyped_type_parameters: Rc<[UntypedDataType]> = Rc::from(untyped_typed_parameters);
        let type_parameter_context = RegularTypeParameterContext::new(typed_type_parameters.clone(), untyped_type_parameters.clone());
        let context = SyntaxContext::new(global_elements, type_parameter_context, from);
        let symbol = Rc::new(Element::generate_typed_symbol(context)?);
        Some(Self {
            typed_type_parameters,
            untyped_type_parameters,
            symbol,
            pre_implementation: None,
            implementation: None,
        })
    }

    pub fn set_pre_implementation(&mut self, pre_implementation: Element::PreImplementation) -> Option<()> {
        if self.pre_implementation.is_none() {
            self.pre_implementation.replace(pre_implementation);
            Some(())
        }
        else {
            None
        }
    }

    pub fn set_implementation(&mut self, implementation: Element::Implementation) -> Option<()> {
        if self.implementation.is_none() {
            self.implementation.replace(implementation);
            Some(())
        }
        else {
            None
        }
    }

    pub fn symbol(&self) -> &Element::Symbol<TypedAST> {
        &self.symbol
    }

    pub fn symbol_owned(&self) -> Rc<Element::Symbol<TypedAST>> {
        self.symbol.clone()
    }

    pub fn pre_implementation(&self) -> Option<&Element::PreImplementation> {
        self.pre_implementation.as_ref()
    }

    pub fn implementation(&self) -> Option<&Element::Implementation> {
        self.implementation.as_ref()
    }

    pub fn typed_type_parameters(&self) -> &[TypedTypeParameter] {
        &self.typed_type_parameters
    }

    pub fn typed_type_parameters_owned(&self) -> Rc<[TypedTypeParameter]> {
        self.typed_type_parameters.clone()
    }

    pub fn untyped_type_parameters(&self) -> &[UntypedDataType] {
        &self.untyped_type_parameters
    }

    pub fn untyped_type_parameters_owned(&self) -> Rc<[UntypedDataType]> {
        self.untyped_type_parameters.clone()
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