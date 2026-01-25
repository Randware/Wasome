use std::collections::HashMap;
use std::rc::Rc;
use ast::{ASTNode, ASTType, TypedAST, UntypedAST};
use ast::data_type::UntypedDataType;
use ast::symbol::{FunctionSymbol, SymbolWithTypeParameter, VariableSymbol};
use ast::top_level::Function;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use crate::mics_sa::analyze_data_type;
use crate::symbol_translation::syntax_element_map::SyntaxElementMap;

pub(crate) trait AnalyzableSyntaxElementWithTypeParameter {
    type Symbol<Type: ASTType>: SymbolWithTypeParameter<Type>;
    type PreImplementation;
    type Implementation;
    type ASTReference<'a, 'b>: Clone where 'b: 'a;
    /// This uses a Rc as the symbol is loaded from the untyped AST and not generated
    // Rustrover is wrong, the lifetimes can't be elided
    fn load_untyped_symbol<'a, 'b>(from: &Self::ASTReference<'a, 'b>) -> Rc<Self::Symbol<UntypedAST>>;
    /// This uses a direct symbol as it is generated
    fn generate_typed_symbol<'a, 'b>(type_parameters: &[TypedTypeParameter], from: &Self::ASTReference<'a, 'b>, global_elements: &mut SyntaxElementMap) -> Option<Self::Symbol<TypedAST>>;
    fn generate_pre_implementation<'a, 'b>(type_parameters: &[TypedTypeParameter], from: &Self::ASTReference<'a, 'b>, global_elements: &mut SyntaxElementMap) -> Option<Self::PreImplementation>;
    fn generate_implementation<'a, 'b>(type_parameters: &[TypedTypeParameter], from: &Self::ASTReference<'a, 'b>, global_elements: &mut SyntaxElementMap) -> Option<Self::Implementation>;

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

    fn generate_typed_symbol(type_parameters: &[TypedTypeParameter], from: &Self::ASTReference<'_, '_>, global_elements: &mut SyntaxElementMap) -> Option<Self::Symbol<TypedAST>> {
        todo!()
    }

    fn generate_pre_implementation(_type_parameters: &[TypedTypeParameter], _from: &Self::ASTReference<'_, '_>, _global_elements: &mut SyntaxElementMap) -> Option<Self::PreImplementation> {
        Some(())
    }

    fn generate_implementation(type_parameters: &[TypedTypeParameter], from: &Self::ASTReference<'_, '_>, global_elements: &mut SyntaxElementMap) -> Option<Self::Implementation> {
        todo!()
    }
}

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
    symbol: Rc<Element::Symbol<TypedAST>>,
    pre_implementation: Option<Element::PreImplementation>,
    implementation: Option<Element::Implementation>
}

impl<Element: AnalyzableSyntaxElementWithTypeParameter> TypedSyntaxElement<Element> {
    pub fn new(type_parameters: &[TypedTypeParameter], from: &Element::ASTReference<'_, '_>, global_elements: &mut SyntaxElementMap) -> Option<Self> {
        Some(Self {
            symbol: Rc::new(Element::generate_typed_symbol(type_parameters, from, global_elements)?),
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
}

/*/// Converts an untyped function symbol (with String types) into a typed symbol (with Enum types).
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
) -> Result<Rc<FunctionSymbol<TypedAST>>, String> {
    let return_type = match untyped.return_type() {
        Some(type_name) => {
            let dt = analyze_data_type(type_name).ok_or_else(|| {
                format!(
                    "Semantic Error: Unknown return type '{}' in function '{}'",
                    type_name,
                    untyped.name()
                )
            })?;
            Some(dt)
        }
        None => None,
    };

    let mut typed_params = Vec::new();
    for param in untyped.params() {
        let param_type_name = param.data_type();
        let dt = analyze_data_type(param_type_name).ok_or_else(|| {
            format!(
                "Semantic Error: Unknown parameter type '{}' for parameter '{}' in function '{}'",
                param_type_name,
                param.name(),
                untyped.name()
            )
        })?;

        let typed_param = Rc::new(VariableSymbol::new(param.name().to_string(), dt));
        typed_params.push(typed_param);
    }

    Ok(Rc::new(FunctionSymbol::new(
        untyped.name().to_string(),
        return_type,
        typed_params,
    )))
}*/