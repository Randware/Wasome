use crate::symbol::syntax_element_with_type_parameter_guard::{
    SyntaxElementWithTypeParameterTranslator, TypedSyntaxElement,
};
use crate::symbol::{
    AnalyzableEnum, AnalyzableFunction, AnalyzableMethod, AnalyzableStruct,
    AnalyzableSyntaxElementWithTypeParameter, SyntaxContext, TypeParameterContext,
};
use ast::composite::Enum;
use ast::symbol::{
    EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol,
    SymbolWithTypeParameter,
};
use ast::top_level::Function;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

/// Multiple [`SingleSyntaxElementMap`], one for each of syntax element with type parameters
///
/// This also includes functionality for handling methods
pub(crate) struct SyntaxElementMap<'a> {
    functions: SingleSyntaxElementMap<'a, AnalyzableFunction>,
    enums: SingleSyntaxElementMap<'a, AnalyzableEnum>,
    structs: SingleSyntaxElementMap<'a, AnalyzableStruct>,
}

impl<'a> SyntaxElementMap<'a> {
    pub fn new() -> Self {
        Self {
            functions: SingleSyntaxElementMap::new_root(),
            enums: SingleSyntaxElementMap::new_root(),
            structs: SingleSyntaxElementMap::new_root(),
        }
    }

    pub fn get_or_insert_typed_function_symbol(
        &self,
        symbol: Rc<FunctionSymbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        self.functions
            .get_or_insert_typed_symbol(self, symbol, type_parameters)
    }

    /// Gets the symbol of the provided method
    ///
    /// This first of all loads the struct the method belongs to and then fetches the method
    pub fn get_typed_method_symbol(
        &self,
        from: &StructSymbol<UntypedAST>,
        from_type_parameters: &[TypedTypeParameter],
        symbol: Rc<FunctionSymbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        let struct_of_method = self
            .structs
            .get_typed_syntax_element_mut(from, from_type_parameters)?;
        let methods = struct_of_method.subanalyzables();

        methods.get_or_insert_typed_symbol(self, symbol, type_parameters)
    }

    pub fn insert_untyped_enum(
        &mut self,
        to_insert: &'a EnumTraversalHelper<'a, 'a, UntypedAST>,
    ) -> Option<()> {
        self.enums.insert_untyped_element(to_insert)
    }

    pub fn get_typed_enum_symbol(
        &self,
        symbol: Rc<EnumSymbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<EnumSymbol<TypedAST>>> {
        self.enums
            .get_or_insert_typed_symbol(self, symbol, type_parameters)
    }

    pub fn get_enum_variants<'b>(
        &'b self,
        symbol: &EnumSymbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Ref<'b, Vec<Rc<EnumVariantSymbol<TypedAST>>>>> {
        self.enums.get_pre_implementation(symbol, type_parameters)
    }

    pub fn insert_untyped_struct(
        &mut self,
        to_insert: &'a StructTraversalHelper<'a, 'a, UntypedAST>,
    ) -> Option<()> {
        self.structs.insert_untyped_element(to_insert)
    }

    pub fn get_typed_struct_symbol(
        &self,
        symbol: Rc<StructSymbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<StructSymbol<TypedAST>>> {
        self.structs
            .get_or_insert_typed_symbol(self, symbol, type_parameters)
    }

    pub fn get_struct_fields<'b>(
        &'b self,
        symbol: &StructSymbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Ref<'b, Vec<Rc<StructFieldSymbol<TypedAST>>>>> {
        self.structs.get_pre_implementation(symbol, type_parameters)
    }

    pub fn insert_untyped_function(
        &mut self,
        to_insert: &'a FunctionTraversalHelper<'a, 'a, UntypedAST>,
    ) -> Option<()> {
        self.functions.insert_untyped_element(to_insert)
    }

    /// Fills self with all typed variants that are ever used in the program
    ///
    /// # Tree Shaking
    /// Only functions without type parameters (e.g., `main` or specific implementations) are used as entry points.
    /// Any code reachable from these functions is analyzed. Unreachable generic functions or unused library code
    /// that is not transitively referenced by a non-generic function will be ignored.
    pub fn fill(&self) -> Option<()> {
        let mut ok = Some(());
        // It is sufficient to translate all functions without type parameters
        // Anything reachable (=used by) from these functions will be translated as well
        // The main functions may not have type parameters
        // And anything not reachable by the main functions is unused and thus not required by codegen
        let funcs = self.functions.untyped_elements();
        funcs
            .into_iter()
            .filter(|func| func.as_ref().type_parameters().is_empty())
            .for_each(|func| {
                let typed_symbol = self.get_or_insert_typed_function_symbol(func, &[]);
                if typed_symbol.is_none() {
                    ok = None;
                }
            });
        ok
    }

    pub fn function_implementations_for_untyped_symbol(
        &mut self,
        symbol: &FunctionSymbol<UntypedAST>,
    ) -> Option<impl Iterator<Item = ASTNode<Function<TypedAST>>>> {
        self.functions
            .implementations_for_untyped_symbol(symbol)
            .map(|funcs| funcs.map(|func| func.0))
    }

    pub fn enum_implementations_for_untyped_symbol(
        &mut self,
        symbol: &EnumSymbol<UntypedAST>,
    ) -> Option<impl Iterator<Item = ASTNode<Enum<TypedAST>>>> {
        self.enums
            .implementations_for_untyped_symbol(symbol)
            .map(|ens| ens.map(|en| en.0))
    }

    pub fn struct_implementations_for_untyped_symbol(
        &mut self,
        symbol: &StructSymbol<UntypedAST>,
    ) -> Option<
        impl Iterator<
            Item = StructImplementation<'a>,
        >,
    > {
        self.structs.implementations_for_untyped_symbol(symbol)
    }

    pub fn untyped_struct_symbol_from_typed(
        &self,
        typed: &StructSymbol<TypedAST>,
    ) -> Option<Rc<StructSymbol<UntypedAST>>> {
        self.structs.untyped_from_typed_symbol(typed)
    }
}

pub(crate) type StructImplementation<'a> = (
    (
        Rc<StructSymbol<TypedAST>>,
        Vec<Rc<StructFieldSymbol<TypedAST>>>,
    ),
    SingleSyntaxElementMap<'a, AnalyzableMethod>,
);

impl<'a> Default for SyntaxElementMap<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Translates a single type of syntax element that has type parameters from untyped to typed
///
/// The process for a single element is as follows:
/// 1. Generate the symbol
/// 2. Initialize the sub-analyzables
///     - Currently, only structs have non-empty sub-analyzables
///         - Structs have methods
///         - If syntax element A is a subanalyzable of B, then we say that B is the parent of A
///     - If not empty, they are an instance of [`SingleSyntaxElementMap`]
/// 2. Generate the pre-implementation
///     - It consists of the variants for enums and fields for structs
///     - Otherwise, it is empty
/// 3. Generate the implementation
///
/// Each intermediate result is immediately inserted into the map to allow for cyclic usages
/// (e.g.: A struct that contains itself)
///
/// Translating everything at once would not allow this
///
///
/// The entire semantic analysis process can be described as:
/// 1. Load all untyped elements and insert them
/// 2. Recursively translate all elements and their dependencies
/// 3. Take the typed translation results and put them into a typed AST
///
/// This uses dynamic borrow checking as static borrow checking is simply insufficient to allow
/// the complex usage patters required
pub(crate) struct SingleSyntaxElementMap<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    elements: HashMap<
        Rc<Element::Symbol<UntypedAST>>,
        RefCell<SyntaxElementWithTypeParameterTranslator<'a, 'a, Element>>,
    >,
    /// Maps typed symbols to untyped
    /// Required as certain operations require untyped symbols and only typed symbols may be available
    /// (e.g.: When analyzing a method call)
    untyped_symbols:
        RefCell<UntypedTypeParameterMap<Element>>,
    /// All type parameters that are available here
    type_parameters: Option<Rc<TypeParameterContext>>,
}

// It doesn't get enforced, but leaving the bound away makes Element have no associated typed
#[allow(type_alias_bounds)]
pub(crate) type UntypedTypeParameterMap<Element: AnalyzableSyntaxElementWithTypeParameter> = HashMap<Rc<Element::Symbol<TypedAST>>, Rc<Element::Symbol<UntypedAST>>>;

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> SingleSyntaxElementMap<'a, Element> {
    pub fn new_root() -> Self {
        Self {
            elements: HashMap::new(),
            type_parameters: None,
            untyped_symbols: RefCell::default(),
        }
    }

    pub fn new_child(type_parameters: Rc<TypeParameterContext>) -> Self {
        Self {
            elements: HashMap::new(),
            type_parameters: Some(type_parameters),
            untyped_symbols: RefCell::default(),
        }
    }

    /// Inserts a new untyped syntax element
    ///
    /// This is only supposed to be called at the beginning of the semantic analysis process
    ///
    /// # Errors
    ///
    /// If the element already exists
    pub fn insert_untyped_element(
        &mut self,
        to_insert: Element::ASTReference<'a, 'a>,
    ) -> Option<()> {
        let untyped_symbol = Element::load_untyped_symbol(&to_insert);
        if self.elements.contains_key(&untyped_symbol) {
            return None;
        }
        let guard = SyntaxElementWithTypeParameterTranslator::new(
            untyped_symbol.clone(),
            to_insert,
            self.type_parameters.clone(),
        );
        self.elements.insert(untyped_symbol, RefCell::new(guard));
        Some(())
    }

    pub fn untyped_elements(&self) -> impl Iterator<Item = Rc<Element::Symbol<UntypedAST>>> {
        self.elements.keys().cloned()
    }

    /// Gets a typed variant of an untyped symbol with the provided type parameters
    ///
    /// If it doesn't exist, it and the rest of its element is created
    pub fn get_or_insert_typed_symbol<'b>(
        &self,
        root: &'b SyntaxElementMap<'a>,
        symbol: Rc<Element::Symbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<Element::Symbol<TypedAST>>> {
        {
            // Use an inner scope to drop the borrow and prevent panics
            let guard = self.elements.get(&symbol)?.borrow();

            if guard.typed_variant(type_parameters).is_none() {
                drop(guard);
                self.insert_typed_variant(type_parameters.to_vec(), root, symbol.clone())?;
            }
        }
        Self::get_typed_syntax_element(self, &symbol, type_parameters)
            .map(|syntax_element| syntax_element.symbol_owned())
    }

    pub fn get_pre_implementation<'b>(
        &'b self,
        symbol: &<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Ref<'b, Element::PreImplementation>> {
        Ref::filter_map(
            self.get_typed_syntax_element(symbol, type_parameters)?,
            |element| element.pre_implementation(),
        )
        .ok()
    }

    pub fn get_typed_syntax_element<'b>(
        &'b self,
        symbol: &<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Ref<'b, TypedSyntaxElement<'a, Element>>> {
        Ref::filter_map(self.elements.get(symbol)?.borrow(), |guard| {
            guard.typed_variant(type_parameters)
        })
        .ok()
    }

    pub fn get_typed_syntax_element_mut<'b>(
        &'b self,
        symbol: &<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<RefMut<'b, TypedSyntaxElement<'a, Element>>> {
        RefMut::<'_, SyntaxElementWithTypeParameterTranslator<'a, 'a, Element>>::filter_map(
            self.elements.get(symbol)?.borrow_mut(),
            |guard| guard.typed_variant_mut(type_parameters),
        )
        .ok()
    }

    fn insert_typed_variant(
        &self,
        typed_type_parameters: Vec<TypedTypeParameter>,
        root: &SyntaxElementMap<'a>,
        symbol: Rc<<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>>,
    ) -> Option<()> {
        let guard = self.elements.get(&symbol)?.borrow_mut();
        let type_parameters = Rc::new(TypeParameterContext::new(
            guard.in_context_type_parameters(),
            Rc::from(typed_type_parameters.clone()),
        ));
        let ast_reference = guard.ast_reference().clone();
        drop(guard);
        let typed_variant =
            TypedSyntaxElement::new(type_parameters.clone(), ast_reference.clone(), root)?;
        let typed_symbol = typed_variant.symbol_owned();

        let mut guard = self.elements.get(&symbol)?.borrow_mut();
        guard.insert_typed_variant(typed_variant, typed_type_parameters.clone());
        drop(guard);

        let context = SyntaxContext::new(root, type_parameters, ast_reference.clone());
        let pre_implementation = Element::generate_pre_implementation(&context)?;

        let mut guard = self.elements.get(&symbol)?.borrow_mut();
        let typed_variant = guard.typed_variant_mut(&typed_type_parameters)?;
        typed_variant.set_pre_implementation(pre_implementation.clone());
        drop(guard);

        let implementation =
            Element::generate_implementation(typed_symbol.clone(), pre_implementation, &context)?;

        let mut guard = self.elements.get(&symbol)?.borrow_mut();
        let typed_variant = guard.typed_variant_mut(&typed_type_parameters)?;
        typed_variant.set_implementation(implementation);
        self.untyped_symbols
            .borrow_mut()
            .insert(typed_symbol, symbol);
        Some(())
    }

    pub fn implementations_for_untyped_symbol(
        &mut self,
        symbol: &Element::Symbol<UntypedAST>,
    ) -> Option<impl Iterator<Item = (Element::Implementation, Element::SubAnalyzables<'a>)>> {
        self.elements
            .remove(symbol)
            .map(|implement| implement.into_inner().into_implementations())
    }

    pub fn implementations(
        self,
    ) -> impl Iterator<Item = (Element::Implementation, Element::SubAnalyzables<'a>)> {
        self.elements
            .into_values()
            .flat_map(|guard| guard.into_inner().into_implementations())
    }

    pub fn untyped_from_typed_symbol(
        &self,
        typed_symbol: &Element::Symbol<TypedAST>,
    ) -> Option<Rc<Element::Symbol<UntypedAST>>> {
        self.untyped_symbols.borrow().get(typed_symbol).cloned()
    }
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> Default
    for SingleSyntaxElementMap<'a, Element>
{
    fn default() -> Self {
        Self::new_root()
    }
}
