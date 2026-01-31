use crate::symbol::syntax_element_map::SyntaxElementMap;
use crate::symbol::syntax_element_map::SingleSyntaxElementMap;
use crate::symbol::{
    AnalyzableSyntaxElementWithTypeParameter, SyntaxContext, TypeParameterContext,
};
use ast::symbol::SymbolWithTypeParameter;
use ast::type_parameter::TypedTypeParameter;
use ast::{TypedAST, UntypedAST};
use std::collections::HashMap;
use std::rc::Rc;


/// Translates untyped syntax elements to typed syntax elements
///
/// This may have a parent element.
///     - Note that currently, it is not explicitly stored and only influences creation behavior
///     - It is currently only used for methods
///
/// See [`SyntaxElementMap`] for more info
pub(crate) struct SyntaxElementWithTypeParameterTranslator<
    'a,
    'b: 'a,
    Element: AnalyzableSyntaxElementWithTypeParameter,
> {
    /// The symbol of the untyped element
    untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
    /// All typed elements
    typed: HashMap<Vec<TypedTypeParameter>, TypedSyntaxElement<'a, Element>>,
    /// A reference to the AST part of the element
    ast_reference: Element::ASTReference<'a, 'b>,
    /// All type parameters available from a parent element
    /// Currently only used for methods
    in_context_type_parameters: Option<Rc<TypeParameterContext>>,
}

impl<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter>
    SyntaxElementWithTypeParameterTranslator<'a, 'b, Element>
{
    /// Creates a new empty instance
    pub fn new(
        untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
        ast_reference: Element::ASTReference<'a, 'b>,
        in_context_type_parameters: Option<Rc<TypeParameterContext>>,
    ) -> Self {
        Self {
            untyped_symbol,
            typed: HashMap::new(),
            ast_reference,
            in_context_type_parameters,
        }
    }

    /// Gets the typed variant with the provided type parameters
    pub fn typed_variant(
        &self,
        type_parameter: &[TypedTypeParameter],
    ) -> Option<&TypedSyntaxElement<'a, Element>> {
        self.typed.get(type_parameter)
    }

    /// Like [`Self::typed_variant`], but with mutable references
    pub fn typed_variant_mut(
        &mut self,
        type_parameter: &[TypedTypeParameter],
    ) -> Option<&mut TypedSyntaxElement<'a, Element>> {
        self.typed.get_mut(type_parameter)
    }

    /// Inserts a typed variant with the provided type parameters
    pub fn insert_typed_variant(
        &mut self,
        to_insert: TypedSyntaxElement<'a, Element>,
        type_parameters: Vec<TypedTypeParameter>,
    ) -> Option<()> {
        // TODO: Can we take the type parameters from the symbol?
        let to_insert_type_parameters = to_insert.symbol().type_parameters();
        if self.cnt_type_params() != to_insert_type_parameters.len() {
            return None;
        }
        if self.cnt_type_params() != type_parameters.len() {
            return None;
        }
        // Prevent overriding
        if self.typed_variant(&type_parameters).is_some() {
            return None;
        }
        self.typed.insert(type_parameters, to_insert);
        Some(())
    }

    /// Gets the count of type parameters
    ///
    /// Note that this works as both typed and untyped instances of the same syntax element must
    /// have the same number of type parameters
    fn cnt_type_params(&self) -> usize {
        self.untyped_symbol.type_parameters().len()
    }

    /// Gets a reference to the ast part that represents the element being translated
    pub fn ast_reference(&self) -> &Element::ASTReference<'a, 'b> {
        &self.ast_reference
    }

    /// Gets an iterator over all typed implementations
    ///
    /// This is only supposed to be used in the final step of semantic analysis as self is consumed
    /// in the process
    pub fn into_implementations(
        self,
    ) -> impl Iterator<Item = (Element::Implementation, Element::SubAnalyzables<'a>)> {
        self.typed.into_iter().filter_map(|typed| {
            let implementation = typed.1.into_implementation();
            Some((implementation.0?, implementation.1))
        })
    }

    /// Gets all type parameters available to the current element from external sources (e.g.: a parent element)
    pub fn in_context_type_parameters(&self) -> Option<Rc<TypeParameterContext>> {
        self.in_context_type_parameters.clone()
    }
}

/// A single typed syntax element
///
/// See [`SingleSyntaxElementMap`] for more information
pub(crate) struct TypedSyntaxElement<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    symbol: Rc<Element::Symbol<TypedAST>>,
    pre_implementation: Option<Element::PreImplementation>,
    implementation: Option<Element::Implementation>,
    subanalyzables: Element::SubAnalyzables<'a>,
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> TypedSyntaxElement<'a, Element> {
    /// Creates a new instance
    ///
    /// The symbol is created and the subanalyzables are initialized in the process
    pub fn new(
        type_parameters: Rc<TypeParameterContext>,
        from: <Element as AnalyzableSyntaxElementWithTypeParameter>::ASTReference<'a, 'a>,
        global_elements: &SyntaxElementMap<'a>,
    ) -> Option<Self> {
        let context = SyntaxContext::new(global_elements, type_parameters.clone(), from);
        let subanalyzables = Element::init_subanalyzables(&context);
        let symbol = Element::generate_typed_symbol(&context)?;
        Some(Self {
            symbol,
            pre_implementation: None,
            implementation: None,
            subanalyzables,
        })
    }

    pub fn set_pre_implementation(
        &mut self,
        pre_implementation: Element::PreImplementation,
    ) -> Option<()> {
        if self.pre_implementation.is_none() {
            self.pre_implementation.replace(pre_implementation);
            Some(())
        } else {
            None
        }
    }

    pub fn set_implementation(&mut self, implementation: Element::Implementation) -> Option<()> {
        if self.implementation.is_none() {
            self.implementation.replace(implementation);
            Some(())
        } else {
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

    pub fn into_implementation(
        self,
    ) -> (Option<Element::Implementation>, Element::SubAnalyzables<'a>) {
        (self.implementation, self.subanalyzables)
    }

    pub fn subanalyzables(&self) -> &Element::SubAnalyzables<'a> {
        &self.subanalyzables
    }
}
