use crate::symbol::syntax_element_map::SyntaxElementMap;
use crate::symbol::{
    AnalyzableSyntaxElementWithTypeParameter, SyntaxContext, TypeParameterContext,
};
use ast::data_type::UntypedDataType;
use ast::symbol::SymbolWithTypeParameter;
use ast::type_parameter::TypedTypeParameter;
use ast::{TypedAST, UntypedAST};
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) struct SyntaxElementWithTypeParameterGuard<
    'a,
    'b: 'a,
    Element: AnalyzableSyntaxElementWithTypeParameter,
> {
    untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
    // We could use typed, this would be quicker to create and consume less memory, but would
    // require translating from untyped for each lookup
    // Potential performance improvement: Use a different HashMap for translating from untyped to typed
    // and then lookup with that
    typed: HashMap<Vec<UntypedDataType>, TypedSyntaxElement<'a, Element>>,
    ast_reference: Element::ASTReference<'a, 'b>,
    in_context_type_parameters: Option<Rc<TypeParameterContext>>,
}

impl<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter>
    SyntaxElementWithTypeParameterGuard<'a, 'b, Element>
{
    pub fn new_root(
        untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
        ast_reference: Element::ASTReference<'a, 'b>,
    ) -> Self {
        Self {
            untyped_symbol,
            typed: HashMap::new(),
            ast_reference,
            in_context_type_parameters: None,
        }
    }

    pub fn new_child(
        untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
        ast_reference: Element::ASTReference<'a, 'b>,
        in_context_type_parameters: Rc<TypeParameterContext>,
    ) -> Self {
        Self {
            untyped_symbol,
            typed: HashMap::new(),
            ast_reference,
            in_context_type_parameters: Some(in_context_type_parameters),
        }
    }

    pub fn new(
        untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
        typed: HashMap<Vec<UntypedDataType>, TypedSyntaxElement<'a, Element>>,
        ast_reference: Element::ASTReference<'a, 'b>,
        in_context_type_parameters: Option<Rc<TypeParameterContext>>,
    ) -> Self {
        Self {
            untyped_symbol,
            typed,
            ast_reference,
            in_context_type_parameters,
        }
    }

    pub fn untyped_symbol(&self) -> &Rc<Element::Symbol<UntypedAST>> {
        &self.untyped_symbol
    }

    pub fn typed_variant(
        &self,
        type_parameter: &[UntypedDataType],
    ) -> Option<&TypedSyntaxElement<'a, Element>> {
        self.typed.get(type_parameter)
    }

    pub fn typed_variant_mut(
        &mut self,
        type_parameter: &[UntypedDataType],
    ) -> Option<&mut TypedSyntaxElement<'a, Element>> {
        self.typed.get_mut(type_parameter)
    }

    pub fn insert_typed_variant(
        &mut self,
        to_insert: TypedSyntaxElement<'a, Element>,
        untyped_type_parameters: Vec<UntypedDataType>,
    ) -> Option<()> {
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

    pub fn into_implementations(self) -> impl Iterator<Item = (Element::Implementation, Element::SubAnalyzables<'a>)> {
        self.typed
            .into_iter()
            .filter_map(|typed| {
                let implementation = typed.1.into_implementation();
                Some((implementation.0?, implementation.1))
            })
    }

    pub fn in_context_type_parameters(&self) -> Option<Rc<TypeParameterContext>> {
        self.in_context_type_parameters.clone()
    }
}

pub(crate) struct TypedSyntaxElement<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    type_parameters: Rc<TypeParameterContext>,
    symbol: Rc<Element::Symbol<TypedAST>>,
    pre_implementation: Option<Element::PreImplementation>,
    implementation: Option<Element::Implementation>,
    subanalyzables: Element::SubAnalyzables<'a>,
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> TypedSyntaxElement<'a, Element> {
    pub fn new(
        type_parameters: Rc<TypeParameterContext>,
        untyped_type_parameters: Rc<[UntypedDataType]>,
        from: <Element as AnalyzableSyntaxElementWithTypeParameter>::ASTReference<'a, 'a>,
        global_elements: &SyntaxElementMap<'a>,
    ) -> Option<Self> {
        let context = SyntaxContext::new(global_elements, type_parameters.clone(), from);
        let subanalyzables = Element::init_subanalyzables(&context, untyped_type_parameters);
        let symbol = Element::generate_typed_symbol(&context)?;
        Some(Self {
            type_parameters,
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

    pub fn implementation(&self) -> Option<&Element::Implementation> {
        self.implementation.as_ref()
    }

    pub fn typed_type_parameters(&self) -> &[TypedTypeParameter] {
        self.type_parameters.current()
    }

    pub fn into_implementation(self) -> (Option<Element::Implementation>, Element::SubAnalyzables<'a>) {
        (self.implementation, self.subanalyzables)
    }

    pub fn subanalyzables(&self) -> &Element::SubAnalyzables<'a> {
        &self.subanalyzables
    }

    pub fn subanalyzables_mut(&mut self) -> &mut Element::SubAnalyzables<'a> {
        &mut self.subanalyzables
    }
}
