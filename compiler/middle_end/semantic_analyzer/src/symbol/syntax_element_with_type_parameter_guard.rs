use crate::symbol::syntax_element_map::SyntaxElementMap;
use crate::symbol::{
    AnalyzableSyntaxElementWithTypeParameter, RegularTypeParameterContext, SyntaxContext,
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
    typed: HashMap<Vec<UntypedDataType>, TypedSyntaxElement<'a, 'b, Element>>,
    ast_reference: Element::ASTReference<'a, 'b>,
}

impl<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter>
    SyntaxElementWithTypeParameterGuard<'a, 'b, Element>
{
    pub fn new(
        untyped_symbol: Rc<Element::Symbol<UntypedAST>>,
        ast_reference: Element::ASTReference<'a, 'b>,
    ) -> Self {
        Self {
            untyped_symbol,
            typed: HashMap::new(),
            ast_reference,
        }
    }

    pub fn untyped_symbol(&self) -> &Rc<Element::Symbol<UntypedAST>> {
        &self.untyped_symbol
    }

    pub fn typed_variant(
        &self,
        type_parameter: &[UntypedDataType],
    ) -> Option<&TypedSyntaxElement<'a, 'b, Element>> {
        self.typed.get(type_parameter)
    }

    pub fn typed_variant_mut(
        &mut self,
        type_parameter: &[UntypedDataType],
    ) -> Option<&mut TypedSyntaxElement<'a, 'b, Element>> {
        self.typed.get_mut(type_parameter)
    }

    pub fn insert_typed_variant(
        &mut self,
        to_insert: TypedSyntaxElement<'a, 'b, Element>,
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

    pub fn into_implementations(self) -> impl Iterator<Item = Element::Implementation> {
        self.typed
            .into_iter()
            .filter_map(|typed| typed.1.into_implementation())
    }
}

pub(crate) struct TypedSyntaxElement<'a, 'b: 'a ,Element: AnalyzableSyntaxElementWithTypeParameter> {
    typed_type_parameters: Rc<[TypedTypeParameter]>,
    untyped_type_parameters: Rc<[UntypedDataType]>,
    symbol: Rc<Element::Symbol<TypedAST>>,
    pre_implementation: Option<Element::PreImplementation>,
    implementation: Option<Element::Implementation>,
    subanalyzables: Element::SubAnalyzables<'a, 'b>
}

impl<'a, 'b: 'a, Element: AnalyzableSyntaxElementWithTypeParameter> TypedSyntaxElement<'a, 'b, Element> {
    pub fn new(
        typed_type_parameters: &[TypedTypeParameter],
        untyped_typed_parameters: &[UntypedDataType],
        from: <Element as AnalyzableSyntaxElementWithTypeParameter>::ASTReference<'a, 'b>,
        global_elements: &mut SyntaxElementMap<'b>,
    ) -> Option<Self> {
        let typed_type_parameters: Rc<[TypedTypeParameter]> = Rc::from(typed_type_parameters);
        let untyped_type_parameters: Rc<[UntypedDataType]> = Rc::from(untyped_typed_parameters);
        let type_parameter_context =
            RegularTypeParameterContext::new(typed_type_parameters.clone());
        let subanalyzables = Element::init_subanalyzables(&from);
        let context = SyntaxContext::new(global_elements, type_parameter_context, from);
        let symbol = Element::generate_typed_symbol(context)?;
        Some(Self {
            typed_type_parameters,
            untyped_type_parameters,
            symbol,
            pre_implementation: None,
            implementation: None,
            subanalyzables
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

    pub fn into_implementation(self) -> Option<Element::Implementation> {
        self.implementation
    }
}
