use crate::error_sa::SemanticError;
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
        span: source::types::Span,
    ) -> Result<Rc<FunctionSymbol<TypedAST>>, SemanticError> {
        self.functions
            .get_or_insert_typed_symbol(self, symbol, type_parameters, span)
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
        span: source::types::Span,
    ) -> Result<Rc<FunctionSymbol<TypedAST>>, SemanticError> {
        let struct_of_method = self
            .structs
            .get_typed_syntax_element_mut(from, from_type_parameters)
            .ok_or_else(|| SemanticError::UnknownSymbol {
                name: from.name().to_string(),
                span,
            })?;

        let methods = struct_of_method.subanalyzables();
        methods.get_or_insert_typed_symbol(self, symbol, type_parameters, span)
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
        span: source::types::Span,
    ) -> Result<Rc<EnumSymbol<TypedAST>>, SemanticError> {
        self.enums
            .get_or_insert_typed_symbol(self, symbol, type_parameters, span)
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
        span: source::types::Span,
    ) -> Result<Rc<StructSymbol<TypedAST>>, SemanticError> {
        self.structs
            .get_or_insert_typed_symbol(self, symbol, type_parameters, span)
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
    pub fn fill(&self) -> Result<(), SemanticError> {
        let funcs = self.functions.untyped_elements();
        for func in funcs
            .into_iter()
            .filter(|func| func.as_ref().type_parameters().is_empty())
        {
            let span = *self
                .functions
                .elements
                .get(&func)
                .unwrap()
                .borrow()
                .ast_reference()
                .inner()
                .position();
            self.get_or_insert_typed_function_symbol(func.clone(), &[], span)?;
        }
        Ok(())
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
    ) -> Option<impl Iterator<Item = StructImplementation<'a>>> {
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

pub(crate) struct SingleSyntaxElementMap<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    elements: HashMap<
        Rc<Element::Symbol<UntypedAST>>,
        RefCell<SyntaxElementWithTypeParameterTranslator<'a, 'a, Element>>,
    >,
    untyped_symbols: RefCell<UntypedTypeParameterMap<Element>>,
    type_parameters: Option<Rc<TypeParameterContext>>,
}

#[allow(type_alias_bounds)]
pub(crate) type UntypedTypeParameterMap<Element: AnalyzableSyntaxElementWithTypeParameter> =
    HashMap<Rc<Element::Symbol<TypedAST>>, Rc<Element::Symbol<UntypedAST>>>;

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
    pub fn get_or_insert_typed_symbol<'b>(
        &self,
        root: &'b SyntaxElementMap<'a>,
        symbol: Rc<Element::Symbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
        span: source::types::Span,
    ) -> Result<Rc<Element::Symbol<TypedAST>>, SemanticError> {
        let mut insert_needed = false;
        {
            let guard = match self.elements.get(&symbol) {
                Some(g) => g.borrow(),
                None => {
                    return Err(SemanticError::UnknownSymbol {
                        name: symbol.name().to_string(),
                        span,
                    });
                }
            };

            if guard.typed_variant(type_parameters).is_none() {
                insert_needed = true;
            }
        }

        if insert_needed {
            self.insert_typed_variant(type_parameters.to_vec(), root, symbol.clone(), span)?;
        }

        Ok(
            Self::get_typed_syntax_element(self, &symbol, type_parameters)
                .unwrap()
                .symbol_owned(),
        )
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
        span: source::types::Span,
    ) -> Result<(), SemanticError> {
        let guard_opt = self.elements.get(&symbol);
        if guard_opt.is_none() {
            return Err(SemanticError::UnknownSymbol {
                name: symbol.name().to_string(),
                span,
            });
        }

        let (in_context_type_parameters, ast_reference) = {
            let guard = guard_opt.unwrap().borrow();
            (
                guard.in_context_type_parameters(),
                guard.ast_reference().clone(),
            )
        };

        let type_parameters = Rc::new(TypeParameterContext::new(
            in_context_type_parameters,
            Rc::from(typed_type_parameters.clone()),
        ));

        let typed_variant =
            TypedSyntaxElement::new(type_parameters.clone(), ast_reference.clone(), root)?;
        let typed_symbol = typed_variant.symbol_owned();

        {
            let mut guard = self.elements.get(&symbol).unwrap().borrow_mut();
            guard.insert_typed_variant(typed_variant, typed_type_parameters.clone());
        }

        let context = SyntaxContext::new(root, type_parameters, ast_reference.clone());
        let pre_implementation = Element::generate_pre_implementation(&context)?;

        {
            let mut guard = self.elements.get(&symbol).unwrap().borrow_mut();
            let typed_variant = guard.typed_variant_mut(&typed_type_parameters).unwrap();
            typed_variant.set_pre_implementation(pre_implementation.clone());
        }

        let implementation =
            Element::generate_implementation(typed_symbol.clone(), pre_implementation, &context)?;

        {
            let mut guard = self.elements.get(&symbol).unwrap().borrow_mut();
            let typed_variant = guard.typed_variant_mut(&typed_type_parameters).unwrap();
            typed_variant.set_implementation(implementation);
        }

        self.untyped_symbols
            .borrow_mut()
            .insert(typed_symbol, symbol);

        Ok(())
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
