use crate::symbol::syntax_element_with_type_parameter_guard::{
    SyntaxElementWithTypeParameterGuard, TypedSyntaxElement,
};
use crate::symbol::{AnalyzableEnum, AnalyzableFunction, AnalyzableMethod, AnalyzableStruct, AnalyzableSyntaxElementWithTypeParameter, SyntaxContext, TypeParameterContext};
use ast::data_type::UntypedDataType;
use ast::symbol::{EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructFieldSymbol, StructSymbol, SymbolWithTypeParameter};
use ast::top_level::Function;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use ast::{ASTNode, TypedAST, UntypedAST};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use ast::composite::{Enum, EnumVariant, StructField};

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
        self.functions.get_or_insert_typed_symbol(
            self,
            symbol,
            type_parameters,
        )
    }

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
        self.structs.get_or_insert_typed_symbol(
            self,
            symbol,
            type_parameters,
        )
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

    pub fn fill(&self) -> Option<()> {
        let mut ok = Some(());
        // Collect to break off the borrow chain
        let funcs = self.functions.untyped_elements().collect::<Vec<_>>();
        funcs
            .into_iter()
            .filter(|func| func.as_ref().type_parameters().is_empty())
            .for_each(|func| {
                // Mutable borrow required here
                let typed_symbol =
                    self.get_or_insert_typed_function_symbol(func, &[]);
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
        self.functions.implementations_for_untyped_symbol(symbol).map(|funcs| funcs.map(|func| func.0))
    }

    pub fn enum_implementations_for_untyped_symbol(
        &mut self,
        symbol: &EnumSymbol<UntypedAST>,
    ) -> Option<impl Iterator<Item = ASTNode<Enum<TypedAST>>>> {
        self.enums.implementations_for_untyped_symbol(symbol).map(|ens| ens.map(|en| en.0))
    }

    pub fn struct_implementations_for_untyped_symbol(
        &mut self,
        symbol: &StructSymbol<UntypedAST>,
    ) -> Option<impl Iterator<Item = ((Rc<StructSymbol<TypedAST>>, Vec<Rc<StructFieldSymbol<TypedAST>>>), SingleSyntaxElementMap<'a, AnalyzableMethod>)>> {
        self.structs.implementations_for_untyped_symbol(symbol)
    }
    
    pub fn untyped_struct_symbol_from_typed(&self, typed: &StructSymbol<TypedAST>) -> Option<Rc<StructSymbol<UntypedAST>>> {
        self.structs.untyped_from_typed_symbol(typed)
    }
}

impl<'a> Default for SyntaxElementMap<'a> {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct SingleSyntaxElementMap<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    elements: HashMap<
        Rc<Element::Symbol<UntypedAST>>,
        RefCell<SyntaxElementWithTypeParameterGuard<'a, 'a, Element>>,
    >,
    untyped_symbols: RefCell<HashMap<Rc<Element::Symbol<TypedAST>>, Rc<Element::Symbol<UntypedAST>>>>,
    type_parameters: Option<Rc<TypeParameterContext>>,
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> SingleSyntaxElementMap<'a, Element> {
    pub fn new_root() -> Self {
        Self {
            elements: HashMap::new(),
            type_parameters: None,
            untyped_symbols: RefCell::default()
        }
    }

    pub fn new_child(type_parameters: Rc<TypeParameterContext>) -> Self {
        Self {
            elements: HashMap::new(),
            type_parameters: Some(type_parameters),
            untyped_symbols: RefCell::default()
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
        let guard =
            SyntaxElementWithTypeParameterGuard::new_root(untyped_symbol.clone(), to_insert);
        self.elements.insert(untyped_symbol, RefCell::new(guard));
        Some(())
    }

    pub fn untyped_elements(&self) -> impl Iterator<Item = Rc<Element::Symbol<UntypedAST>>> {
        self.elements.keys().cloned()
    }

    pub fn get_or_insert_typed_symbol<'b>(
        &self,
        root: &'b SyntaxElementMap<'a>,
        symbol: Rc<Element::Symbol<UntypedAST>>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Rc<Element::Symbol<TypedAST>>> {
        {
            let guard = self.elements.get(&symbol)?.borrow_mut();

            if guard.typed_variant(&type_parameters).is_none() {
                self.insert_typed_variant(type_parameters.to_vec(), root, symbol.clone())?;
            }
        }
        Self::get_typed_syntax_element(self, &symbol, &type_parameters)
            .map(|syntax_element| syntax_element.symbol_owned())
    }

    pub fn get_pre_implementation<'b>(
        &'b self,
        symbol: &<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>,
        type_parameters: &[TypedTypeParameter],
    ) -> Option<Ref<'b, Element::PreImplementation>> {
        Ref::filter_map(self.get_typed_syntax_element(symbol, type_parameters)?, |element| element.pre_implementation()).ok()
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
        RefMut::<'_, SyntaxElementWithTypeParameterGuard<'a, 'a, Element>>::filter_map(
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
        self.untyped_symbols.borrow_mut().insert(typed_symbol, symbol);
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
        self.elements.into_values().map(|guard| guard.into_inner().into_implementations()).flatten()
    }
    
    pub fn untyped_from_typed_symbol(&self, typed_symbol: &Element::Symbol<TypedAST>) -> Option<Rc<Element::Symbol<UntypedAST>>> {
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
