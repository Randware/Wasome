use std::collections::HashMap;
use std::rc::Rc;
use ast::data_type::UntypedDataType;
use ast::{TypedAST, UntypedAST};
use ast::symbol::FunctionSymbol;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use crate::symbol_translation::{AnalyzableFunction, AnalyzableSyntaxElementWithTypeParameter, RegularTypeParameterContext, SyntaxContext};
use crate::symbol_translation::syntax_element_with_type_parameter_guard::{SyntaxElementWithTypeParameterGuard, TypedSyntaxElement};

pub(crate) struct SyntaxElementMap<'a> {
    functions: SingleSyntaxElementMap<'a, AnalyzableFunction>
}

impl<'a> SyntaxElementMap<'a> {
    pub fn new() -> Self {
        Self { functions: SingleSyntaxElementMap::new() }
    }

    pub fn get_typed_function_symbol(&mut self, symbol: &FunctionSymbol<UntypedAST>, type_parameters: &[UntypedDataType]) -> Option<Rc<FunctionSymbol<TypedAST>>> {
        let mut single_and_root = SingleAndRoot::new(|root| &root.functions, | root| &mut root.functions, self);
        SingleSyntaxElementMap::get_typed_symbol(&mut single_and_root, symbol, type_parameters)
    }

    pub fn insert_untyped_function(&mut self, to_insert: FunctionTraversalHelper<'a, 'a, UntypedAST>) -> Option<()> {
        self.functions.insert_untyped_element(to_insert)
    }
}

impl<'a> Default for SyntaxElementMap<'a> {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct SingleSyntaxElementMap<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    elements: HashMap<Rc<Element::Symbol<UntypedAST>>, SyntaxElementWithTypeParameterGuard<'a, 'a, Element>>,
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> SingleSyntaxElementMap<'a, Element> {
    pub fn new() -> Self {
        Self { elements: HashMap::new() }
    }

    pub fn insert_untyped_element(&mut self, to_insert: Element::ASTReference<'a, 'a>) -> Option<()> {
        let untyped_symbol = Element::load_untyped_symbol(&to_insert);
        if self.elements.contains_key(&untyped_symbol) {
            return None
        }
        let guard = SyntaxElementWithTypeParameterGuard::new(untyped_symbol.clone(), to_insert);
        self.elements.insert(untyped_symbol, guard);
        Some(())
    }

    pub fn get_typed_symbol(root: &mut SingleAndRoot<Element>, symbol: &Element::Symbol<UntypedAST>, type_parameters: &[UntypedDataType]) -> Option<Rc<Element::Symbol<TypedAST>>>{
        let guard = root.single_mut().elements.get_mut(symbol)?;
        if guard.typed_variant(type_parameters).is_none() {
            Self::insert_typed_variant(root, symbol, type_parameters)?;
        }
        // Recreate guard to prevent borrowing issues
        let guard = root.single_mut().elements.get_mut(symbol)?;
        guard.typed_variant(type_parameters).map(|var| var.symbol_owned())
    }

    fn insert_typed_variant(root: &mut SingleAndRoot<Element>, symbol: &<Element as AnalyzableSyntaxElementWithTypeParameter>::Symbol<UntypedAST>, type_parameters: &[UntypedDataType]) -> Option<()> {
        let guard = root.single_mut().elements.get_mut(symbol)?;
        let typed_type_parameters: Vec<TypedTypeParameter> = gen_t(); // TODO
        let ast_reference = guard.ast_reference().clone();
        let typed_variant = TypedSyntaxElement::new(&typed_type_parameters, type_parameters, ast_reference.clone(), root.root())?;
        let untyped_type_parameters = typed_variant.untyped_type_parameters_owned();
        let typed_type_parameters = typed_variant.typed_type_parameters_owned();

        let guard = root.single_mut().elements.get_mut(symbol)?;
        guard.insert_typed_variant(typed_variant, type_parameters.to_vec());


        let type_parameter_context = RegularTypeParameterContext::new(typed_type_parameters.clone(), untyped_type_parameters.clone());
        let context = SyntaxContext::new(root.root(), type_parameter_context, ast_reference.clone());
        let pre_implementation = Element::generate_pre_implementation(context)?;

        let guard = root.single_mut().elements.get_mut(symbol)?;
        let typed_variant = guard.typed_variant_mut(type_parameters)?;
        typed_variant.set_pre_implementation(pre_implementation.clone());


        let type_parameter_context = RegularTypeParameterContext::new(typed_type_parameters.clone(), untyped_type_parameters);
        let context = SyntaxContext::new(root.root(), type_parameter_context, ast_reference);
        let implementation = Element::generate_implementation(pre_implementation, context)?;

        let guard = root.single_mut().elements.get_mut(symbol)?;
        let typed_variant = guard.typed_variant_mut(type_parameters)?;
        typed_variant.set_implementation(implementation);
        Some(())
    }
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> Default for SingleSyntaxElementMap<'a, Element> {
    fn default() -> Self {
        Self::new()
    }
}

// Temp to make code readable
fn gen_t<T>() -> T {
    todo!()
}
type SingleFromMap<'b, Element> = for<'a> fn(&'a SyntaxElementMap<'b>) -> &'a SingleSyntaxElementMap<'b,Element>;
type SingleFromMapMut<'b, Element> = for<'a> fn(&'a mut SyntaxElementMap<'b>) -> &'a mut SingleSyntaxElementMap<'b,Element>;

struct SingleAndRoot<'a, 'b, Element: AnalyzableSyntaxElementWithTypeParameter> {
    single: SingleFromMap<'b, Element>,
    single_mut: SingleFromMapMut<'b, Element>,
    root: &'a mut SyntaxElementMap<'b>
}

impl<'a, 'b, Element: AnalyzableSyntaxElementWithTypeParameter> SingleAndRoot<'a, 'b, Element> {
    pub fn new(single: SingleFromMap<'b, Element>, single_mut: SingleFromMapMut<'b, Element>, root: &'a mut SyntaxElementMap<'b>) -> Self {
        Self { single, single_mut, root }
    }

    pub fn root(&mut self) -> &mut SyntaxElementMap<'b> {
        self.root
    }

    pub fn single(&self) -> &SingleSyntaxElementMap<'b,Element> {
        (self.single)(&self.root)
    }

    pub fn single_mut(&mut self) -> &mut SingleSyntaxElementMap<'b,Element> {
        (self.single_mut)(&mut self.root)
    }
}