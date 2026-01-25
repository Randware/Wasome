use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use ast::data_type::UntypedDataType;
use ast::{TypedAST, UntypedAST};
use ast::type_parameter::TypedTypeParameter;
use crate::symbol_translation::syntax_element_with_type_parameter_guard::{AnalyzableFunction, AnalyzableSyntaxElementWithTypeParameter, SyntaxElementWithTypeParameterGuard, TypedSyntaxElement};

pub(crate) struct SyntaxElementMap<'a> {
    functions: SingleSyntaxElementMap<'a, AnalyzableFunction>
}

pub(crate) struct SingleSyntaxElementMap<'a, Element: AnalyzableSyntaxElementWithTypeParameter> {
    elements: HashMap<Rc<Element::Symbol<UntypedAST>>, SyntaxElementWithTypeParameterGuard<'a, 'a, Element>>,
    // TODO: Do we need this?
    implementations_to_create: VecDeque<(Rc<Element::Symbol<UntypedAST>>, Vec<UntypedDataType>)>,
}

impl<'a, Element: AnalyzableSyntaxElementWithTypeParameter> SingleSyntaxElementMap<'a, Element> {
    pub fn new() -> Self {
        Self { elements: HashMap::new(), implementations_to_create: VecDeque::new() }
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
        let typed_variant = TypedSyntaxElement::new(&typed_type_parameters, &ast_reference, root.root())?;

        let guard = root.single_mut().elements.get_mut(symbol)?;
        guard.insert_typed_variant(typed_variant, type_parameters.to_vec());
        let pre_implementation = Element::generate_pre_implementation(&typed_type_parameters, &ast_reference, root.root())?;

        let guard = root.single_mut().elements.get_mut(symbol)?;
        let typed_variant = guard.typed_variant_mut(type_parameters)?;
        typed_variant.set_pre_implementation(pre_implementation);
        // TODO: Implementation
        Some(())
    }
}

// Temp to make code readable
fn gen_t<T>() -> T {
    todo!()
}
type SingleFromMap<'b, Element> = for<'a> fn(&'a SyntaxElementMap) -> &'a SingleSyntaxElementMap<'b,Element>;
type SingleFromMapMut<'b, Element> = for<'a> fn(&'a mut SyntaxElementMap) -> &'a mut SingleSyntaxElementMap<'b,Element>;

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