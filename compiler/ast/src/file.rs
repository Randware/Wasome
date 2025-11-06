use std::path::PathBuf;
use crate::symbol::{FunctionSymbol, Symbol};
use crate::top_level::{Function, Import};
use crate::visibility::{Visibility, Visible};
use crate::{ASTNode, ASTType, SemanticEquality};
use crate::composite::{Enum, Struct};

#[derive(Debug, PartialEq)]
pub struct File<Type: ASTType> {
    /// Filename without the file extension
    name: String,
    imports: Vec<ASTNode<Import>>,
    functions: Vec<ASTNode<Function<Type>>>,
    enums: Vec<ASTNode<Enum<Type>>>,
    structs: Vec<ASTNode<Struct<Type>>>
}

impl<Type: ASTType> File<Type> {
    pub fn new(
        name: String,
        imports: Vec<ASTNode<Import>>,
        functions: Vec<ASTNode<Function<Type>>>,
        enums: Vec<ASTNode<Enum<Type>>>,
        structs: Vec<ASTNode<Struct<Type>>>
    ) -> Self {
        Self {
            name,
            imports,
            functions,
            enums,
            structs
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn imports(&self) -> &[ASTNode<Import>] {
        &self.imports
    }

    pub fn functions(&self) -> &[ASTNode<Function<Type>>] {
        &self.functions
    }

    pub fn enums(&self) -> &Vec<ASTNode<Enum<Type>>> {
        &self.enums
    }

    pub fn structs(&self) -> &Vec<ASTNode<Struct<Type>>> {
        &self.structs
    }

    /** Gets the symbol with the specified name
     */
    pub fn symbol(&self, origin: &[String]) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(origin, false)
    }

    /** Gets the symbol with the specified name if it is public
     */
    pub fn symbol_public(&self, origin: &[String]) -> Option<Symbol<'_, Type>> {
        self.symbol_specified_origin(origin, true)
    }

    /** Gets the symbol with the specified origin if it is public or outside is false
     */
    fn symbol_specified_origin(&self, origin: &[String], outside: bool) -> Option<Symbol<'_, Type>> {
        // Symbols can be a direct function...
        self.function_symbol(origin.get(0)?, outside)
            .map(|function_symbol| Symbol::Function(function_symbol))
            // ... or a function in a struct ...
            .or_else(|| Some(Symbol::Function(self.struct_by_name(&origin[0])?.function_symbol(origin.get(1)?, outside)?)))
            // ... or a struct
            .or_else(|| Some(Symbol::Struct(self.struct_by_name(&origin[0])?.symbol())))
            // ... or a variant of an enum ...
            .or_else(|| Some(Symbol::EnumVariant(self.enum_by_name(&origin[0])?.variant_by_name(&origin.get(1)?)?.inner())))
            // ... or a struct
            .or_else(|| Some(Symbol::Enum(self.enum_by_name(&origin[0])?.symbol())))
    }

    /** Gets the function with the specified name
     */
    pub fn function_by_name(&self, name: &str) -> Option<&ASTNode<Function<Type>>> {
        self.functions()
            .iter()
            .find(|function| function.declaration().name() == name)
    }

    /** Gets the function with the specified name if it is public or only_public is false
     */
    fn function_symbol(&self, name: &str, only_public: bool) -> Option<&FunctionSymbol<Type>> {
        self.function_by_name(name)
            .filter(|function| !only_public || function.visibility() == Visibility::Public)
            .map(|function| function.declaration())
    }

    /** Gets the struct with the specified name
    */
    pub fn struct_by_name(&self, name: &str) -> Option<&ASTNode<Struct<Type>>> {
        self.structs()
            .iter()
            .find(|st| st.symbol().name() == name)
    }

    /** Gets the enum with the specified name
    */
    pub fn enum_by_name(&self, name: &str) -> Option<&ASTNode<Enum<Type>>> {
        self.enums()
            .iter()
            .find(|en| en.symbol().name() == name)
    }

    /** Gets an iterator over all enums
    */
    pub fn enums_iterator(&self) -> impl Iterator<Item = &ASTNode<Enum<Type>>> {
        self.enums().iter()
    }

    /** Gets an iterator over all structs
    */
    pub fn structs_iterator(&self) -> impl Iterator<Item = &ASTNode<Struct<Type>>> {
        self.structs().iter()
    }
}

impl<Type: ASTType> SemanticEquality for File<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.imports().semantic_equals(other.imports())
            && self.functions().semantic_equals(other.functions())
    }
}

/** This represents all composites <br>
inside a [File]
*/
pub struct Composites<Type: ASTType>
{
    enums: Vec<ASTNode<Enum<Type>>>,
    structs: Vec<ASTNode<Struct<Type>>>
}

impl<Type: ASTType> Composites<Type>
{
    pub fn new(enums: Vec<ASTNode<Enum<Type>>>, structs: Vec<ASTNode<Struct<Type>>>) -> Self {
        Self { enums, structs }
    }

    pub fn enums(&self) -> &Vec<ASTNode<Enum<Type>>> {
        &self.enums
    }

    pub fn structs(&self) -> &Vec<ASTNode<Struct<Type>>> {
        &self.structs
    }
}