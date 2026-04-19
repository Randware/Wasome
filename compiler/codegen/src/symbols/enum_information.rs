use ast::symbol::EnumVariantSymbol;
use ast::TypedAST;
use inkwell::types::StructType;
use inkwell::values::FunctionValue;
use std::rc::Rc;

/// Stores lowered LLVM representation and metadata for an enum type.
///
/// Each enum declaration from the AST is registered in the [`SymbolRegistry`] with a
/// corresponding [`EnumInformation`] instance. The enum's variant types are filled in
/// during the fill phase.
///
/// Each enum variant is represented as an LLVM struct with the following layout:
/// - Index 0: reference count (u32) - the reference count of the allocation
/// - Index 1: discriminant tag (u32) - the variant's index
/// - Index 2+: lowered field types - the variant's actual fields
#[derive(Eq, PartialEq)]
pub struct EnumInformation<'ctx> {
    /// The enum variants, each paired with its lowered LLVM struct type.
    ///
    /// Each tuple contains the variant symbol and its corresponding LLVM [`StructType`].
    /// The vector index corresponds to the discriminant tag value.
    ///
    /// Populated during the [`fill_enums`](crate::generators::compile_internal) phase.
    variants: Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)>,
    /// The `drop` function for this enum type.
    ///
    /// Called when the reference count reaches zero. Reads the discriminant tag to determine
    /// which variant is active, then recursively drops the variant's fields and frees memory.
    on_drop: FunctionValue<'ctx>,
}

impl<'ctx> EnumInformation<'ctx> {
    /// Creates a new `EnumInformation` with an empty variant list and a drop function.
    ///
    /// Variants are populated during the [`fill_enums`](crate::generators::compile_internal) phase.
    ///
    /// # Arguments
    ///
    /// * `on_drop` - The `drop` function [`FunctionValue`] for this enum type
    pub const fn new(on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            variants: Vec::new(),
            on_drop,
        }
    }

    /// Inserts a variant symbol and its lowered LLVM struct type.
    ///
    /// Called during the fill phase. The variant's position in the internal vector
    /// determines its discriminant tag value.
    ///
    /// # Arguments
    ///
    /// * `key` - The enum variant symbol
    /// * `val` - The lowered LLVM [`StructType`] for this variant
    pub fn insert(&mut self, key: Rc<EnumVariantSymbol<TypedAST>>, val: StructType<'ctx>) {
        self.variants.push((key, val));
    }

    /// Looks up a variant by symbol and returns its lowered LLVM struct type.
    ///
    /// # Arguments
    ///
    /// * `key` - The enum variant symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(StructType)` - The lowered LLVM struct type for the variant
    /// * `None` - The variant is not registered
    pub fn lookup(&self, key: &EnumVariantSymbol<TypedAST>) -> Option<StructType<'ctx>> {
        self.variants
            .iter()
            .find(|to_check| &*to_check.0 == key)
            .map(|variant| &variant.1)
            .copied()
    }

    /// Returns the index of a variant by symbol.
    ///
    /// The index serves as the discriminant tag value used in pattern matching and drop logic.
    ///
    /// # Arguments
    ///
    /// * `key` - The enum variant symbol to look up
    ///
    /// # Returns
    ///
    /// * `Some(usize)` - The variant's index (discriminant tag)
    /// * `None` - The variant is not registered
    pub fn index_of(&self, key: &EnumVariantSymbol<TypedAST>) -> Option<usize> {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, to_check)| &*to_check.0 == key)
            .map(|(i, _)| i)
    }

    /// Returns the drop function for this enum.
    ///
    /// The drop function reads the discriminant tag to determine which variant is active,
    /// then recursively drops the variant's fields and frees the allocation.
    pub const fn on_drop(&self) -> FunctionValue<'ctx> {
        self.on_drop
    }

    /// Returns all registered enum variants with their lowered types.
    ///
    /// The vector index of each variant corresponds to its discriminant tag value.
    pub const fn variants(&self) -> &Vec<(Rc<EnumVariantSymbol<TypedAST>>, StructType<'ctx>)> {
        &self.variants
    }
}
