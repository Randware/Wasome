use ast::TypedAST;
use ast::symbol::StructFieldSymbol;
use inkwell::types::StructType;
use inkwell::values::FunctionValue;
use std::rc::Rc;

/// Stores lowered LLVM representation and metadata for a struct type.
///
/// Each struct declaration from the AST is registered in the [`SymbolRegistry`] with a
/// corresponding [`StructInformation`] instance. The struct starts as an opaque LLVM type
/// and is later filled in with its complete field layout during the fill phase.
///
/// The struct layout includes a reference count field (i32) at index 0, followed by the
/// lowered field types at subsequent indices. This refcount field is used for automatic
/// memory management via reference counting.
#[derive(PartialEq, Eq)]
pub struct StructInformation<'ctx> {
    /// The struct field symbols from the AST, in declaration order.
    ///
    /// Populated during the fill phase. Each field corresponds to a lowered type at
    /// index `i + 1` in the LLVM struct (index 0 is the refcount field).
    fields: Vec<Rc<StructFieldSymbol<TypedAST>>>,
    /// The lowered LLVM [`StructType`] for this struct.
    ///
    /// Initially created as opaque, then filled with the complete field layout including
    /// the refcount field and all lowered field types.
    lowered: StructType<'ctx>,
    /// The `drop` function for this struct type.
    ///
    /// Called when the reference count reaches zero to recursively deallocate the struct
    /// and all its nested heap-allocated fields.
    on_drop: FunctionValue<'ctx>,
    /// The optional `predrop` function for this struct type.
    ///
    /// Called before the main drop logic with a ghost refcount of 2 to prevent infinite
    /// loops when dealing with self-referential structures. Only present if the struct
    /// defines a `predrop` function. Should this revive the struct, the drop is aborted
    predrop: Option<FunctionValue<'ctx>>,
}

impl<'ctx> StructInformation<'ctx> {
    /// Creates a new `StructInformation` with an opaque struct type and drop function.
    ///
    /// The struct type starts opaque and is later filled with the complete field layout
    /// during the [`fill_structs`](crate::generators::compile_internal) phase.
    ///
    /// # Arguments
    ///
    /// * `lowered` - The opaque LLVM [`StructType`] for this struct
    /// * `on_drop` - The `drop` function [`FunctionValue`] for this struct type
    pub const fn new(lowered: StructType<'ctx>, on_drop: FunctionValue<'ctx>) -> Self {
        Self {
            fields: Vec::new(),
            lowered,
            on_drop,
            predrop: None,
        }
    }

    /// Returns the lowered LLVM struct type.
    pub const fn lowered(&self) -> StructType<'ctx> {
        self.lowered
    }

    /// Returns the struct field symbols in declaration order.
    ///
    /// Each field's position in the vector corresponds to its index in the LLVM struct
    /// layout (offset by 1 for the refcount field at index 0).
    pub fn fields(&self) -> &[Rc<StructFieldSymbol<TypedAST>>] {
        &self.fields
    }

    /// Adds a field symbol to the struct during the fill phase.
    ///
    /// # Arguments
    ///
    /// * `field` - The struct field symbol to add
    pub fn add_field(&mut self, field: Rc<StructFieldSymbol<TypedAST>>) {
        self.fields.push(field);
    }

    /// Returns the drop function for this struct.
    ///
    /// The drop function is called when the reference count reaches zero to recursively
    /// deallocate the struct and all its nested heap-allocated fields.
    pub const fn on_drop(&self) -> FunctionValue<'ctx> {
        self.on_drop
    }

    /// Returns the optional predrop function for this struct.
    ///
    /// The predrop function is called before the main drop logic with a ghost refcount
    /// of 2 to prevent infinite loops when dealing with self-referential structures.
    pub const fn predrop(&self) -> Option<FunctionValue<'ctx>> {
        self.predrop
    }

    /// Sets the predrop function for this struct.
    ///
    /// Called during the drop function generation phase if the struct defines a `predrop` function.
    ///
    /// # Arguments
    ///
    /// * `predrop` - The predrop function [`FunctionValue`]
    pub const fn set_predrop(&mut self, predrop: FunctionValue<'ctx>) {
        self.predrop = Some(predrop);
    }
}
