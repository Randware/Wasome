mod expression;
mod function;
mod statement;

use crate::context::{FunctionContext, StatementContext};
use crate::symbols::{EnumInformation, StructInformation, VariableTable};
use crate::{Codegen, context::LLVMContext};
use ast::data_type::{DataType, Typed};
use ast::expression::FunctionCall;
use ast::id::Id;
use ast::symbol::SymbolWithTypeParameter;
use ast::top_level::FunctionType;
use ast::traversal::FunctionContainer;
use ast::traversal::directory_traversal::DirectoryTraversalHelper;
use ast::traversal::enum_traversal::EnumTraversalHelper;
use ast::traversal::file_traversal::FileTraversalHelper;
use ast::traversal::function_traversal::FunctionTraversalHelper;
use ast::traversal::struct_traversal::StructTraversalHelper;
use ast::type_parameter::TypedTypeParameter;
use ast::{AST, TypedAST};
use inkwell::types::BasicType;
use inkwell::values::CallSiteValue;
use std::iter::once;

impl<'ctx> Codegen<'ctx> {
    /// Compiles the given typed AST into WebAssembly object code.
    ///
    /// This is the main entry point for the compilation pipeline. It orchestrates the
    /// following phases:
    ///
    /// 1. **Initialization** - Creates an LLVM context with the target configuration
    /// 2. **Registration** - Registers all structs, enums, and functions with the symbol registry
    /// 3. **Filling** - Fills struct and enum layouts with complete LLVM type definitions
    /// 4. **Drop generation** - Generates drop functions for structs and enums
    /// 5. **Implementation** - Compiles the body of each function
    /// 6. **Optimization** - Applies LLVM optimization passes and emits object code
    ///
    /// # Arguments
    ///
    /// * `to_compile` - The typed AST to compile
    ///
    /// # Returns
    ///
    /// A vector containing the compiled WebAssembly object code bytes.
    pub fn compile(&mut self, to_compile: &AST<TypedAST>) -> Vec<u8> {
        let mut llvm_context = LLVMContext::new(self.context, self.opt_level);
        self.compile_internal(&mut llvm_context, to_compile);
        llvm_context.get_object()
    }

    /// Performs the internal compilation steps: registering types, filling struct/enum layouts,
    /// registering functions, creating drop functions, and implementing function bodies.
    ///
    /// This method executes the compilation pipeline in the following order:
    /// 1. Register all struct declarations
    /// 2. Register all enum declarations
    /// 3. Fill struct layouts with complete field information
    /// 4. Fill enum layouts with variant types
    /// 5. Register all function declarations
    /// 6. Generate struct drop function bodies
    /// 7. Generate enum drop function bodies
    /// 8. Compile function bodies
    #[allow(clippy::missing_panics_doc)]
    pub fn compile_internal(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        to_compile: &AST<TypedAST>,
    ) {
        let root = DirectoryTraversalHelper::new_from_ast(to_compile);

        self.register_structs(llvm_context, &root);

        Self::register_enums(llvm_context, &root);

        self.fill_structs(llvm_context, &root);

        self.fill_enums(llvm_context, &root);

        self.register_functions(llvm_context, &root);

        self.create_struct_drop_functions(llvm_context, &root);

        self.create_enum_drop_functions(llvm_context, &root);

        self.impl_functions(llvm_context, &root);
    }

    /// Iterates over all functions in the AST and compiles their bodies.
    ///
    /// Traverses the entire directory tree including functions inside structs.
    /// Only generates code for regular functions; external functions are skipped.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lookups and IR operations
    /// * `root` - The directory tree traversal helper containing all functions
    fn impl_functions(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_functions_of_dir(root, |func| match func.inner().function_type() {
            FunctionType::Regular(_) => self.compile_function(llvm_context, &func),
            FunctionType::External => (),
        });
    }

    /// Generates the drop function body for each enum by iterating over all enum declarations.
    ///
    /// For each enum:
    /// 1. Retrieves the enum's drop function from the symbol registry
    /// 2. Creates a main basic block
    /// 3. Creates a `FunctionContext` and positions the builder
    /// 4. Calls the enum drop compilation logic to generate the drop logic
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lookups and IR operations
    /// * `root` - The directory tree traversal helper containing all enums
    fn create_enum_drop_functions(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_enums_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let tr = llvm_context.type_registry_mut();
            let enum_information = tr.get_enum(symbol).expect("Unregistered enum");
            let func = enum_information.on_drop();
            drop(tr);
            let main_bb = self.context.append_basic_block(func, "main");
            let mut fc = FunctionContext::new(func, main_bb);
            llvm_context.builder().position_at_end(main_bb);

            self.compile_enum_drop(
                llvm_context,
                &mut fc,
                symbol,
                func.get_first_param()
                    .expect("Drop function takes no parameters")
                    .into_pointer_value(),
            );
        });
    }

    /// Generates the drop function body for each struct, including optional predrop handling.
    ///
    /// For each struct:
    /// 1. Searches for an optional `predrop` function (takes 1 parameter, no type parameters, returns void)
    /// 2. If found, registers it in the symbol registry and stores it in [`StructInformation`]
    /// 3. Creates a main basic block for the drop function
    /// 4. Creates a `FunctionContext` and positions the builder
    /// 5. Calls the struct drop compilation logic to generate the drop logic
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lookups and IR operations
    /// * `root` - The directory tree traversal helper containing all structs
    fn create_struct_drop_functions(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_structs_of_dir(root, |st| {
            let predrop = st
                .function_iterator()
                .find(|func| {
                    let symbol = func.inner().declaration();
                    symbol.name() == "drop"
                        && symbol.type_parameters().is_empty()
                        // This must always be a pointer
                        // As methods have an implicit one
                        && symbol.params().len() == 1
                        && symbol.return_type().is_none()
                })
                .map(|func| {
                    llvm_context
                        .type_registry()
                        .get_function(func.inner().declaration())
                        .expect("Unknown function")
                });
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let struct_information = tr.get_struct_mut(symbol).expect("Unregistered struct");
            if let Some(predrop) = predrop {
                struct_information.set_predrop(predrop);
            }
            let func = struct_information.on_drop();
            drop(tr);
            let main_bb = self.context.append_basic_block(func, "main");
            let mut fc = FunctionContext::new(func, main_bb);
            llvm_context.builder().position_at_end(main_bb);

            self.compile_struct_drop(
                llvm_context,
                &mut fc,
                symbol,
                func.get_first_param()
                    .expect("Drop function takes no parameters")
                    .into_pointer_value(),
            );
        });
    }

    /// Fills each enum's [`EnumInformation`] with lowered LLVM struct types for each variant.
    ///
    /// For each enum variant, creates an LLVM struct with the following layout:
    /// - Index 0: reference count (u32)
    /// - Index 1: discriminant tag (u32)
    /// - Index 2+: lowered field types
    ///
    /// The variant types are stored in the [`EnumInformation`] for later lookup during
    /// pattern matching and drop function generation.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lowering and LLVM operations
    /// * `root` - The directory tree traversal helper containing all enums
    fn fill_enums(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        recursive_enums_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let enum_information = tr.get_enum_mut(symbol).expect("Unregistered enum");
            let variants = st.inner().variants();
            let base_enum = &[
                self.context.i32_type().as_basic_type_enum(),
                self.context.i32_type().as_basic_type_enum(),
            ];
            for variant in variants {
                let fields_lowered = base_enum
                    .iter()
                    .copied()
                    .chain(
                        variant
                            .inner()
                            .fields()
                            .iter()
                            .map(|field| llvm_context.lower_type(field)),
                    )
                    .collect::<Vec<_>>();
                let variant_lowered = self.context.struct_type(&fields_lowered, false);
                enum_information.insert(variant.inner_owned(), variant_lowered);
            }
        });
    }

    /// Fills each struct's [`StructInformation`] with its lowered LLVM struct type and field symbols.
    ///
    /// For each struct, creates the complete LLVM struct type with the following layout:
    /// - Index 0: reference count (u32)
    /// - Index 1+: lowered field types
    ///
    /// The struct's LLVM type body is updated via [`StructType::set_body`], and field symbols
    /// are stored in the [`StructInformation`] for later field access and drop logic.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lowering and LLVM operations
    /// * `root` - The directory tree traversal helper containing all structs
    fn fill_structs(&self, llvm_context: &LLVMContext, root: &DirectoryTraversalHelper<TypedAST>) {
        recursive_structs_of_dir(root, |st| {
            let symbol = st.inner().symbol();
            let mut tr = llvm_context.type_registry_mut();
            let struct_information = tr.get_struct_mut(symbol).expect("Unregistered struct");
            let fields = st.inner().fields();
            let ref_count_field = self.context.i32_type().as_basic_type_enum();
            let fields_lowered = once(ref_count_field)
                .chain(
                    fields
                        .iter()
                        .map(|field| llvm_context.lower_type(field.inner().data_type())),
                )
                .collect::<Vec<_>>();
            struct_information
                .lowered()
                .set_body(&fields_lowered, false);
            for field in fields {
                struct_information.add_field(field.inner_owned());
            }
        });
    }

    /// Registers all function declarations in the AST with the module and symbol registry.
    ///
    /// For each function:
    /// 1. Lowers the parameter and return types to LLVM types
    /// 2. Determines the function name:
    ///    - Main function: `_start`
    ///    - Regular functions: mangled name (`{name}-{id}`)
    ///    - External functions: `{name}_{param_sizes}` (e.g., `external_4_8`)
    ///         - No params: just `{name}`
    /// 3. Creates or retrieves the LLVM [`FunctionValue`] in the module
    /// 4. Registers the mapping in the [`SymbolRegistry`]
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lowering and module access
    /// * `root` - The directory tree traversal helper containing all functions
    fn register_functions(
        &self,
        llvm_context: &mut LLVMContext,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        let module = llvm_context.module();
        recursive_functions_of_dir(root, |func| {
            let symbol = func.inner().declaration_owned();
            let params = symbol
                .params()
                .iter()
                .map(|arg| llvm_context.lower_type(arg.data_type()).into())
                .collect::<Vec<_>>();
            let lowered_type = symbol.return_type().map_or_else(
                || llvm_context.context().void_type().fn_type(&params, false),
                |ret| llvm_context.lower_type(ret).fn_type(&params, false),
            );
            let name = if symbol == self.main_function {
                "_start".to_string()
            } else {
                match func.inner().function_type() {
                    FunctionType::Regular(_) => mangle(symbol.name(), symbol.id()),
                    FunctionType::External => {
                        let containing_struct = func.containing_struct();
                        let data_types = containing_struct
                            .iter()
                            .flat_map(|st| st.type_parameters().iter())
                            .chain(symbol.type_parameters().iter())
                            .map(TypedTypeParameter::data_type);
                        let sizes = data_types
                            .map(|param| match param {
                                DataType::Struct(_) | DataType::Enum(_) => "prt".to_string(),
                                _ => param.size_bytes().to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join("_");
                        if sizes.is_empty() {
                            symbol.name().to_string()
                        } else {
                            format!("{}_{}", symbol.name(), sizes)
                        }
                    }
                }
            };
            // If we have an external function, one with its name might already exist
            // as the mangling for them does not always produce unique names
            // In this case, we just take the already existing signature as we never implement them
            // and won't get conflicts there
            let lowered = module
                .get_function(&name)
                .unwrap_or_else(|| module.add_function(&name, lowered_type, None));
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_function(symbol, lowered)
                    .is_none()
            );
        });
    }

    /// Registers all enum declarations with the module and symbol registry.
    ///
    /// For each enum:
    /// 1. Creates a mangled name (`{name}-{id}`)
    /// 2. Creates a drop function (`{mangled name}-drop`) with the signature from [`GlobalRegistry::drop`]
    ///     - No implementation is produced here
    /// 3. Registers the enum in the [`SymbolRegistry`] with a new [`EnumInformation`]
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for module and type access
    /// * `root` - The directory tree traversal helper containing all enums
    fn register_enums(llvm_context: &mut LLVMContext, root: &DirectoryTraversalHelper<TypedAST>) {
        let module = llvm_context.module();
        recursive_enums_of_dir(root, |en| {
            let symbol = en.inner().symbol_owned();
            let name = mangle(symbol.name(), symbol.id());
            let drop = module.add_function(
                &format!("{name}-drop"),
                llvm_context.global_registry().drop(),
                None,
            );
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_enum(symbol, EnumInformation::new(drop))
                    .is_none()
            );
        });
    }

    /// Registers all struct declarations with the module and symbol registry.
    ///
    /// For each struct:
    /// 1. Creates a mangled name (`{name}-{id}`)
    /// 2. Creates an opaque LLVM [`StructType`] with the mangled name
    /// 3. Creates a drop function (`{mangled name}-drop`) with the signature from [`GlobalRegistry::drop`]
    ///     - No implementation is produced here
    /// 4. Registers the struct in the [`SymbolRegistry`] with a new [`StructInformation`]
    ///
    /// The struct type is opaque at this stage and will be filled with the complete layout
    /// during the [`fill_structs`](Self::fill_structs) phase.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for module and type access
    /// * `root` - The directory tree traversal helper containing all structs
    fn register_structs(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        root: &DirectoryTraversalHelper<TypedAST>,
    ) {
        let module = llvm_context.module();
        recursive_structs_of_dir(root, |st| {
            let symbol = st.inner().symbol_owned();
            let name = mangle(symbol.name(), symbol.id());
            let lowered = self.context.opaque_struct_type(&name);
            let drop = module.add_function(
                &format!("{name}-drop"),
                llvm_context.global_registry().drop(),
                None,
            );
            debug_assert!(
                llvm_context
                    .type_registry_mut()
                    .register_struct(symbol, StructInformation::new(lowered, drop))
                    .is_none()
            );
        });
    }

    /// Compiles a function call expression, generating the call instruction and dropping
    /// argument values after the call.
    ///
    /// For each argument:
    /// 1. Compiles the argument expression to get its LLVM value
    /// 2. After the call, drops the argument value to manage reference counts
    ///
    /// This is used by both [`compile_nonvoid_call`](Codegen::compile_nonvoid_call)
    /// and [`compile_void_call`](Codegen::compile_void_call).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The LLVM context for type lookups and IR operations
    /// * `vars` - The `VariableTable` for variable lookups
    /// * `statement_context` - The `StatementContext` for block management
    /// * `to_generate` - The function call expression to compile
    ///
    /// # Returns
    ///
    /// The LLVM `CallSiteValue` for the generated call instruction.
    fn compile_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, '_>,
        to_generate: &FunctionCall<TypedAST>,
    ) -> CallSiteValue<'ctx> {
        let func = llvm_context
            .type_registry()
            .get_function(to_generate.function())
            .expect("Call to unknown function!");
        let args = to_generate
            .args()
            .iter()
            .map(|arg| self.compile_expression(llvm_context, vars, statement_context, arg))
            .collect::<Vec<_>>();
        let ret = llvm_context
            .builder()
            .build_call(
                func,
                &args.iter().copied().map(Into::into).collect::<Vec<_>>(),
                "call",
            )
            .unwrap();
        for arg in args.iter().zip(to_generate.args()) {
            self.compile_val_drop(
                llvm_context,
                statement_context.function_context_mut(),
                &arg.1.data_type(),
                *arg.0,
            );
        }
        ret
    }
}

/// Mangles a symbol name with its unique ID for LLVM linkage.
///
/// Produces names in the format `{name}-{id}` to ensure uniqueness in the LLVM module.
fn mangle(name: &str, id: &Id) -> String {
    format!("{}-{}", name, id.as_unique_string())
}

/// Recursively iterates over all functions in the directory tree, including those inside structs.
///
/// Traverses all files and subdirectories, yielding each function (including methods
/// defined inside structs) to the callback.
fn recursive_functions_of_dir<'b>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: impl for<'a> FnMut(FunctionTraversalHelper<'a, 'b, TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.function_iterator().for_each(&mut callback);
        file.structs_iterator()
            .for_each(|st| st.function_iterator().for_each(&mut callback));
    });
}

/// Recursively iterates over all enum declarations in the directory tree.
///
/// Traverses all files and subdirectories, yielding each enum declaration to the callback.
fn recursive_enums_of_dir(
    dir: &DirectoryTraversalHelper<TypedAST>,
    mut callback: impl FnMut(EnumTraversalHelper<TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.enums_iterator().for_each(&mut callback);
    });
}

/// Recursively iterates over all struct declarations in the directory tree.
///
/// Traverses all files and subdirectories, yielding each struct declaration to the callback.
fn recursive_structs_of_dir(
    dir: &DirectoryTraversalHelper<TypedAST>,
    mut callback: impl FnMut(StructTraversalHelper<TypedAST>),
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.structs_iterator().for_each(&mut callback);
    });
}

/// Recursively iterates over all files in the directory tree.
///
/// First recursively processes all subdirectories, then processes all files in the
/// current directory. This ensures a consistent traversal order.
fn recursive_files_of_dir<'b, Callback: FnMut(FileTraversalHelper<'_, 'b, TypedAST>)>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    callback: &mut Callback,
) {
    dir.subdirectories_iterator()
        .for_each(|subdir| recursive_files_of_dir(&subdir, callback));
    dir.files_iterator().for_each(callback);
}
