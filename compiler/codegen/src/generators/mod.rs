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
            let prev = llvm_context
                .type_registry_mut()
                .register_function(symbol, lowered);
            debug_assert!(
                prev
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
            let prev = llvm_context
                .type_registry_mut()
                .register_enum(symbol, EnumInformation::new(drop));
            debug_assert!(
                prev
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
            let prev = llvm_context
                .type_registry_mut()
                .register_struct(symbol, StructInformation::new(lowered, drop));
            debug_assert!(
                prev
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
fn recursive_functions_of_dir<'b, Callback: FnMut(FunctionTraversalHelper<'_, 'b, TypedAST>)>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: Callback,
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
fn recursive_enums_of_dir<'b, Callback: FnMut(EnumTraversalHelper<'_, 'b, TypedAST>)>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: Callback,
) {
    recursive_files_of_dir(dir, &mut |file| {
        file.enums_iterator().for_each(&mut callback);
    });
}

/// Recursively iterates over all struct declarations in the directory tree.
///
/// Traverses all files and subdirectories, yielding each struct declaration to the callback.
fn recursive_structs_of_dir<'b, Callback: FnMut(StructTraversalHelper<'_, 'b, TypedAST>)>(
    dir: &DirectoryTraversalHelper<'_, 'b, TypedAST>,
    mut callback: Callback,
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

#[cfg(test)]
mod tests {
    use super::*;
    use ast::composite::{Enum, EnumVariant, Struct};
    use ast::directory::Directory;
    use ast::file::File;
    use ast::id::Id;
    use ast::statement::{CodeBlock, Statement};
    use ast::symbol::{EnumSymbol, EnumVariantSymbol, FunctionSymbol, StructSymbol};
    use ast::top_level::{Function, FunctionType};
    use ast::traversal::enum_traversal::EnumTraversalHelper;
    use ast::traversal::function_traversal::FunctionTraversalHelper;
    use ast::traversal::struct_traversal::StructTraversalHelper;
    use ast::visibility::Visibility;
    use ast::{AST, ASTNode, TypedAST};
    use source::types::FileID;
    use std::path::PathBuf;
    use std::rc::Rc;

    fn sample_span() -> source::types::Span {
        FileID::from(0).span(0, 10)
    }

    #[test]
    fn mangle_simple_name() {
        let id = Id::new();
        let result = mangle("foo", &id);
        assert_eq!(result, format!("foo-{}", id.as_unique_string()));
    }

    #[test]
    fn mangle_name_with_underscore() {
        let id = Id::new();
        let result = mangle("my_func", &id);
        assert_eq!(result, format!("my_func-{}", id.as_unique_string()));
    }

    #[test]
    fn mangle_different_ids_different_results() {
        let id1 = Id::new();
        let id2 = Id::new();
        let r1 = mangle("same", &id1);
        let r2 = mangle("same", &id2);
        assert_ne!(r1, r2);
    }

    #[test]
    fn mangle_same_id_same_name_same_result() {
        let id = Id::new();
        let id_clone = id.clone();
        let r1 = mangle("test", &id);
        let r2 = mangle("test", &id_clone);
        assert_eq!(r1, r2);
    }

    #[test]
    fn mangle_different_names_different_results() {
        let id = Id::new();
        let r1 = mangle("alpha", &id);
        let r2 = mangle("beta", &id);
        assert_ne!(r1, r2);
        assert!(r1.starts_with("alpha-"));
        assert!(r2.starts_with("beta-"));
    }

    // -----------------------------------------------------------------------
    // recursive_files_of_dir tests
    // -----------------------------------------------------------------------

    fn make_ast_with_files(file_names: Vec<&str>) -> AST<TypedAST> {
        let files: Vec<ASTNode<File<TypedAST>, FileID>> = file_names
            .into_iter()
            .map(|name| {
                ASTNode::new(
                    File::<TypedAST>::new(
                        name.to_string(),
                        Vec::new(),
                        Vec::new(),
                        Vec::new(),
                        Vec::new(),
                    ),
                    FileID::from(0),
                )
            })
            .collect();
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), files),
            PathBuf::new(),
        );
        AST::new(root_dir).unwrap()
    }

    #[test]
    fn recursive_files_empty() {
        let ast = make_ast_with_files(Vec::new());
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        recursive_files_of_dir(&root, &mut |_| panic!());
    }

    #[test]
    fn recursive_files_single() {
        let ast = make_ast_with_files(vec!["main"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_files_of_dir(&root, &mut |_| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn recursive_files_multiple() {
        let ast = make_ast_with_files(vec!["a", "b", "c"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_files_of_dir(&root, &mut |_| count += 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn recursive_files_with_subdirectory() {
        let sub_files: Vec<ASTNode<File<TypedAST>, FileID>> = vec![ASTNode::new(
            File::<TypedAST>::new(
                "sub_file".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        )];
        let sub_dir = ASTNode::new(
            Directory::new("sub".to_string(), Vec::new(), sub_files),
            PathBuf::new(),
        );
        let main_file: ASTNode<File<TypedAST>, FileID> = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), vec![sub_dir], vec![main_file]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_files_of_dir(&root, &mut |_| count += 1);
        assert_eq!(count, 2);
    }

    #[test]
    fn recursive_files_nested_subdirectories() {
        let deep_files: Vec<ASTNode<File<TypedAST>, FileID>> = vec![ASTNode::new(
            File::<TypedAST>::new(
                "deep".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        )];
        let deep_dir = ASTNode::new(
            Directory::new("deep".to_string(), Vec::new(), deep_files),
            PathBuf::new(),
        );
        let mid_files: Vec<ASTNode<File<TypedAST>, FileID>> = vec![ASTNode::new(
            File::<TypedAST>::new(
                "mid".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        )];
        let mid_dir = ASTNode::new(
            Directory::new("mid".to_string(), vec![deep_dir], mid_files),
            PathBuf::new(),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), vec![mid_dir], Vec::new()),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_files_of_dir(&root, &mut |_| count += 1);
        assert_eq!(count, 2);
    }

    #[test]
    fn recursive_files_with_subdirectory_order() {
        let sub_files: Vec<ASTNode<File<TypedAST>, FileID>> = vec![ASTNode::new(
            File::<TypedAST>::new(
                "sub_file".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        )];
        let sub_dir = ASTNode::new(
            Directory::new("sub".to_string(), Vec::new(), sub_files),
            PathBuf::new(),
        );
        let file_a: ASTNode<File<TypedAST>, FileID> = ASTNode::new(
            File::<TypedAST>::new(
                "a".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        );
        let file_b: ASTNode<File<TypedAST>, FileID> = ASTNode::new(
            File::<TypedAST>::new(
                "b".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), vec![sub_dir], vec![file_a, file_b]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut visited: Vec<String> = Vec::new();
        recursive_files_of_dir(&root, &mut |file| {
            visited.push(file.inner().name().to_string());
        });
        assert_eq!(visited.len(), 3);
        assert!(visited.contains(&"sub_file".to_string()));
        assert!(visited.contains(&"a".to_string()));
        assert!(visited.contains(&"b".to_string()));
    }

    // -----------------------------------------------------------------------
    // recursive_functions_of_dir tests
    // -----------------------------------------------------------------------

    fn make_ast_with_functions(func_names: Vec<&str>) -> AST<TypedAST> {
        let functions: Vec<ASTNode<Function<TypedAST>, source::types::Span>> = func_names
            .into_iter()
            .map(|name| {
                let symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
                    name.to_string(),
                    None,
                    Vec::new(),
                    Vec::new(),
                ));
                ASTNode::new(
                    Function::new(
                        symbol,
                        FunctionType::Regular(ASTNode::new(
                            Statement::Codeblock(CodeBlock::new(Vec::new())),
                            sample_span(),
                        )),
                        Visibility::Public,
                    ),
                    sample_span(),
                )
            })
            .collect();
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                functions,
                Vec::new(),
                Vec::new(),
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        AST::new(root_dir).unwrap()
    }

    #[test]
    fn recursive_functions_empty() {
        let ast = make_ast_with_functions(Vec::new());
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        recursive_functions_of_dir(&root, |_| panic!());
    }

    #[test]
    fn recursive_functions_single() {
        let ast = make_ast_with_functions(vec!["foo"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_functions_of_dir(&root, |_| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn recursive_functions_multiple() {
        let ast = make_ast_with_functions(vec!["a", "b", "c"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_functions_of_dir(&root, |_| count += 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn recursive_functions_with_struct_methods() {
        let struct_symbol = Rc::new(StructSymbol::<TypedAST>::new(
            "MyStruct".to_string(),
            Vec::new(),
        ));
        let method1_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            "method1".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let method2_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            "method2".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let struct_fn1 = ASTNode::new(
            Function::new(
                method1_symbol,
                FunctionType::Regular(ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(Vec::new())),
                    sample_span(),
                )),
                Visibility::Public,
            ),
            sample_span(),
        );
        let struct_fn2 = ASTNode::new(
            Function::new(
                method2_symbol,
                FunctionType::Regular(ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(Vec::new())),
                    sample_span(),
                )),
                Visibility::Public,
            ),
            sample_span(),
        );
        let struct_node = ASTNode::new(
            Struct::<TypedAST>::new(
                struct_symbol.clone(),
                vec![struct_fn1, struct_fn2],
                Vec::new(),
                Visibility::Public,
            ),
            sample_span(),
        );
        let main_fn_symbol = Rc::new(FunctionSymbol::<TypedAST>::new(
            "main".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let main_fn = ASTNode::new(
            Function::new(
                main_fn_symbol,
                FunctionType::Regular(ASTNode::new(
                    Statement::Codeblock(CodeBlock::new(Vec::new())),
                    sample_span(),
                )),
                Visibility::Public,
            ),
            sample_span(),
        );
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                vec![main_fn],
                Vec::new(),
                vec![struct_node],
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_functions_of_dir(&root, |_| count += 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn recursive_functions_returns_correct_symbols() {
        let fn_a = Rc::new(FunctionSymbol::<TypedAST>::new(
            "func_a".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let fn_b = Rc::new(FunctionSymbol::<TypedAST>::new(
            "func_b".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));
        let struct_symbol = Rc::new(StructSymbol::<TypedAST>::new(
            "MyStruct".to_string(),
            Vec::new(),
        ));
        let method = Rc::new(FunctionSymbol::<TypedAST>::new(
            "method".to_string(),
            None,
            Vec::new(),
            Vec::new(),
        ));

        let make_fn = |sym: Rc<FunctionSymbol<TypedAST>>| -> ASTNode<Function<TypedAST>, source::types::Span> {
            ASTNode::new(
                Function::new(
                    sym,
                    FunctionType::Regular(ASTNode::new(
                        Statement::Codeblock(CodeBlock::new(Vec::new())),
                        sample_span(),
                    )),
                    Visibility::Public,
                ),
                sample_span(),
            )
        };

        let struct_node = ASTNode::new(
            Struct::<TypedAST>::new(
                struct_symbol.clone(),
                vec![make_fn(method.clone())],
                Vec::new(),
                Visibility::Public,
            ),
            sample_span(),
        );
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                vec![make_fn(fn_a.clone()), make_fn(fn_b.clone())],
                Vec::new(),
                vec![struct_node],
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut collected_names: Vec<String> = Vec::new();
        recursive_functions_of_dir(&root, &mut |func: FunctionTraversalHelper<TypedAST>| {
            collected_names.push(func.inner().declaration().name().to_string());
        });
        assert_eq!(collected_names.len(), 3);
        assert!(collected_names.contains(&"func_a".to_string()));
        assert!(collected_names.contains(&"func_b".to_string()));
        assert!(collected_names.contains(&"method".to_string()));
    }

    // -----------------------------------------------------------------------
    // recursive_structs_of_dir tests
    // -----------------------------------------------------------------------

    fn make_ast_with_structs(struct_names: Vec<&str>) -> AST<TypedAST> {
        let structs: Vec<ASTNode<Struct<TypedAST>, source::types::Span>> = struct_names
            .into_iter()
            .map(|name| {
                let symbol = Rc::new(StructSymbol::<TypedAST>::new(name.to_string(), Vec::new()));
                ASTNode::new(
                    Struct::<TypedAST>::new(symbol, Vec::new(), Vec::new(), Visibility::Public),
                    sample_span(),
                )
            })
            .collect();
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                structs,
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        AST::new(root_dir).unwrap()
    }

    #[test]
    fn recursive_structs_empty() {
        let ast = make_ast_with_structs(Vec::new());
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        recursive_structs_of_dir(&root, |_| panic!());
    }

    #[test]
    fn recursive_structs_single() {
        let ast = make_ast_with_structs(vec!["Foo"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_structs_of_dir(&root, |_| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn recursive_structs_multiple() {
        let ast = make_ast_with_structs(vec!["A", "B", "C"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_structs_of_dir(&root, |_| count += 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn recursive_structs_returns_correct_symbols() {
        let struct_a = Rc::new(StructSymbol::<TypedAST>::new(
            "StructA".to_string(),
            Vec::new(),
        ));
        let struct_b = Rc::new(StructSymbol::<TypedAST>::new(
            "StructB".to_string(),
            Vec::new(),
        ));

        let make_struct =
            |sym: Rc<StructSymbol<TypedAST>>| -> ASTNode<Struct<TypedAST>, source::types::Span> {
                ASTNode::new(
                    Struct::<TypedAST>::new(sym, Vec::new(), Vec::new(), Visibility::Public),
                    sample_span(),
                )
            };

        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                vec![make_struct(struct_a.clone()), make_struct(struct_b.clone())],
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut collected_names: Vec<String> = Vec::new();
        recursive_structs_of_dir(&root, |st: StructTraversalHelper<TypedAST>| {
            collected_names.push(st.inner().symbol().name().to_string());
        });
        assert_eq!(collected_names.len(), 2);
        assert!(collected_names.contains(&"StructA".to_string()));
        assert!(collected_names.contains(&"StructB".to_string()));
    }

    // -----------------------------------------------------------------------
    // recursive_enums_of_dir tests
    // -----------------------------------------------------------------------

    fn make_ast_with_enums(enum_names: Vec<&str>) -> AST<TypedAST> {
        let enums: Vec<ASTNode<Enum<TypedAST>, source::types::Span>> = enum_names
            .into_iter()
            .map(|name| {
                let symbol = Rc::new(EnumSymbol::<TypedAST>::new(name.to_string(), Vec::new()));
                let variant_symbol = Rc::new(EnumVariantSymbol::<TypedAST>::new(
                    format!("{}V", name),
                    Vec::new(),
                ));
                ASTNode::new(
                    Enum::<TypedAST>::new(
                        symbol,
                        vec![ASTNode::new(
                            EnumVariant::<TypedAST>::new(variant_symbol),
                            sample_span(),
                        )],
                        Visibility::Public,
                    ),
                    sample_span(),
                )
            })
            .collect();
        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                Vec::new(),
                enums,
                Vec::new(),
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        AST::new(root_dir).unwrap()
    }

    #[test]
    fn recursive_enums_empty() {
        let ast = make_ast_with_enums(Vec::new());
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        recursive_enums_of_dir(&root, |_| panic!());
    }

    #[test]
    fn recursive_enums_single() {
        let ast = make_ast_with_enums(vec!["Color"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_enums_of_dir(&root, |_| count += 1);
        assert_eq!(count, 1);
    }

    #[test]
    fn recursive_enums_multiple() {
        let ast = make_ast_with_enums(vec!["Red", "Green", "Blue"]);
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut count = 0;
        recursive_enums_of_dir(&root, |_| count += 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn recursive_enums_returns_correct_symbols() {
        let enum_a = Rc::new(EnumSymbol::<TypedAST>::new("EnumA".to_string(), Vec::new()));
        let enum_b = Rc::new(EnumSymbol::<TypedAST>::new("EnumB".to_string(), Vec::new()));

        let make_enum =
            |sym: Rc<EnumSymbol<TypedAST>>| -> ASTNode<Enum<TypedAST>, source::types::Span> {
                let variant_sym = Rc::new(EnumVariantSymbol::<TypedAST>::new(
                    format!("{}_V", sym.name()),
                    Vec::new(),
                ));
                ASTNode::new(
                    Enum::<TypedAST>::new(
                        sym,
                        vec![ASTNode::new(
                            EnumVariant::<TypedAST>::new(variant_sym),
                            sample_span(),
                        )],
                        Visibility::Public,
                    ),
                    sample_span(),
                )
            };

        let file = ASTNode::new(
            File::<TypedAST>::new(
                "main".to_string(),
                Vec::new(),
                Vec::new(),
                vec![make_enum(enum_a.clone()), make_enum(enum_b.clone())],
                Vec::new(),
            ),
            FileID::from(0),
        );
        let root_dir = ASTNode::new(
            Directory::new("src".to_string(), Vec::new(), vec![file]),
            PathBuf::new(),
        );
        let ast = AST::new(root_dir).unwrap();
        let root = DirectoryTraversalHelper::new_from_ast(&ast);
        let mut collected_names: Vec<String> = Vec::new();
        recursive_enums_of_dir(&root, |en: EnumTraversalHelper<TypedAST>| {
            collected_names.push(en.inner().symbol().name().to_string());
        });
        assert_eq!(collected_names.len(), 2);
        assert!(collected_names.contains(&"EnumA".to_string()));
        assert!(collected_names.contains(&"EnumB".to_string()));
    }
}
