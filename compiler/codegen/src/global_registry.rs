use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;
use inkwell::types::{BasicType, StructType};
use inkwell::values::FunctionValue;

pub(crate) struct GlobalRegistry<'ctx> {
    base_enum: StructType<'ctx>,
    base_heap_allocated: StructType<'ctx>,
    stacksave: FunctionValue<'ctx>,
    stackrestore: FunctionValue<'ctx>,
}

impl<'ctx> GlobalRegistry<'ctx> {
    pub fn new(ctx: &'ctx Context, module: &'_ Module<'ctx>) -> Self {
        let base_heap_allocated = ctx.struct_type(&[ctx.i32_type().as_basic_type_enum()], false);
        let base_enum = ctx.struct_type(
            &[
                ctx.i32_type().as_basic_type_enum(),
                ctx.i32_type().as_basic_type_enum(),
            ],
            false,
        );

        let ptr_type = ctx.ptr_type(AddressSpace::default()).as_basic_type_enum();
        let stacksave_intrinsic =
            Intrinsic::find("llvm.stacksave").expect("Hardcoded intrinsic should exist");
        let stacksave = stacksave_intrinsic
            .get_declaration(module, &[ptr_type])
            .unwrap();

        let stackrestore_intrinsic =
            Intrinsic::find("llvm.stackrestore").expect("Hardcoded intrinsic should exist");
        let stackrestore = stackrestore_intrinsic
            .get_declaration(
                module,
                &[ctx.ptr_type(AddressSpace::default()).as_basic_type_enum()],
            )
            .unwrap();

        Self {
            base_enum,
            base_heap_allocated,
            stacksave,
            stackrestore,
        }
    }

    pub fn base_enum(&self) -> StructType<'ctx> {
        self.base_enum
    }

    pub fn base_heap_allocated(&self) -> StructType<'ctx> {
        self.base_heap_allocated
    }

    pub fn stacksave(&self) -> FunctionValue<'ctx> {
        self.stacksave
    }

    pub fn stackrestore(&self) -> FunctionValue<'ctx> {
        self.stackrestore
    }
}
