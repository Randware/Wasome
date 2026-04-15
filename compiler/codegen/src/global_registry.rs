use inkwell::context::Context;
use inkwell::types::{BasicType, StructType};

pub(crate) struct GlobalRegistry<'ctx> {
    base_enum: StructType<'ctx>,
    base_heap_allocated: StructType<'ctx>,
}

impl<'ctx> GlobalRegistry<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        let base_heap_allocated = ctx.struct_type(&[ctx.i32_type().as_basic_type_enum()], false);
        let base_enum = ctx.struct_type(
            &[
                ctx.i32_type().as_basic_type_enum(),
                ctx.i32_type().as_basic_type_enum(),
            ],
            false,
        );

        Self {
            base_enum,
            base_heap_allocated,
        }
    }

    pub fn base_enum(&self) -> StructType<'ctx> {
        self.base_enum
    }

    pub fn base_heap_allocated(&self) -> StructType<'ctx> {
        self.base_heap_allocated
    }
}
