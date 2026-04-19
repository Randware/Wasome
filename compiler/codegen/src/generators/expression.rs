use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use crate::Codegen;
use ast::data_type::{DataType, Typed};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, NewEnum, NewStruct,
    StructFieldAccess, Typecast, UnaryOp, UnaryOpType,
};
use ast::symbol::VariableSymbol;
use ast::TypedAST;
use inkwell::builder::{Builder, BuilderError};
use inkwell::types::{IntType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate};

impl<'ctx, 'fc> Codegen<'ctx> {
    /// Compiles an expression to LLVM IR, dispatching to the appropriate handler based on expression type.
    ///
    /// This is the central expression compilation dispatcher. It matches on the expression
    /// variant and delegates to the corresponding handler method.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The expression to compile
    ///
    /// # Returns
    ///
    /// The compiled LLVM [`BasicValueEnum`] representing the expression's value.
    pub(crate) fn compile_expression(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &Expression<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        match to_generate {
            Expression::FunctionCall(call) => {
                self.compile_nonvoid_call(llvm_context, vars, statement_context, call)
            }
            Expression::MethodCall(_) => unreachable!(),
            Expression::Variable(var) => self.compile_var_access(llvm_context, vars, var),
            Expression::Literal(lit) => self.compile_literal(lit),
            Expression::UnaryOp(un) => {
                self.compile_unary_op(llvm_context, vars, statement_context, un)
            }
            Expression::BinaryOp(bin) => {
                self.compile_binary_op(llvm_context, vars, statement_context, bin)
            }
            Expression::NewStruct(ns) => {
                self.compile_new_struct(llvm_context, vars, statement_context, ns)
            }
            Expression::NewEnum(ne) => {
                self.compile_new_enum(llvm_context, vars, statement_context, ne)
            }
            Expression::StructFieldAccess(sfa) => {
                self.compile_sfa(llvm_context, vars, statement_context, sfa)
            }
        }
    }

    /// Compiles a variable access expression by loading the value from its alloca pointer.
    ///
    /// The variable's value is loaded from its stack slot and the reference count is
    /// incremented for struct/enum types via [`compile_val_create`](Codegen::compile_val_create).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lowering and IR operations
    /// * `vars` - The [`VariableTable`] containing the variable's alloca pointer
    /// * `to_generate` - The variable symbol to load
    ///
    /// # Returns
    ///
    /// The loaded LLVM [`BasicValueEnum`].
    pub(crate) fn compile_var_access(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &VariableSymbol<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let var = vars
            .lookup(to_generate)
            .expect("Undeclared variable in the typed AST")
            .pointer;
        let val = llvm_context
            .builder()
            .build_load(
                llvm_context.lower_type(to_generate.data_type()),
                var,
                "var_load",
            )
            .unwrap();
        self.compile_val_create(llvm_context, val);
        val
    }

    /// Compiles a literal value (S32, Bool, Char, F64) to its LLVM constant representation.
    ///
    /// # Arguments
    ///
    /// * `to_generate` - The literal expression to compile
    ///
    /// # Returns
    ///
    /// The LLVM [`BasicValueEnum`] representing the literal constant.
    pub(crate) fn compile_literal(&self, to_generate: &Literal) -> BasicValueEnum<'ctx> {
        match to_generate {
            Literal::S32(val) =>
            {
                #[allow(clippy::cast_sign_loss)]
                self.context
                    .i32_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum()
            }
            Literal::Bool(val) => self
                .context
                .bool_type()
                .const_int(u64::from(*val), false)
                .as_basic_value_enum(),
            Literal::Char(val) => self
                .context
                .i32_type()
                .const_int(u64::from(*val), false)
                .as_basic_value_enum(),
            Literal::F64(val) => self
                .context
                .f64_type()
                .const_float(*val)
                .as_basic_value_enum(),
        }
    }

    /// Compiles a unary operation (negation, logical not, or typecast).
    ///
    /// Dispatches to the appropriate LLVM instruction based on the operation type:
    /// * `Negative` - Float negation or integer negation
    /// * `Not` - Bitwise XOR with 1 (logical not for booleans)
    /// * `Typecast` - Delegates to [`compile_typecast`](Self::compile_typecast)
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The unary operation to compile
    ///
    /// # Returns
    ///
    /// The LLVM [`BasicValueEnum`] representing the result.
    pub(crate) fn compile_unary_op(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,

        to_generate: &UnaryOp<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let inner =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.input());
        match to_generate.op_type() {
            UnaryOpType::Negative => {
                if to_generate.data_type().is_float() {
                    llvm_context
                        .builder()
                        .build_float_neg(inner.into_float_value(), "negative")
                        .unwrap()
                        .as_basic_value_enum()
                } else {
                    llvm_context
                        .builder()
                        .build_int_neg(inner.into_int_value(), "negative")
                        .unwrap()
                        .as_basic_value_enum()
                }
            }
            UnaryOpType::Not => llvm_context
                .builder()
                .build_xor(
                    inner.into_int_value(),
                    self.context.i8_type().const_int(1, false),
                    "not",
                )
                .unwrap()
                .as_basic_value_enum(),

            UnaryOpType::Typecast(cast) => {
                self.compile_typecast(llvm_context, &to_generate.input().data_type(), inner, cast)
            }
        }
    }

    /// Compiles a typecast expression, handling float widening/narrowing and integer
    /// sign changes, extension, truncation, and bit casts.
    ///
    /// Supported casts:
    /// * `F64` → `F32`: Float truncation
    /// * `F32` → `F64`: Float extension
    /// * Signed ↔ Unsigned (same width): Bit cast
    /// * Smaller → Larger integer: Sign or zero extension
    /// * Larger → Smaller integer: Truncation
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for IR operations
    /// * `target` - The target [`DataType`] of the cast
    /// * `to_cast` - The LLVM [`BasicValueEnum`] to cast
    /// * `cast` - The typecast expression containing the source type
    ///
    /// # Returns
    ///
    /// The casted LLVM [`BasicValueEnum`].
    fn compile_typecast(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        target: &DataType,
        to_cast: BasicValueEnum<'ctx>,
        cast: &Typecast<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        use DataType as D;
        match (target, cast.target()) {
            (D::F32, D::F64) => llvm_context
                .builder()
                .build_float_ext(to_cast.into_float_value(), self.context.f64_type(), "cast")
                .unwrap()
                .as_basic_value_enum(),
            (D::F64, D::F32) => llvm_context
                .builder()
                .build_float_trunc(to_cast.into_float_value(), self.context.f32_type(), "cast")
                .unwrap()
                .as_basic_value_enum(),
            (D::S8, D::U8)
            | (D::U8, D::S8)
            | (D::S16, D::U16)
            | (D::U16, D::S16)
            | (D::S32, D::U32)
            | (D::U32, D::S32)
            | (D::S64, D::U64)
            | (D::U64, D::S64) => {
                let target = self.int_dt_to_llvm_dt(cast.target());
                let val = llvm_context
                    .builder()
                    .build_bit_cast(to_cast.into_int_value(), target, "cast")
                    .unwrap()
                    .into_int_value();
                val.as_basic_value_enum()
            }
            (D::S8 | D::U8, D::S16 | D::U16 | D::S32 | D::U32 | D::S64 | D::U64)
            | (D::S16 | D::U16, D::S32 | D::U32 | D::S64 | D::U64)
            | (D::S32 | D::U32, D::S64 | D::U64) => {
                let target = self.int_dt_to_llvm_dt(cast.target());
                let op = if cast.target().is_sint() {
                    Builder::build_int_s_extend::<IntValue<'ctx>>
                } else {
                    Builder::build_int_z_extend
                };
                op(
                    llvm_context.builder(),
                    to_cast.into_int_value(),
                    target,
                    "cast",
                )
                .unwrap()
                .as_basic_value_enum()
            }
            (D::S16 | D::U16, D::S8 | D::U8)
            | (D::S32 | D::U32, D::S16 | D::U16)
            | (D::S64 | D::U64, D::S32 | D::U32) => {
                let target = self.int_dt_to_llvm_dt(cast.target());
                let trunc = llvm_context
                    .builder()
                    .build_int_truncate(to_cast.into_int_value(), target, "cast")
                    .unwrap();
                trunc.as_basic_value_enum()
            }
            _ => unreachable!(),
        }
    }

    /// Compiles a binary operation, dispatching to arithmetic, shift, bitwise, or comparison
    /// handlers based on the operation type.
    ///
    /// Supported operations:
    /// * Arithmetic: `Addition`, `Subtraction`, `Multiplication`, `Division`, `Modulo`
    /// * Shift: `LeftShift`, `RightShift` (arithmetic for signed, logical for unsigned)
    /// * Bitwise: `BitwiseOr`, `BitwiseAnd`, `BitwiseXor`
    /// * Logical: `Or`, `And`, `Xor`
    /// * Comparison: `Equals`, `NotEquals`, `Greater`, `GreaterEquals`, `Lesser`, `LesserEquals`
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The binary operation to compile
    ///
    /// # Returns
    ///
    /// The LLVM [`BasicValueEnum`] representing the result.
    // This can't really be meaningfully split up any further
    #[allow(clippy::too_many_lines)]
    pub(crate) fn compile_binary_op(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &BinaryOp<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let lhs =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.left());
        let rhs =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.right());
        let dt = to_generate.left().data_type();
        match to_generate.op_type() {
            BinaryOpType::Addition => Self::compile_arithmetic_binop(
                llvm_context,
                &dt,
                lhs,
                rhs,
                Builder::build_float_add,
                Builder::build_int_add,
                Builder::build_int_add,
            ),
            BinaryOpType::Subtraction => Self::compile_arithmetic_binop(
                llvm_context,
                &dt,
                lhs,
                rhs,
                Builder::build_float_sub,
                Builder::build_int_sub,
                Builder::build_int_sub,
            ),
            BinaryOpType::Multiplication => Self::compile_arithmetic_binop(
                llvm_context,
                &dt,
                lhs,
                rhs,
                Builder::build_float_mul,
                Builder::build_int_mul,
                Builder::build_int_mul,
            ),
            BinaryOpType::Division => {
                Self::compile_int_zero_check(llvm_context, statement_context, rhs, &dt);
                Self::compile_arithmetic_binop(
                    llvm_context,
                    &dt,
                    lhs,
                    rhs,
                    Builder::build_float_div,
                    Builder::build_int_unsigned_div,
                    Builder::build_int_signed_div,
                )
            }
            BinaryOpType::Modulo => {
                Self::compile_int_zero_check(llvm_context, statement_context, rhs, &dt);
                Self::compile_arithmetic_binop(
                    llvm_context,
                    &dt,
                    lhs,
                    rhs,
                    Builder::build_float_rem,
                    Builder::build_int_unsigned_rem,
                    Builder::build_int_signed_rem,
                )
            }
            BinaryOpType::LeftShift => self.compile_shift(
                llvm_context,
                statement_context,
                lhs,
                rhs,
                |builder, lhs, rhs, name| builder.build_left_shift(lhs, rhs, name),
            ),
            BinaryOpType::RightShift => self.compile_shift(
                llvm_context,
                statement_context,
                lhs,
                rhs,
                |builder, lhs, rhs, name| builder.build_right_shift(lhs, rhs, dt.is_sint(), name),
            ),
            BinaryOpType::BitwiseOr | BinaryOpType::Or => {
                Self::compile_int_binop(llvm_context, lhs, rhs, Builder::build_or)
            }
            BinaryOpType::BitwiseAnd | BinaryOpType::And => {
                Self::compile_int_binop(llvm_context, lhs, rhs, Builder::build_and)
            }
            BinaryOpType::BitwiseXor | BinaryOpType::Xor => {
                Self::compile_int_binop(llvm_context, lhs, rhs, Builder::build_xor)
            }
            BinaryOpType::Equals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::OEQ,
                IntPredicate::EQ,
                IntPredicate::EQ,
            ),
            BinaryOpType::NotEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::ONE,
                IntPredicate::NE,
                IntPredicate::NE,
            ),
            BinaryOpType::Greater => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::OGT,
                IntPredicate::SGT,
                IntPredicate::UGT,
            ),
            BinaryOpType::GreaterEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::OGE,
                IntPredicate::SGE,
                IntPredicate::UGE,
            ),
            BinaryOpType::Lesser => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::OLT,
                IntPredicate::SLT,
                IntPredicate::ULT,
            ),
            BinaryOpType::LesserEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                &dt,
                FloatPredicate::OLE,
                IntPredicate::SLE,
                IntPredicate::ULE,
            ),
        }
    }

    /// Helper for operations with non-zero requirements for ints.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `statement_context` - The [`StatementContext`] for accessing the current function
    /// * `to_check` - The value to check
    /// * `dt` - The data type of the value to check
    fn compile_int_zero_check(llvm_context: &LLVMContext<'ctx>, statement_context: &mut StatementContext<'ctx, '_>, to_check: BasicValueEnum<'ctx>, dt: &DataType) {
        if !dt.is_float() {
            let rhs_int = to_check.into_int_value();
            let zero = rhs_int.get_type().const_int(0, false);
            let is_zero = llvm_context
                .builder()
                .build_int_compare(IntPredicate::EQ, rhs_int, zero, "is_zero")
                .unwrap();
            let then_block = llvm_context.context().append_basic_block(
                statement_context.function_context().current_function(),
                "zero",
            );
            let continue_block = llvm_context.context().append_basic_block(
                statement_context.function_context().current_function(),
                "continue",
            );
            llvm_context
                .builder()
                .build_conditional_branch(is_zero, then_block, continue_block)
                .unwrap();
            statement_context.set_current_block(llvm_context, then_block);
            llvm_context
                .builder()
                .build_call(llvm_context.global_registry().panic(), &[], "panic")
                .unwrap();
            statement_context.set_current_block(llvm_context, continue_block);
        }
    }

    /// Helper for shift operations with bit width validation.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `statement_context` - The [`StatementContext`] for accessing the current function
    /// * `lhs` - The left-hand side LLVM value
    /// * `rhs` - The right-hand side LLVM value (shift amount)
    /// * `op` - The shift operation function to apply
    fn compile_shift(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        op: impl FnOnce(
            &Builder<'ctx>,
            IntValue<'ctx>,
            IntValue<'ctx>,
            &str,
        ) -> Result<IntValue<'ctx>, BuilderError>,
    ) -> BasicValueEnum<'ctx> {
        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();
        let bit_width = lhs_int.get_type().get_bit_width();
        let bit_width_val = rhs_int.get_type().const_int(bit_width as u64, false);
        let is_invalid = llvm_context
            .builder()
            .build_int_compare(IntPredicate::UGE, rhs_int, bit_width_val, "shift_overflow")
            .unwrap();
        let then_block = llvm_context.context().append_basic_block(
            statement_context.function_context().current_function(),
            "shift_overflow",
        );
        let continue_block = llvm_context.context().append_basic_block(
            statement_context.function_context().current_function(),
            "shift_continue",
        );
        llvm_context
            .builder()
            .build_conditional_branch(is_invalid, then_block, continue_block)
            .unwrap();
        statement_context.set_current_block(llvm_context, then_block);
        llvm_context
            .builder()
            .build_call(llvm_context.global_registry().panic(), &[], "panic")
            .unwrap();
        statement_context.set_current_block(llvm_context, continue_block);
        let val = op(llvm_context.builder(), lhs_int, rhs_int, "shift").unwrap();
        val.as_basic_value_enum()
    }

    /// Helper for integer-only binary operations (shifts, bitwise ops).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `lhs` - The left-hand side LLVM value
    /// * `rhs` - The right-hand side LLVM value
    /// * `op` - The operation function to apply
    fn compile_int_binop(
        llvm_context: &LLVMContext<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        op: impl FnOnce(
            &Builder<'ctx>,
            IntValue<'ctx>,
            IntValue<'ctx>,
            &str,
        ) -> Result<IntValue<'ctx>, BuilderError>,
    ) -> BasicValueEnum<'ctx> {
        let val = op(
            llvm_context.builder(),
            lhs.into_int_value(),
            rhs.into_int_value(),
            "int_binop",
        )
        .unwrap();
        val.as_basic_value_enum()
    }

    /// Helper for arithmetic binary operations (add, sub, mul, div, rem) with float and integer support.
    ///
    /// Dispatches to the appropriate operation based on whether the data type is a float.
    /// For integers, selects between signed and unsigned operations.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `dt` - The [`DataType`] determining float vs integer and signed vs unsigned
    /// * `lhs` - The left-hand side LLVM value
    /// * `rhs` - The right-hand side LLVM value
    /// * `float_op` - The float operation function
    /// * `unsigned_op` - The unsigned integer operation function
    /// * `signed_op` - The signed integer operation function
    fn compile_arithmetic_binop(
        llvm_context: &LLVMContext<'ctx>,
        dt: &DataType,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        float_op: impl FnOnce(
            &Builder<'ctx>,
            FloatValue<'ctx>,
            FloatValue<'ctx>,
            &str,
        ) -> Result<FloatValue<'ctx>, BuilderError>,
        unsigned_op: impl Fn(
            &Builder<'ctx>,
            IntValue<'ctx>,
            IntValue<'ctx>,
            &str,
        ) -> Result<IntValue<'ctx>, BuilderError>,
        signed_op: impl Fn(
            &Builder<'ctx>,
            IntValue<'ctx>,
            IntValue<'ctx>,
            &str,
        ) -> Result<IntValue<'ctx>, BuilderError>,
    ) -> BasicValueEnum<'ctx> {
        if dt.is_float() {
            float_op(
                llvm_context.builder(),
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_binop",
            )
            .unwrap()
            .as_basic_value_enum()
        } else {
            let op: &dyn Fn(_, _, _, _) -> _ = if dt.is_sint() {
                &signed_op
            } else {
                &unsigned_op
            };
            op(
                llvm_context.builder(),
                lhs.into_int_value(),
                rhs.into_int_value(),
                "int_binop",
            )
            .unwrap()
            .as_basic_value_enum()
        }
    }

    /// Compares two values using the appropriate LLVM comparison instruction for floats or integers.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] providing the [`Builder`]
    /// * `lhs` - The left-hand side LLVM value
    /// * `rhs` - The right-hand side LLVM value
    /// * `dt` - The [`DataType`] determining float vs integer and signed vs unsigned
    /// * `float_op` - The float comparison predicate
    /// * `unsigned_int_op` - The unsigned integer comparison predicate
    /// * `signed_int_op` - The signed integer comparison predicate
    fn compile_cmp(
        llvm_context: &LLVMContext<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        dt: &DataType,
        float_op: FloatPredicate,
        unsigned_int_op: IntPredicate,
        signed_int_op: IntPredicate,
    ) -> BasicValueEnum<'ctx> {
        if dt.is_float() {
            llvm_context
                .builder()
                .build_float_compare(
                    float_op,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "cmp",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            let op = if dt.is_sint() {
                signed_int_op
            } else {
                unsigned_int_op
            };
            llvm_context
                .builder()
                .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    /// Compiles a non-void function call expression, returning the call result as a [`BasicValueEnum`].
    ///
    /// Delegates to [`compile_call`](Codegen::compile_call) and extracts
    /// the result as a basic value. Panics if the called function returns void.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The function call expression to compile
    ///
    /// # Panics
    ///
    /// If the called function returns void.
    pub(crate) fn compile_nonvoid_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &FunctionCall<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        self.compile_call(llvm_context, vars, statement_context, to_generate)
            .try_as_basic_value()
            .basic()
            .expect("Void call as expression")
    }

    /// Compiles a struct instantiation (`new` expression), allocating memory on the heap and
    /// initializing fields with the provided argument values.
    ///
    /// For each field:
    /// 1. Compiles the argument expression
    /// 2. Computes the struct field pointer via `struct.gep`
    /// 3. Stores the compiled value into the field
    ///
    /// The struct's refcount is initialized to 1 by [`allocate`](Self::allocate).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The struct instantiation expression
    ///
    /// # Returns
    ///
    /// The LLVM [`BasicValueEnum`] pointer to the allocated struct.
    pub(crate) fn compile_new_struct(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &NewStruct<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let tr = llvm_context.type_registry();
        let to_alloc = tr.get_struct(to_generate.symbol()).expect("Unknown struct");
        let alloc = self.allocate(llvm_context, to_alloc.lowered(), "alloc_struct");
        for (i, field) in to_alloc.fields().iter().enumerate() {
            let val = to_generate
                .parameters()
                .iter()
                .find(|param| &*param.0 == field)
                .expect("Struct field is not provided");
            let val = self.compile_expression(llvm_context, vars, statement_context, &val.1);
            #[allow(clippy::cast_possible_truncation)]
            let field = llvm_context
                .builder()
                .build_struct_gep(to_alloc.lowered(), alloc, i as u32 + 1, "field_init_gep")
                .expect("Unknown struct field");
            llvm_context.builder().build_store(field, val).unwrap();
        }
        alloc.as_basic_value_enum()
    }

    /// Compiles an enum variant construction (`new` expression), allocating memory on the heap,
    /// setting the discriminant tag, and initializing variant fields.
    ///
    /// For each variant field:
    /// 1. Compiles the argument expression
    /// 2. Computes the struct field pointer via `struct.gep` (offset by 2 for discriminant and size)
    /// 3. Stores the compiled value into the field
    ///
    /// After field initialization, sets the discriminant tag (index 1) to the variant's index.
    /// The enum's refcount is initialized to 1 by [`allocate`](Self::allocate).
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The enum variant construction expression
    ///
    /// # Returns
    ///
    /// The LLVM [`BasicValueEnum`] pointer to the allocated enum variant.
    pub(crate) fn compile_new_enum(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &NewEnum<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let tr = llvm_context.type_registry();
        let en = tr.get_enum(to_generate.to_create()).expect("Unknown enum");
        let to_alloc = en.lookup(to_generate.variant()).expect("Unknown variant");
        let alloc = self.allocate(llvm_context, to_alloc, "alloc_enum");
        for (i, field) in to_generate.parameters().iter().enumerate() {
            let val = self.compile_expression(llvm_context, vars, statement_context, field);
            #[allow(clippy::cast_possible_truncation)]
            let field = llvm_context
                .builder()
                .build_struct_gep(to_alloc, alloc, i as u32 + 2, "field_init_gep")
                .expect("Unknown enum field");
            llvm_context.builder().build_store(field, val).unwrap();
        }
        let field_tag = en.index_of(to_generate.variant()).expect("Unknown variant");
        let tag = llvm_context
            .builder()
            .build_struct_gep(to_alloc, alloc, 1, "tag_init_gep")
            .expect("Tag is missing");
        llvm_context
            .builder()
            .build_store(
                tag,
                self.context.i32_type().const_int(field_tag as u64, false),
            )
            .unwrap();
        alloc.as_basic_value_enum()
    }

    /// Compiles a struct field access expression, loading the field value and managing
    /// reference counts (increment on load, decrement on struct drop).
    ///
    /// The operation:
    /// 1. Compiles the struct expression to get a pointer
    /// 2. Increments the refcount (via `compile_val_create`)
    /// 3. Loads the field value
    /// 4. Decrements the original struct's refcount (via `compile_struct_dec_refcount`)
    ///
    /// This implements the copy-on-read semantics where accessing a field creates a new
    /// reference to the field's value while releasing the reference to the parent struct.
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type lookups and IR operations
    /// * `vars` - The [`VariableTable`] for variable lookups
    /// * `statement_context` - The [`StatementContext`] for block management
    /// * `to_generate` - The struct field access expression
    ///
    /// # Returns
    ///
    /// The loaded LLVM [`BasicValueEnum`] for the field value.
    pub(crate) fn compile_sfa(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &StructFieldAccess<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let of = self
            .compile_expression(llvm_context, vars, statement_context, to_generate.of())
            .into_pointer_value();
        let DataType::Struct(struct_type) = to_generate.of().data_type() else {
            unreachable!()
        };
        let tr = llvm_context.type_registry();
        let struct_type_lowered = tr.get_struct(&struct_type).expect("Unknown struct");
        let struct_field = struct_type_lowered
            .fields()
            .iter()
            .position(|field| field == to_generate.field())
            .expect("Unknown field");
        #[allow(clippy::cast_possible_truncation)]
        let field = llvm_context
            .builder()
            .build_struct_gep(
                struct_type_lowered.lowered(),
                of,
                struct_field as u32 + 1,
                "field_access_gep",
            )
            .expect("Unknown struct field");
        let val = llvm_context
            .builder()
            .build_load(
                llvm_context.lower_type(&to_generate.data_type()),
                field,
                "var_load",
            )
            .unwrap();
        self.compile_val_create(llvm_context, val);
        self.compile_struct_dec_refcount(
            llvm_context,
            statement_context.function_context_mut(),
            &struct_type,
            of,
        );
        val
    }

    /// Converts a data type to its corresponding LLVM integer type for typecast operations.
    ///
    /// # Arguments
    ///
    /// * `cast` - The [`DataType`] to convert
    ///
    /// # Returns
    ///
    /// The corresponding LLVM [`IntType`].
    ///
    /// # Panics
    ///
    /// If the data type is not a valid integer type (i.e., not U8, S8, U16, S16, U32, S32, U64, or S64).
    fn int_dt_to_llvm_dt(&self, cast: &DataType) -> IntType<'ctx> {
        use DataType as D;
        match cast {
            D::U8 | D::S8 => self.context.i8_type(),
            D::U16 | D::S16 => self.context.i16_type(),
            D::U32 | D::S32 => self.context.i32_type(),
            D::U64 | D::S64 => self.context.i64_type(),
            _ => unreachable!(),
        }
    }

    /// Allocates memory on the heap for a struct type using `malloc` and initializes the refcount to 1.
    ///
    /// All other fields are left uninitialized
    ///
    /// # Arguments
    ///
    /// * `llvm_context` - The [`LLVMContext`] for type size queries and IR operations
    /// * `struct_type` - The LLVM [`StructType`] whose size is used for allocation
    /// * `name` - The name for the size truncation instruction
    ///
    /// # Returns
    ///
    /// A [`PointerValue`] to the allocated memory with refcount initialized to 1.
    fn allocate(
        &self,
        llvm_context: &LLVMContext<'ctx>,
        struct_type: StructType<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let size = llvm_context
            .builder()
            .build_int_truncate(
                struct_type.size_of().expect("Should be sized"),
                self.context.i32_type(),
                "size_resize",
            )
            .unwrap();
        let alloc = llvm_context
            .builder()
            .build_call(
                llvm_context.global_registry().malloc(),
                &[size.into()],
                name,
            )
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();
        Self::write_refcount(
            llvm_context,
            alloc,
            self.context.i32_type().const_int(1, false),
        );
        alloc
    }
}
