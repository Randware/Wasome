use crate::Codegen;
use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use ast::TypedAST;
use ast::data_type::{DataType, Typed};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, NewEnum, NewStruct,
    StructFieldAccess, UnaryOp, UnaryOpType,
};
use ast::symbol::VariableSymbol;
use inkwell::types::IntType;
use inkwell::values::{BasicValueEnum, FloatValue, IntValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

impl<'ctx, 'fc> Codegen<'ctx> {
    pub(crate) fn compile_expression(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &Expression<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        match to_generate {
            Expression::FunctionCall(call) => {
                self.compile_call(llvm_context, vars, statement_context, call)
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
                llvm_context.lower_type(&to_generate.data_type()),
                var,
                "var_load",
            )
            .unwrap();
        if to_generate.data_type().is_prt() {
            self.compile_inc_refcount(llvm_context, val.into_pointer_value());
        }
        val
    }

    pub(crate) fn compile_literal(&self, to_generate: &Literal) -> BasicValueEnum<'ctx> {
        match to_generate {
            Literal::S32(val) =>
            {
                #[allow(clippy::cast_sign_loss)]
                BasicValueEnum::IntValue(self.context.i32_type().const_int(*val as u64, true))
            }
            Literal::Bool(val) => {
                BasicValueEnum::IntValue(self.context.bool_type().const_int(u64::from(*val), false))
            }
            Literal::Char(val) => {
                BasicValueEnum::IntValue(self.context.i32_type().const_int(u64::from(*val), false))
            }
            Literal::F64(val) => {
                BasicValueEnum::FloatValue(self.context.f64_type().const_float(*val))
            }
        }
    }

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
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_mul(
                                match inner {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                self.context.f64_type().const_float(-1.0),
                                "negative",
                            )
                            .unwrap(),
                    )
                } else {
                    BasicValueEnum::IntValue(
                        llvm_context
                            .builder()
                            .build_int_mul(
                                match inner {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                self.context.i64_type().const_all_ones(),
                                "negative",
                            )
                            .unwrap(),
                    )
                }
            }
            UnaryOpType::Not => BasicValueEnum::IntValue(
                llvm_context
                    .builder()
                    .build_not(
                        match inner {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "not",
                    )
                    .unwrap(),
            ),
            UnaryOpType::Typecast(cast) => {
                use DataType as D;
                match (to_generate.input().data_type(), cast.target()) {
                    (D::F32, D::F64) => BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_ext(
                                match inner {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                self.context.f64_type(),
                                "cast",
                            )
                            .unwrap(),
                    ),
                    (D::F64, D::F32) => BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_trunc(
                                match inner {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                self.context.f32_type(),
                                "cast",
                            )
                            .unwrap(),
                    ),
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
                            .build_bit_cast(
                                match inner {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                target,
                                "cast",
                            )
                            .unwrap()
                            .into_int_value();
                        if cast.target().is_sint() {
                            BasicValueEnum::IntValue(val)
                        } else {
                            BasicValueEnum::IntValue(val)
                        }
                    }
                    (D::S8 | D::U8, D::S16 | D::U16 | D::S32 | D::U32 | D::S64 | D::U64)
                    | (D::S16 | D::U16, D::S32 | D::U32 | D::S64 | D::U64)
                    | (D::S32 | D::U32, D::S64 | D::U64) => {
                        let target = self.int_dt_to_llvm_dt(cast.target());
                        if cast.target().is_sint() {
                            BasicValueEnum::IntValue(
                                llvm_context
                                    .builder()
                                    .build_int_z_extend::<IntValue<'ctx>>(
                                        match inner {
                                            BasicValueEnum::IntValue(i) => i,
                                            _ => unreachable!(),
                                        },
                                        target,
                                        "cast",
                                    )
                                    .unwrap(),
                            )
                        } else {
                            BasicValueEnum::IntValue(
                                llvm_context
                                    .builder()
                                    .build_int_s_extend(
                                        match inner {
                                            BasicValueEnum::IntValue(i) => i,
                                            _ => unreachable!(),
                                        },
                                        target,
                                        "cast",
                                    )
                                    .unwrap(),
                            )
                        }
                    }
                    (D::S16 | D::U16 | D::S32 | D::U32 | D::S64 | D::U64, D::S8 | D::U8)
                    | (D::S32 | D::U32 | D::S64 | D::U64, D::S16 | D::U16)
                    | (D::S64 | D::U64, D::S32 | D::U32) => {
                        let target = self.int_dt_to_llvm_dt(cast.target());
                        let trunc = llvm_context
                            .builder()
                            .build_int_z_extend(
                                match inner {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                target,
                                "cast",
                            )
                            .unwrap();
                        if cast.target().is_sint() {
                            BasicValueEnum::IntValue(trunc)
                        } else {
                            BasicValueEnum::IntValue(trunc)
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

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
            BinaryOpType::Addition => {
                if dt.is_float() {
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_add(
                                match lhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                "add",
                            )
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_add(
                            match lhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            match rhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            "add",
                        )
                        .unwrap();
                    BasicValueEnum::IntValue(val)
                }
            }
            BinaryOpType::Subtraction => {
                if dt.is_float() {
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_sub(
                                match lhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                "sub",
                            )
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_sub(
                            match lhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            match rhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            "sub",
                        )
                        .unwrap();
                    BasicValueEnum::IntValue(val)
                }
            }
            BinaryOpType::Multiplication => {
                if dt.is_float() {
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_mul(
                                match lhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                "mul",
                            )
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_mul(
                            match lhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            match rhs {
                                BasicValueEnum::IntValue(i) => i,
                                _ => unreachable!(),
                            },
                            "mul",
                        )
                        .unwrap();
                    BasicValueEnum::IntValue(val)
                }
            }
            BinaryOpType::Division => {
                if dt.is_float() {
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_div(
                                match lhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                "div",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    BasicValueEnum::IntValue(
                        llvm_context
                            .builder()
                            .build_int_signed_div(
                                match lhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                "div",
                            )
                            .unwrap(),
                    )
                } else {
                    BasicValueEnum::IntValue(
                        llvm_context
                            .builder()
                            .build_int_unsigned_div(
                                match lhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                "div",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Modulo => {
                if dt.is_float() {
                    BasicValueEnum::FloatValue(
                        llvm_context
                            .builder()
                            .build_float_rem(
                                match lhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::FloatValue(f) => f,
                                    _ => unreachable!(),
                                },
                                "mod",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    BasicValueEnum::IntValue(
                        llvm_context
                            .builder()
                            .build_int_signed_rem(
                                match lhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                "mod",
                            )
                            .unwrap(),
                    )
                } else {
                    BasicValueEnum::IntValue(
                        llvm_context
                            .builder()
                            .build_int_unsigned_rem(
                                match lhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                match rhs {
                                    BasicValueEnum::IntValue(i) => i,
                                    _ => unreachable!(),
                                },
                                "mod",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::LeftShift => {
                let val = llvm_context
                    .builder()
                    .build_left_shift(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "lshift",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::RightShift => {
                let val = llvm_context
                    .builder()
                    .build_right_shift(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        dt.is_sint(),
                        "rshift",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::BitwiseOr => {
                let val = llvm_context
                    .builder()
                    .build_or(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "bitor",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::Or => {
                let val = llvm_context
                    .builder()
                    .build_or(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "or",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::BitwiseAnd => {
                let val = llvm_context
                    .builder()
                    .build_and(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "bitand",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::And => {
                let val = llvm_context
                    .builder()
                    .build_and(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "and",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::BitwiseXor => {
                let val = llvm_context
                    .builder()
                    .build_xor(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "bitxor",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::Xor => {
                let val = llvm_context
                    .builder()
                    .build_xor(
                        match lhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        match rhs {
                            BasicValueEnum::IntValue(i) => i,
                            _ => unreachable!(),
                        },
                        "xor",
                    )
                    .unwrap();
                BasicValueEnum::IntValue(val)
            }
            BinaryOpType::Equals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::OEQ,
                IntPredicate::EQ,
                IntPredicate::EQ,
            ),
            BinaryOpType::NotEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::ONE,
                IntPredicate::NE,
                IntPredicate::NE,
            ),
            BinaryOpType::Greater => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::OGT,
                IntPredicate::SGT,
                IntPredicate::UGT,
            ),
            BinaryOpType::GreaterEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::OGE,
                IntPredicate::SGE,
                IntPredicate::UGE,
            ),
            BinaryOpType::Lesser => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::OLT,
                IntPredicate::SLT,
                IntPredicate::ULT,
            ),
            BinaryOpType::LesserEquals => Self::compile_cmp(
                llvm_context,
                lhs,
                rhs,
                dt,
                FloatPredicate::OLE,
                IntPredicate::SLE,
                IntPredicate::ULE,
            ),
        }
    }

    fn compile_cmp(
        llvm_context: &LLVMContext<'ctx>,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        dt: DataType,
        float_op: FloatPredicate,
        unsigned_int_op: IntPredicate,
        signed_int_op: IntPredicate,
    ) -> BasicValueEnum<'ctx> {
        if dt.is_float() {
            BasicValueEnum::IntValue(
                llvm_context
                    .builder()
                    .build_float_compare(
                        float_op,
                        lhs.into_float_value(),
                        lhs.into_float_value(),
                        "cmp",
                    )
                    .unwrap(),
            )
        } else {
            let op = if dt.is_sint() {
                signed_int_op
            } else {
                unsigned_int_op
            };
            BasicValueEnum::IntValue(
                llvm_context
                    .builder()
                    .build_int_compare(op, lhs.into_int_value(), rhs.into_int_value(), "cmp")
                    .unwrap(),
            )
        }
    }

    pub(crate) fn compile_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &FunctionCall<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
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
            .unwrap()
            .try_as_basic_value()
            .basic()
            .expect("Void call as expression");
        for arg in args.iter().zip(to_generate.args()) {
            match arg.1.data_type() {
                DataType::Struct(st) => self.compile_struct_dec_refcount(
                    llvm_context,
                    statement_context.function_context().current_function(),
                    &st,
                    match arg.0 {
                        BasicValueEnum::PointerValue(p) => *p,
                        _ => unreachable!(),
                    },
                ),
                DataType::Enum(en) => self.compile_enum_dec_refcount(
                    llvm_context,
                    statement_context.function_context().current_function(),
                    &en,
                    match arg.0 {
                        BasicValueEnum::PointerValue(p) => *p,
                        _ => unreachable!(),
                    },
                ),
                _ => (),
            }
        }
        ret
    }

    pub(crate) fn compile_new_struct(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &NewStruct<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let tr = llvm_context.type_registry();
        let to_alloc = tr.get_struct(to_generate.symbol()).expect("Unknown struct");
        let alloc = llvm_context
            .builder()
            .build_malloc(to_alloc.lowered(), "alloc_struct")
            .unwrap();
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
        self.compile_inc_refcount(llvm_context, alloc);
        BasicValueEnum::PointerValue(alloc)
    }

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
        let alloc = llvm_context
            .builder()
            .build_malloc(to_alloc, "alloc_enum")
            .unwrap();
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
        self.compile_inc_refcount(llvm_context, alloc);
        BasicValueEnum::PointerValue(alloc)
    }

    pub(crate) fn compile_sfa(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &StructFieldAccess<TypedAST>,
    ) -> BasicValueEnum<'ctx> {
        let of = match self.compile_expression(
            llvm_context,
            vars,
            statement_context,
            to_generate.of(),
        ) {
            BasicValueEnum::PointerValue(p) => p,
            _ => unreachable!(),
        };
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
        if to_generate.data_type().is_prt() {
            self.compile_inc_refcount(llvm_context, val.into_pointer_value());
        }
        self.compile_struct_dec_refcount(
            llvm_context,
            statement_context.function_context().current_function(),
            &struct_type,
            of,
        );
        val
    }

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
}
