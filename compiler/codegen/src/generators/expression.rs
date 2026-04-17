use crate::Codegen;
use crate::context::{LLVMContext, StatementContext};
use crate::symbols::VariableTable;
use crate::value::Value;
use ast::TypedAST;
use ast::data_type::{DataType, Typed};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, NewEnum, NewStruct,
    StructFieldAccess, UnaryOp, UnaryOpType,
};
use ast::symbol::VariableSymbol;
use inkwell::types::IntType;
use inkwell::values::IntValue;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

impl<'ctx, 'fc> Codegen<'ctx> {
    pub(crate) fn compile_expression(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &Expression<TypedAST>,
    ) -> Value<'ctx> {
        match to_generate {
            Expression::FunctionCall(call) => {
                self.compile_call(llvm_context, vars, statement_context, call)
            }
            // Method call is only supposed to exist in the untyped AST
            Expression::MethodCall(_) => unreachable!(),
            Expression::Variable(var) => self.compile_var_access(llvm_context, vars, var),
            Expression::Literal(lit) => self.compile_literal(llvm_context, lit),
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
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &VariableSymbol<TypedAST>,
    ) -> Value<'ctx> {
        let var = vars
            .lookup(to_generate)
            .expect("Undeclared variable in the typed AST")
            .pointer;
        match to_generate.data_type() {
            DataType::Char => Value::Char(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U8 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i8_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S8 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i8_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U16 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i16_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S16 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i16_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U32 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S32 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U64 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i64_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S64 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i64_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::Bool => Value::Bool(
                llvm_context
                    .builder()
                    .build_load(self.context.bool_type(), var, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::F32 => Value::Float(
                llvm_context
                    .builder()
                    .build_load(self.context.f32_type(), var, "var_load")
                    .unwrap()
                    .into_float_value(),
            ),
            DataType::F64 => Value::Float(
                llvm_context
                    .builder()
                    .build_load(self.context.f64_type(), var, "var_load")
                    .unwrap()
                    .into_float_value(),
            ),
            DataType::Struct(_) | DataType::Enum(_) => {
                let prt = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        var,
                        "var_load",
                    )
                    .unwrap()
                    .into_pointer_value();
                self.compile_inc_refcount(llvm_context, prt);
                Value::Ptr(prt)
            }
        }
    }

    pub(crate) fn compile_literal(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        to_generate: &Literal,
    ) -> Value<'ctx> {
        match to_generate {
            Literal::S32(val) => Value::Sint(self.context.i32_type().const_int(*val as u64, true)),
            Literal::Bool(val) => {
                Value::Bool(self.context.bool_type().const_int(*val as u64, false))
            }
            Literal::Char(val) => {
                Value::Char(self.context.i32_type().const_int(*val as u64, false))
            }
            Literal::F64(val) => Value::Float(self.context.f64_type().const_float(*val)),
        }
    }

    pub(crate) fn compile_unary_op(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,

        to_generate: &UnaryOp<TypedAST>,
    ) -> Value<'ctx> {
        let inner =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.input());
        match to_generate.op_type() {
            UnaryOpType::Negative => {
                if to_generate.data_type().is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_mul(
                                inner.into_float(),
                                self.context.f64_type().const_float(-1.0),
                                "negative",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Sint(
                        llvm_context
                            .builder()
                            .build_int_mul(
                                inner.into_sint(),
                                self.context.i64_type().const_all_ones(),
                                "negative",
                            )
                            .unwrap(),
                    )
                }
            }
            UnaryOpType::Not => Value::Bool(
                llvm_context
                    .builder()
                    .build_not(inner.into_bool(), "not")
                    .unwrap(),
            ),
            UnaryOpType::Typecast(cast) => {
                use DataType as D;
                match (to_generate.input().data_type(), cast.target()) {
                    (D::F32, D::F64) => Value::Float(
                        llvm_context
                            .builder()
                            .build_float_ext(inner.into_float(), self.context.f64_type(), "cast")
                            .unwrap(),
                    ),
                    (D::F64, D::F32) => Value::Float(
                        llvm_context
                            .builder()
                            .build_float_trunc(inner.into_float(), self.context.f32_type(), "cast")
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
                            .build_bit_cast(inner.into_int(), target, "cast")
                            .unwrap()
                            .into_int_value();
                        if cast.target().is_sint() {
                            Value::Sint(val)
                        } else {
                            Value::Uint(val)
                        }
                    }
                    (D::S8 | D::U8, D::S16 | D::U16 | D::S32 | D::U32 | D::S64 | D::U64)
                    | (D::S16 | D::U16, D::S32 | D::U32 | D::S64 | D::U64)
                    | (D::S32 | D::U32, D::S64 | D::U64) => {
                        let target = self.int_dt_to_llvm_dt(cast.target());
                        if cast.target().is_sint() {
                            Value::Sint(
                                llvm_context
                                    .builder()
                                    .build_int_z_extend::<IntValue<'ctx>>(
                                        inner.into_int(),
                                        target,
                                        "cast",
                                    )
                                    .unwrap(),
                            )
                        } else {
                            Value::Uint(
                                llvm_context
                                    .builder()
                                    .build_int_s_extend(inner.into_int(), target, "cast")
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
                            .build_int_z_extend(inner.into_int(), target, "cast")
                            .unwrap();
                        if cast.target().is_sint() {
                            Value::Sint(trunc)
                        } else {
                            Value::Uint(trunc)
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
    ) -> Value<'ctx> {
        let lhs =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.left());
        let rhs =
            self.compile_expression(llvm_context, vars, statement_context, to_generate.right());
        let dt = to_generate.left().data_type();
        match to_generate.op_type() {
            BinaryOpType::Addition => {
                if dt.is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_add(lhs.into_float(), rhs.into_float(), "add")
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_add(lhs.into_int(), rhs.into_int(), "add")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                }
            }
            BinaryOpType::Subtraction => {
                if dt.is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_sub(lhs.into_float(), rhs.into_float(), "sub")
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_sub(lhs.into_int(), rhs.into_int(), "sub")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                }
            }
            BinaryOpType::Multiplication => {
                if dt.is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_mul(lhs.into_float(), rhs.into_float(), "mul")
                            .unwrap(),
                    )
                } else {
                    let val = llvm_context
                        .builder()
                        .build_int_mul(lhs.into_int(), rhs.into_int(), "mul")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                }
            }
            BinaryOpType::Division => {
                if dt.is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_div(lhs.into_float(), rhs.into_float(), "div")
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Sint(
                        llvm_context
                            .builder()
                            .build_int_signed_div(lhs.into_int(), rhs.into_int(), "div")
                            .unwrap(),
                    )
                } else {
                    Value::Uint(
                        llvm_context
                            .builder()
                            .build_int_unsigned_div(lhs.into_int(), rhs.into_int(), "div")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Modulo => {
                if dt.is_float() {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_rem(lhs.into_float(), rhs.into_float(), "mod")
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Sint(
                        llvm_context
                            .builder()
                            .build_int_signed_rem(lhs.into_int(), rhs.into_int(), "mod")
                            .unwrap(),
                    )
                } else {
                    Value::Uint(
                        llvm_context
                            .builder()
                            .build_int_unsigned_rem(lhs.into_int(), rhs.into_int(), "mod")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::LeftShift => {
                let val = llvm_context
                    .builder()
                    .build_left_shift(lhs.into_int(), rhs.into_int(), "lshift")
                    .unwrap();
                if dt.is_sint() {
                    Value::Sint(val)
                } else {
                    Value::Uint(val)
                }
            }
            BinaryOpType::RightShift => {
                let val = llvm_context
                    .builder()
                    .build_right_shift(lhs.into_int(), rhs.into_int(), dt.is_sint(), "rshift")
                    .unwrap();
                if dt.is_sint() {
                    Value::Sint(val)
                } else {
                    Value::Uint(val)
                }
            }
            BinaryOpType::BitwiseOr => {
                let val = llvm_context
                    .builder()
                    .build_or(lhs.into_int(), rhs.into_int(), "bitor")
                    .unwrap();
                if dt.is_sint() {
                    Value::Sint(val)
                } else {
                    Value::Uint(val)
                }
            }
            BinaryOpType::Or => {
                let val = llvm_context
                    .builder()
                    .build_or(lhs.into_int(), rhs.into_int(), "or")
                    .unwrap();
                Value::Bool(val)
            }
            BinaryOpType::BitwiseAnd => {
                let val = llvm_context
                    .builder()
                    .build_and(lhs.into_int(), rhs.into_int(), "bitand")
                    .unwrap();
                if dt.is_sint() {
                    Value::Sint(val)
                } else {
                    Value::Uint(val)
                }
            }
            BinaryOpType::And => {
                let val = llvm_context
                    .builder()
                    .build_and(lhs.into_int(), rhs.into_int(), "and")
                    .unwrap();
                Value::Bool(val)
            }
            BinaryOpType::BitwiseXor => {
                let val = llvm_context
                    .builder()
                    .build_xor(lhs.into_int(), rhs.into_int(), "bitxor")
                    .unwrap();
                if dt.is_sint() {
                    Value::Sint(val)
                } else {
                    Value::Uint(val)
                }
            }
            BinaryOpType::Xor => {
                let val = llvm_context
                    .builder()
                    .build_xor(lhs.into_int(), rhs.into_int(), "xor")
                    .unwrap();
                Value::Bool(val)
            }
            BinaryOpType::Equals => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::OEQ,
                                lhs.into_float(),
                                rhs.into_float(),
                                "eq",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::EQ,
                                lhs.into_int(),
                                rhs.into_int(),
                                "eq",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::NotEquals => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::ONE,
                                lhs.into_float(),
                                rhs.into_float(),
                                "neq",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::NE,
                                lhs.into_int(),
                                rhs.into_int(),
                                "neq",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Greater => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::OGT,
                                lhs.into_float(),
                                rhs.into_float(),
                                "gt",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::SGT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "gt",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::UGT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "gt",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::GreaterEquals => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::OGE,
                                lhs.into_float(),
                                rhs.into_float(),
                                "ge",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::SGT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "ge",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::UGT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "ge",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Lesser => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::OLT,
                                lhs.into_float(),
                                rhs.into_float(),
                                "lt",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::SLT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "lt",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::ULT,
                                lhs.into_int(),
                                rhs.into_int(),
                                "lt",
                            )
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::LesserEquals => {
                if dt.is_float() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_float_compare(
                                FloatPredicate::OLE,
                                lhs.into_float(),
                                rhs.into_float(),
                                "le",
                            )
                            .unwrap(),
                    )
                } else if dt.is_sint() {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::SLE,
                                lhs.into_int(),
                                rhs.into_int(),
                                "le",
                            )
                            .unwrap(),
                    )
                } else {
                    Value::Bool(
                        llvm_context
                            .builder()
                            .build_int_compare(
                                IntPredicate::ULE,
                                lhs.into_int(),
                                rhs.into_int(),
                                "le",
                            )
                            .unwrap(),
                    )
                }
            }
        }
    }

    pub(crate) fn compile_call(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &FunctionCall<TypedAST>,
    ) -> Value<'ctx> {
        let func = llvm_context
            .type_registry()
            .get_function(to_generate.function())
            .expect("Call to unknown function!");
        let args = to_generate
            .args()
            .iter()
            .map(|arg| {
                self.compile_expression(llvm_context, vars, statement_context, arg)
                    .into_basic_value_enum()
            })
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
                    arg.0.into_pointer_value(),
                ),
                DataType::Enum(en) => self.compile_enum_dec_refcount(
                    llvm_context,
                    statement_context.function_context().current_function(),
                    &en,
                    arg.0.into_pointer_value(),
                ),
                _ => (),
            }
        }
        match to_generate
            .function()
            .return_type()
            .expect("Void call as expression")
        {
            DataType::Char => Value::Char(ret.into_int_value()),
            DataType::U8 | DataType::U16 | DataType::U32 | DataType::U64 => {
                Value::Uint(ret.into_int_value())
            }
            DataType::S8 | DataType::S16 | DataType::S32 | DataType::S64 => {
                Value::Sint(ret.into_int_value())
            }
            DataType::Bool => Value::Bool(ret.into_int_value()),
            DataType::F32 | DataType::F64 => Value::Float(ret.into_float_value()),
            DataType::Struct(_) => Value::Ptr(ret.into_pointer_value()),
            DataType::Enum(_) => Value::Ptr(ret.into_pointer_value()),
        }
    }

    pub(crate) fn compile_new_struct(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &NewStruct<TypedAST>,
    ) -> Value<'ctx> {
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
            let field = llvm_context
                .builder()
                .build_struct_gep(to_alloc.lowered(), alloc, i as u32 + 1, "field_init_gep")
                .expect("Unknown struct field");
            llvm_context
                .builder()
                .build_store(field, val.into_basic_value_enum())
                .unwrap();
        }
        self.compile_inc_refcount(llvm_context, alloc);
        Value::Ptr(alloc)
    }

    pub(crate) fn compile_new_enum(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &NewEnum<TypedAST>,
    ) -> Value<'ctx> {
        let tr = llvm_context.type_registry();
        let en = tr.get_enum(to_generate.to_create()).expect("Unknown enum");
        let to_alloc = en.lookup(to_generate.variant()).expect("Unknown variant");
        let alloc = llvm_context
            .builder()
            .build_malloc(to_alloc, "alloc_enum")
            .unwrap();
        for (i, field) in to_generate.parameters().iter().enumerate() {
            let val = self.compile_expression(llvm_context, vars, statement_context, field);
            let field = llvm_context
                .builder()
                .build_struct_gep(to_alloc, alloc, i as u32 + 2, "field_init_gep")
                .expect("Unknown enum field");
            llvm_context
                .builder()
                .build_store(field, val.into_basic_value_enum())
                .unwrap();
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
        Value::Ptr(alloc)
    }

    pub(crate) fn compile_sfa(
        &mut self,
        llvm_context: &LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        statement_context: &mut StatementContext<'ctx, 'fc>,
        to_generate: &StructFieldAccess<TypedAST>,
    ) -> Value<'ctx> {
        let of = self
            .compile_expression(llvm_context, vars, statement_context, to_generate.of())
            .into_prt();
        let struct_type = match to_generate.of().data_type() {
            DataType::Struct(st) => st,
            _ => unreachable!(),
        };
        let tr = llvm_context.type_registry();
        let struct_type_lowered = tr.get_struct(&struct_type).expect("Unknown struct");
        let struct_field = struct_type_lowered
            .fields()
            .iter()
            .position(|field| field == to_generate.field())
            .expect("Unknown field");
        let field = llvm_context
            .builder()
            .build_struct_gep(
                struct_type_lowered.lowered(),
                of,
                struct_field as u32 + 1,
                "field_access_gep",
            )
            .expect("Unknown struct field");

        let result = match to_generate.data_type() {
            DataType::Char => Value::Char(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U8 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i8_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S8 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i8_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U16 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i16_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S16 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i16_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U32 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S32 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i32_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::U64 => Value::Uint(
                llvm_context
                    .builder()
                    .build_load(self.context.i64_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::S64 => Value::Sint(
                llvm_context
                    .builder()
                    .build_load(self.context.i64_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::Bool => Value::Bool(
                llvm_context
                    .builder()
                    .build_load(self.context.bool_type(), field, "var_load")
                    .unwrap()
                    .into_int_value(),
            ),
            DataType::F32 => Value::Float(
                llvm_context
                    .builder()
                    .build_load(self.context.f32_type(), field, "var_load")
                    .unwrap()
                    .into_float_value(),
            ),
            DataType::F64 => Value::Float(
                llvm_context
                    .builder()
                    .build_load(self.context.f64_type(), field, "var_load")
                    .unwrap()
                    .into_float_value(),
            ),
            DataType::Struct(_) | DataType::Enum(_) => {
                let prt = llvm_context
                    .builder()
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        field,
                        "var_load",
                    )
                    .unwrap()
                    .into_pointer_value();
                self.compile_inc_refcount(llvm_context, prt);
                Value::Ptr(prt)
            }
        };
        self.compile_struct_dec_refcount(
            llvm_context,
            statement_context.function_context().current_function(),
            &struct_type,
            of,
        );
        result
    }

    fn int_dt_to_llvm_dt(&mut self, cast: &DataType) -> IntType<'ctx> {
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
