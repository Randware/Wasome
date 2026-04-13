use crate::Codegen;
use crate::context::LLVMContext;
use crate::symbols::VariableTable;
use crate::value::Value;
use ast::TypedAST;
use ast::data_type::{DataType, Typed};
use ast::expression::{
    BinaryOp, BinaryOpType, Expression, FunctionCall, Literal, UnaryOp, UnaryOpType,
};
use ast::symbol::VariableSymbol;
use inkwell::types::IntType;
use inkwell::values::IntValue;
use inkwell::{FloatPredicate, IntPredicate};
use std::env::var;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_expression(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &Expression<TypedAST>,
    ) -> Value<'ctx> {
        match to_generate {
            Expression::FunctionCall(call) => self.compile_call(llvm_context, vars, call),
            // Method call is only supposed to exist in the untyped AST
            Expression::MethodCall(_) => unreachable!(),
            Expression::Variable(var) => self.compile_var_access(llvm_context, vars, var),
            Expression::Literal(lit) => self.compile_literal(llvm_context, lit),
            Expression::UnaryOp(un) => self.compile_unary_op(llvm_context, vars, un),
            Expression::BinaryOp(bin) => self.compile_binary_op(llvm_context, vars, bin),
            Expression::NewStruct(_) => todo!(),
            Expression::NewEnum(_) => todo!(),
            Expression::StructFieldAccess(_) => todo!(),
        }
    }

    pub(crate) fn compile_var_access(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
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
            DataType::Struct(_) => todo!(),
            DataType::Enum(_) => todo!(),
        }
    }

    pub(crate) fn compile_literal(
        &mut self,
        llvm_context: &mut LLVMContext<'ctx>,
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
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &UnaryOp<TypedAST>,
    ) -> Value<'ctx> {
        let inner = self.compile_expression(llvm_context, vars, to_generate.input());
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
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
        to_generate: &BinaryOp<TypedAST>,
    ) -> Value<'ctx> {
        let lhs = self.compile_expression(llvm_context, vars, to_generate.left());
        let rhs = self.compile_expression(llvm_context, vars, to_generate.right());
        let dt = to_generate.left().data_type();
        match to_generate.op_type() {
            BinaryOpType::Addition => {
                if dt.is_float() {
                    let val = llvm_context
                        .builder()
                        .build_int_add(lhs.into_int(), rhs.into_int(), "add")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                } else {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_add(lhs.into_float(), rhs.into_float(), "add")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Subtraction => {
                if dt.is_float() {
                    let val = llvm_context
                        .builder()
                        .build_int_sub(lhs.into_int(), rhs.into_int(), "sub")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                } else {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_sub(lhs.into_float(), rhs.into_float(), "sub")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Multiplication => {
                if dt.is_float() {
                    let val = llvm_context
                        .builder()
                        .build_int_mul(lhs.into_int(), rhs.into_int(), "mul")
                        .unwrap();
                    if dt.is_sint() {
                        Value::Sint(val)
                    } else {
                        Value::Uint(val)
                    }
                } else {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_mul(lhs.into_float(), rhs.into_float(), "mul")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Division => {
                if dt.is_float() {
                    if dt.is_sint() {
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
                } else {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_div(lhs.into_float(), rhs.into_float(), "div")
                            .unwrap(),
                    )
                }
            }
            BinaryOpType::Modulo => {
                if dt.is_float() {
                    if dt.is_sint() {
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
                } else {
                    Value::Float(
                        llvm_context
                            .builder()
                            .build_float_rem(lhs.into_float(), rhs.into_float(), "mod")
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
        llvm_context: &mut LLVMContext<'ctx>,
        vars: &VariableTable<'ctx>,
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
                self.compile_expression(llvm_context, vars, arg)
                    .into_basic_value_enum()
                    .into()
            })
            .collect::<Vec<_>>();
        let ret = llvm_context
            .builder()
            .build_call(func, &args, "call")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .expect("Void call as expression");
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
