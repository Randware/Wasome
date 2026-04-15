use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};

pub(crate) enum Value<'a> {
    Uint(IntValue<'a>),
    Sint(IntValue<'a>),
    Float(FloatValue<'a>),
    Ptr(PointerValue<'a>),
    Char(IntValue<'a>),
    Bool(IntValue<'a>),
}

impl<'a> Value<'a> {
    pub(crate) fn into_float(self) -> FloatValue<'a> {
        match self {
            Value::Float(float) => float,
            _ => panic!("This is not a float"),
        }
    }

    pub(crate) fn into_sint(self) -> IntValue<'a> {
        match self {
            Value::Sint(int) => int,
            _ => panic!("This is not a sint"),
        }
    }

    pub(crate) fn into_int(self) -> IntValue<'a> {
        match self {
            Value::Uint(int) => int,
            Value::Sint(int) => int,
            _ => panic!("This is not an int"),
        }
    }

    pub(crate) fn into_bool(self) -> IntValue<'a> {
        match self {
            Value::Bool(bool) => bool,
            _ => panic!("This is not a bool"),
        }
    }

    pub(crate) fn into_prt(self) -> PointerValue<'a> {
        match self {
            Value::Ptr(bool) => bool,
            _ => panic!("This is not a pointer"),
        }
    }

    pub(crate) fn into_basic_value_enum(self) -> BasicValueEnum<'a> {
        match self {
            Value::Uint(val) | Value::Sint(val) | Value::Char(val) | Value::Bool(val) => {
                BasicValueEnum::IntValue(val)
            }
            Value::Float(float) => BasicValueEnum::FloatValue(float),
            Value::Ptr(prt) => BasicValueEnum::PointerValue(prt),
        }
    }
}
