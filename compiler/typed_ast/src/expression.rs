use std::rc::Rc;
use crate::data_type::{DataType, Type};
use crate::eq_return_option;
use crate::symbol::{FunctionSymbol, VariableSymbol};

/** This represents an expression as per section 2 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Expression
{
    // Only valid if it doesn't return void
    FunctionCall(Rc<FunctionSymbol>),
    Variable(Rc<VariableSymbol>),
    Literal(Literal),
    UnaryOp(Box<UnaryOp>), // The boxes prevent this from becoming an infinitely sized type
    BinaryOp(Box<BinaryOp>)
}

impl Type for Expression
{
    fn data_type(&self) -> DataType
    {
        use Expression as Ex;
        match self {
            // Unwrap safety:
            // It may only exist if the return type is not void
            Ex::FunctionCall(inner) => inner.return_type().unwrap(),
            Ex::Literal(inner) => inner.data_type(),
            Ex::UnaryOp(inner) => inner.data_type(),
            Ex::BinaryOp(inner) => inner.data_type(),
            Ex::Variable(inner) => inner.data_type()
        }
    }
}


/** This represents a literal as described at the end of section 2 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Literal
{
    I32(i32),
    I64(i64),
    Bool(bool),
    Char(u8), //Ascii character, todo: Maybe change to an arbitiary UTF8-char
    F32(f32),
    F64(f64)
}

impl Literal
{
    pub fn data_type(&self) -> DataType
    {
        use DataType as DT;
        match self {
            Literal::I32(_) => DT::I32,
            Literal::I64(_) => DT::I64,
            Literal::Bool(_) => DT::Bool,
            Literal::Char(_) => DT::Char,
            Literal::F32(_) => DT::F32,
            Literal::F64(_) => DT::F64
        }
    }
}

/** This is a type of operator that only takes one input
*/
#[derive(Debug, PartialEq)]
pub struct UnaryOp
{
    // The type of the expression
    op_type: UnaryOpType,
    // The expression to "process"
    input: Expression
}

impl UnaryOp
{
    /** Creates a new instance of UnaryOp
    @params
    op_type: The type of this expression
    input: The expression to base this on
    @return
    Some(output data type) if the provided type can be processed to the output type
    None if the processed type can't be processed
    */
    pub fn new(op_type: UnaryOpType, input: Expression) -> Option<Self>
    {
        // Can't process
        op_type.type_from_processing_type(input.data_type())?;
        Some(Self {
            op_type,
            input
        })
    }
}

impl Type for UnaryOp
{
    fn data_type(&self) -> DataType
    {
        self.op_type.type_from_processing_type(self.input.data_type())
            .unwrap()   // Unwrap safety: UnaryOps may only exist if the input type can be processed
                        // This is checked in the constructor
                        // Therefore, this can never panic
    }
}

/** This is the type of an unary operator
*/
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOpType
{
    /// Section 3 lang spec, subtract operator with only one operand
    Negative,
    /// Section 3 lang spec, not operator
    Not,
    /// Section 11 lang spec
    Typecast(Typecast)
}

impl UnaryOpType
{
    /** Gets the type from processing the provided type
    @params
    to_process: The provided data type
    @return
    Some(output data type) if the provided type can be processed to the output type
    None if the processed type can't be processed
    */
    pub fn type_from_processing_type(&self, to_process: DataType) -> Option<DataType>
    {
        match self {
            UnaryOpType::Negative => Self::type_from_processing_type_with_minus(to_process),
            UnaryOpType::Not => Self::type_from_processing_type_with_neg(to_process),
            UnaryOpType::Typecast(inner) => inner.type_from_processing_type(to_process)
        }
    }

    fn type_from_processing_type_with_neg(to_process: DataType) -> Option<DataType>
    {
        match to_process {
            DataType::Bool => Some(DataType::Bool),
            _ => None
        }
    }

    fn type_from_processing_type_with_minus(to_process: DataType) -> Option<DataType>
    {
        match to_process {
            DataType::Char | DataType::Bool => None,
            _ => Some(to_process)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
/** Represents a typecast that casts the provided type to target
*/
pub struct Typecast
{
    target: DataType
}

impl Typecast
{
    /** Creates a new Typecast
    */
    pub fn new(target: DataType) -> Self
    {
        Self {
            target
        }
    }

    /** Returns what a specific type typecasted with self would return
    @Param
    to_process: The Type to process
    @Return
    Some(result) if to_process can be casted to result
    None if there is no cast available
    */
    pub fn type_from_processing_type(&self, to_process: DataType) -> Option<DataType>
    {
        match (to_process, self.target) {
            (DataType::I32, DataType::F32) |
            (DataType::I32, DataType::I64) |
            (DataType::I64, DataType::I32) |
            (DataType::I64, DataType::F64) |
            (DataType::F64, DataType::F32) |
            (DataType::F64, DataType::I64) |
            (DataType::F32, DataType::F64) |
            (DataType::F32, DataType::I32)  => Some(self.target),
            _ => None
        }
    }
}


/** This is a type of operator that takes two inputs
*/
#[derive(Debug, PartialEq)]
pub struct BinaryOp
{
    // The type of the expression
    op_type: BinaryOpType,
    // The left expression to process
    left: Expression,
    // The left expression to process
    right: Expression
}

impl BinaryOp
{
    /** Creates a new instance of UnaryOp
       @params
       op_type: The type of this expression
       input: The expression to base this on
       @return
       Some(output data type) if the provided type can be processed to the output type
       None if the processed type can't be processed
    */
    pub fn new(op_type: BinaryOpType, left: Expression, right: Expression) -> Option<Self>
    {
        // Can't process
        op_type.type_from_processing_type(left.data_type(), right.data_type())?;
        Some(Self {
            op_type,
            left,
            right
        })

    }
}

impl Type for BinaryOp
{
    fn data_type(&self) -> DataType
    {
        self.op_type.type_from_processing_type(self.left.data_type(), self.right.data_type())
            .unwrap()   // Unwrap safety: BinaryOps may only exist if the input type can be processed
                        // This is checked in the constructor
                        // Therefore, this can never panic
    }
}

/** This is the type of an unary operator
*/
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOpType
{
    /// Section 3 lang spec, addition operator
    Addition,
    /// Section 3 lang spec, subtraction operator with two operands
    Subtraction,
    /// Section 3 lang spec, multiplication operator
    Multiplication,
    /// Section 3 lang spec, division operator
    Division,
    /// Section 3 lang spec, modulo operator
    Modulo,
    /// Section 3 lang spec, left shift operator
    LeftShift,
    /// Section 3 lang spec, right shift operator
    RightShift,
    /// Section 3 lang spec, bitwise or operator
    BitwiseOr,
    /// Section 3 lang spec, or operator
    Or,
    /// Section 3 lang spec, bitwise and operator
    BitwiseAnd,
    /// Section 3 lang spec, and operator
    And,
    /// Section 3 lang spec, bitwise xor operator
    BitwiseXor,
    /// Section 3 lang spec, xor operator
    Xor,
    /// Section 3 lang spec, equals operator
    Equals,
    /// Section 3 lang spec, not equals operator
    NotEquals,
    /// Section 3 lang spec, greater operator
    Greater,
    /// Section 3 lang spec, greater or equals operator
    GreaterEquals,
    /// Section 3 lang spec, lesser operator
    Lesser,
    /// Section 3 lang spec, lesser or equals operator
    LesserEquals
}

impl BinaryOpType
{
    /** Gets the type from processing the provided type
       @params
       left, right: The provided data types
       @return
       Some(output data type) if the provided types can be processed to the output type
       None if the processed type can't be processed
    */
    pub fn type_from_processing_type(&self, left: DataType, right: DataType) -> Option<DataType>
    {
        use BinaryOpType as BOT;
        match self {
            BOT::Addition |
            BOT::Subtraction |
            BOT::Multiplication |
            BOT::Division => Self::type_from_processing_types_with_base_arithmetic(left, right),
            BOT::Modulo |
            BOT::LeftShift |
            BOT::RightShift |
            BOT::BitwiseOr |
            BOT::BitwiseAnd |
            BOT::BitwiseXor => Self::type_from_processing_types_with_integer_only_operator(left, right),
            BOT::Or |
            BOT::And |
            BOT::Xor => Self::type_from_processing_types_with_bool_operator(left, right),
            BOT::Equals | BOT::NotEquals => {
                    eq_return_option(left, right)?;
                    Some(DataType::Bool)
                },
            BOT::Greater |
            BOT::GreaterEquals |
            BOT::Lesser |
            BOT:: LesserEquals => Self::type_from_processing_types_with_comparison_operator(left, right)
        }
    }

    fn type_from_processing_types_with_base_arithmetic(left: DataType, right: DataType) -> Option<DataType>
    {
        eq_return_option(left, right)?;
        match left {
            DataType::I32 |
            DataType::I64 |
            DataType::F32 |
            DataType::F64 => Some(left),
            _ => None
        }
    }

    fn type_from_processing_types_with_integer_only_operator(left: DataType, right: DataType) -> Option<DataType>
    {
        eq_return_option(left, right)?;
        match left {
            DataType::I32 |
            DataType::I64 => Some(left),
            _ => None
        }
    }

    fn type_from_processing_types_with_bool_operator(left: DataType, right: DataType) -> Option<DataType>
    {
        eq_return_option(left, right)?;
        match left {
            DataType::Bool => Some(left),
            _ => None
        }
    }

    fn type_from_processing_types_with_comparison_operator(left: DataType, right: DataType) -> Option<DataType>
    {
        eq_return_option(left, right)?;
        Some(DataType::Bool)
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    #[test]
    fn binary_op_type()
    {
        let add = BinaryOpType::Addition;
        assert_eq!(None, add.type_from_processing_type(DataType::F32, DataType::F64));
        assert_eq!(Some(DataType::F32), add.type_from_processing_type(DataType::F32, DataType::F32));
        assert_eq!(None, add.type_from_processing_type(DataType::Bool, DataType::Bool));

        let bxor = BinaryOpType::BitwiseXor;
        assert_eq!(None, bxor.type_from_processing_type(DataType::F32, DataType::F32));
        assert_eq!(Some(DataType::I64), bxor.type_from_processing_type(DataType::I64, DataType::I64));
    }

    #[test]
    fn unary_op_type()
    {
        let negative = UnaryOpType::Negative;
        assert_eq!(Some(DataType::F32), negative.type_from_processing_type(DataType::F32));
        assert_eq!(None, negative.type_from_processing_type(DataType::Bool));

        let not = UnaryOpType::Not;
        assert_eq!(Some(DataType::Bool), not.type_from_processing_type(DataType::Bool));
        assert_eq!(None, not.type_from_processing_type(DataType::Char));

        let tc_i32 = UnaryOpType::Typecast(Typecast::new(DataType::I32));
        assert_eq!(Some(DataType::I32), tc_i32.type_from_processing_type(DataType::F32));
        assert_eq!(Some(DataType::I32), tc_i32.type_from_processing_type(DataType::I64));
        assert_eq!(None, tc_i32.type_from_processing_type(DataType::Bool));

        let tc_bool = UnaryOpType::Typecast(Typecast::new(DataType::Bool));
        assert_eq!(None, tc_bool.type_from_processing_type(DataType::I32));
    }

    #[test]
    fn literal()
    {
        let literal_f32 = Literal::F32(5.0);
        assert_eq!(DataType::F32, literal_f32.data_type());

        let literal_char = Literal::Char('a' as u8);
        assert_eq!(DataType::Char, literal_char.data_type());
    }

    #[test]
    fn expression()
    {
        let expression = Expression::BinaryOp(Box::new(BinaryOp::new(BinaryOpType::Addition,
            Expression::Literal(Literal::I32(5)),
            Expression::UnaryOp(Box::new(UnaryOp::new(UnaryOpType::Typecast(Typecast::new(DataType::I32)),
                Expression::Literal(Literal::F32(10.3))).unwrap()))
            ).unwrap())
        );
        assert_eq!(DataType::I32, expression.data_type());
    }
}