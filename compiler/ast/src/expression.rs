use crate::data_type::{DataType, Typed};
use crate::symbol::FunctionCall;
use crate::{ASTType, TypedAST, UntypedAST, eq_return_option};

/** This represents an expression as per section 2 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Expression<Type: ASTType> {
    // Only valid if it doesn't return void
    FunctionCall(FunctionCall<Type>),
    Variable(Type::VariableUse),
    Literal(Type::LiteralType),
    UnaryOp(Box<UnaryOp<Type>>), // The boxes prevent this from becoming an infinitely sized type
    BinaryOp(Box<BinaryOp<Type>>),
}

impl Typed for Expression<TypedAST> {
    fn data_type(&self) -> DataType {
        use Expression as Ex;
        match self {
            // Unwrap safety:
            // It may only exist if the return type is not void
            Ex::FunctionCall(inner) => *inner.function().return_type().unwrap(),
            Ex::Literal(inner) => inner.data_type(),
            Ex::UnaryOp(inner) => inner.data_type(),
            Ex::BinaryOp(inner) => inner.data_type(),
            Ex::Variable(inner) => *inner.data_type(),
        }
    }
}

/** This represents a literal as described at the end of section 2 of the lang spec
*/
#[derive(Debug, PartialEq)]
pub enum Literal {
    // All integer literals are of type S32
    S32(i32),
    Bool(bool),
    Char(u32), //UTF-8 character
    //F32(f32),
    F64(f64),
}

impl Literal {
    pub fn data_type(&self) -> DataType {
        use DataType as DT;
        match self {
            Literal::S32(_) => DT::S32,
            Literal::Bool(_) => DT::Bool,
            Literal::Char(_) => DT::Char,
            //Literal::F32(_) => DT::F32,
            Literal::F64(_) => DT::F64,
        }
    }
}

/** This is a type of operator that only takes one input
*/
#[derive(Debug, PartialEq)]
pub struct UnaryOp<Type: ASTType> {
    // The type of the expression
    op_type: UnaryOpType<Type>,
    // The expression to "process"
    input: Expression<Type>,
}

impl<Type: ASTType> UnaryOp<Type> {
    pub fn input(&self) -> &Expression<Type> {
        &self.input
    }

    pub fn op_type(&self) -> &UnaryOpType<Type> {
        &self.op_type
    }

    pub fn destructure(&self) -> (&UnaryOpType<Type>, &Expression<Type>) {
        (&self.op_type, &self.input)
    }
}

impl UnaryOp<TypedAST> {
    /** Creates a new instance of UnaryOp
    @params
    op_type: The type of this expression
    input: The expression to base this on
    @return
    Some(output data type) if the provided type can be processed to the output type
    None if the processed type can't be processed
    */
    pub fn new(op_type: UnaryOpType<TypedAST>, input: Expression<TypedAST>) -> Option<Self> {
        // Can't process
        op_type.result_type(input.data_type())?;
        Some(Self { op_type, input })
    }
}

impl UnaryOp<UntypedAST> {
    /** Creates a new instance of UnaryOp
       @params
       op_type: The type of this expression
       input: The expression to base this on
    */
    pub fn new(op_type: UnaryOpType<UntypedAST>, input: Expression<UntypedAST>) -> Self {
        Self { op_type, input }
    }
}

impl Typed for UnaryOp<TypedAST> {
    fn data_type(&self) -> DataType {
        self.op_type.result_type(self.input.data_type()).unwrap() // Unwrap safety: UnaryOps may only exist if the input type can be processed
        // This is checked in the constructor
        // Therefore, this can never panic
    }
}

/** This is the type of an unary operator
*/
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnaryOpType<Type: ASTType> {
    /// Section 3 lang spec, subtract operator with only one operand
    Negative,
    /// Section 3 lang spec, not operator
    Not,
    /// Section 11 lang spec
    Typecast(Typecast<Type>),
}

impl UnaryOpType<TypedAST> {
    /** Gets the type from processing the provided type
    @params
    to_process: The provided data type
    @return
    Some(output data type) if the provided type can be processed to the output type
    None if the processed type can't be processed
    */
    pub fn result_type(&self, to_process: DataType) -> Option<DataType> {
        match self {
            UnaryOpType::Negative => Self::minus_type(to_process),
            UnaryOpType::Not => Self::neg_type(to_process),
            UnaryOpType::Typecast(inner) => inner.result_type(to_process),
        }
    }

    /** Returns the type of data from putting the input type through a negation operator
       (lang spec, section 3)
    */
    fn neg_type(to_process: DataType) -> Option<DataType> {
        match to_process {
            DataType::Bool => Some(DataType::Bool),
            _ => None,
        }
    }

    /** Returns the type of data from putting the input type through a minus operator
          (lang spec, section 3)
    */
    fn minus_type(to_process: DataType) -> Option<DataType> {
        match to_process {
            DataType::Char
            | DataType::Bool
            | DataType::U8
            | DataType::U16
            | DataType::U32
            | DataType::U64 => None,
            _ => Some(to_process),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
/** Represents a typecast that casts the provided type to target
*/
pub struct Typecast<Type: ASTType> {
    target: Type::GeneralDataType,
}

impl<Type: ASTType> Typecast<Type> {
    pub fn target(&self) -> &Type::GeneralDataType {
        &self.target
    }
}

impl<Type: ASTType> Typecast<Type> {
    /** Creates a new Typecast
     */
    pub fn new(target: Type::GeneralDataType) -> Self {
        Self { target }
    }
}

impl Typecast<TypedAST> {
    /** Returns what a specific type typecasted with self would return
       @Param
       to_process: The Type to process
       @Return
       Some(result) if to_process can be casted to result
       None if there is no cast available
    */
    pub fn result_type(&self, to_process: DataType) -> Option<DataType> {
        match (to_process, self.target) {
            (DataType::S8 | DataType::U16, DataType::U8)
            | (DataType::U8 | DataType::S16, DataType::S8)
            | (DataType::S16 | DataType::U32 | DataType::U8, DataType::U16)
            | (DataType::U16 | DataType::S32 | DataType::S8, DataType::S16)
            | (DataType::S32 | DataType::U64 | DataType::U16, DataType::U32)
            | (DataType::U32 | DataType::S64 | DataType::S16 | DataType::F32, DataType::S32)
            | (DataType::S64 | DataType::U32, DataType::U64)
            | (DataType::U64 | DataType::S32 | DataType::F64, DataType::S64)
            | (DataType::S32 | DataType::F64, DataType::F32)
            | (DataType::S64 | DataType::F32, DataType::F64) => Some(self.target),
            _ => None,
        }
    }
}

/** This is a type of operator that takes two inputs
*/
#[derive(Debug, PartialEq)]
pub struct BinaryOp<Type: ASTType> {
    // The type of the expression
    op_type: BinaryOpType,
    // The left expression to process
    left: Expression<Type>,
    // The left expression to process
    right: Expression<Type>,
}

impl<Type: ASTType> BinaryOp<Type> {
    pub fn left(&self) -> &Expression<Type> {
        &self.left
    }

    pub fn right(&self) -> &Expression<Type> {
        &self.right
    }

    pub fn op_type(&self) -> &BinaryOpType {
        &self.op_type
    }

    pub fn destructure(&self) -> (BinaryOpType, &Expression<Type>, &Expression<Type>) {
        (self.op_type, &self.left, &self.right)
    }
}

impl BinaryOp<TypedAST> {
    /** Creates a new instance of UnaryOp
          @params
          op_type: The type of this expression
          input: The expression to base this on
          @return
          Some(output data type) if the provided type can be processed to the output type
          None if the processed type can't be processed
    */
    pub fn new(
        op_type: BinaryOpType,
        left: Expression<TypedAST>,
        right: Expression<TypedAST>,
    ) -> Option<Self> {
        // Can't process
        op_type.result_type(left.data_type(), right.data_type())?;
        Some(Self {
            op_type,
            left,
            right,
        })
    }
}

impl BinaryOp<UntypedAST> {
    /** Creates a new instance of UnaryOp
             @params
             op_type: The type of this expression
             input: The expression to base this on
    */
    pub fn new(
        op_type: BinaryOpType,
        left: Expression<UntypedAST>,
        right: Expression<UntypedAST>,
    ) -> Self {
        Self {
            op_type,
            left,
            right,
        }
    }
}

impl Typed for BinaryOp<TypedAST> {
    fn data_type(&self) -> DataType {
        self.op_type
            .result_type(self.left.data_type(), self.right.data_type())
            .unwrap() // Unwrap safety: BinaryOps may only exist if the input type can be processed
        // This is checked in the constructor
        // Therefore, this can never panic
    }
}

/** This is the type of an binary operator
*/
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOpType {
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
    LesserEquals,
}

impl BinaryOpType {
    /** Gets the type from processing the provided type
       @params
       left, right: The provided data types
       @return
       Some(output data type) if the provided types can be processed to the output type
       None if the processed type can't be processed
    */
    pub fn result_type(&self, left: DataType, right: DataType) -> Option<DataType> {
        use BinaryOpType as BOT;
        match self {
            BOT::Addition | BOT::Subtraction | BOT::Multiplication | BOT::Division => {
                Self::arithmetic_type(left, right)
            }
            BOT::Modulo
            | BOT::LeftShift
            | BOT::RightShift
            | BOT::BitwiseOr
            | BOT::BitwiseAnd
            | BOT::BitwiseXor => Self::int_op_type(left, right),
            BOT::Or | BOT::And | BOT::Xor => Self::bool_op_type(left, right),
            BOT::Equals | BOT::NotEquals => {
                eq_return_option(left, right)?;
                Some(DataType::Bool)
            }
            BOT::Greater | BOT::GreaterEquals | BOT::Lesser | BOT::LesserEquals => {
                Self::comparison_op_type(left, right)
            }
        }
    }

    /** Returns the type of data from putting the two input types through an arethmetic operator
    (lang spec, section 3)
    */
    fn arithmetic_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        match left {
            DataType::U8
            | DataType::S8
            | DataType::U16
            | DataType::S16
            | DataType::U32
            | DataType::S32
            | DataType::U64
            | DataType::S64
            | DataType::F32
            | DataType::F64 => Some(left),
            _ => None,
        }
    }

    /** Returns the type of data from putting the two input types through an integer only operator
    (lang spec, section 3)
    */
    fn int_op_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        match left {
            DataType::U8
            | DataType::S8
            | DataType::U16
            | DataType::S16
            | DataType::U32
            | DataType::S32
            | DataType::U64
            | DataType::S64 => Some(left),
            _ => None,
        }
    }

    /** Returns the type of data from putting the two input types through a bool only (such as or) operator
       (lang spec, section 3)
    */
    fn bool_op_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        match left {
            DataType::Bool => Some(left),
            _ => None,
        }
    }

    /** Returns the type of data from putting the two input types through a comparison operator
       (lang spec, section 3)
    */
    fn comparison_op_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        if left == DataType::Bool || left == DataType::Char {
            return None;
        }
        Some(DataType::Bool)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn binary_op_type() {
        let add = BinaryOpType::Addition;
        assert_eq!(None, add.result_type(DataType::F32, DataType::F64));
        assert_eq!(
            Some(DataType::F32),
            add.result_type(DataType::F32, DataType::F32)
        );
        assert_eq!(None, add.result_type(DataType::Bool, DataType::Bool));

        let bxor = BinaryOpType::BitwiseXor;
        assert_eq!(None, bxor.result_type(DataType::F32, DataType::F32));
        assert_eq!(
            Some(DataType::S64),
            bxor.result_type(DataType::S64, DataType::S64)
        );
    }

    #[test]
    fn unary_op_type() {
        let negative = UnaryOpType::Negative;
        assert_eq!(Some(DataType::F32), negative.result_type(DataType::F32));
        assert_eq!(None, negative.result_type(DataType::Bool));

        let not = UnaryOpType::Not;
        assert_eq!(Some(DataType::Bool), not.result_type(DataType::Bool));
        assert_eq!(None, not.result_type(DataType::Char));

        let tc_i32 = UnaryOpType::Typecast(Typecast::new(DataType::S32));
        assert_eq!(Some(DataType::S32), tc_i32.result_type(DataType::F32));
        assert_eq!(Some(DataType::S32), tc_i32.result_type(DataType::S64));
        assert_eq!(None, tc_i32.result_type(DataType::Bool));

        let tc_bool = UnaryOpType::Typecast(Typecast::new(DataType::Bool));
        assert_eq!(None, tc_bool.result_type(DataType::S32));
    }

    #[test]
    fn literal() {
        let literal_f64 = Literal::F64(5.0);
        assert_eq!(DataType::F64, literal_f64.data_type());

        let literal_char = Literal::Char('a' as u32);
        assert_eq!(DataType::Char, literal_char.data_type());
    }

    #[test]
    fn typecast() {
        let typecast_s32 = Typecast::new(DataType::S32);
        assert_eq!(Some(DataType::S32), typecast_s32.result_type(DataType::F32));
        assert_eq!(Some(DataType::S32), typecast_s32.result_type(DataType::S16));
        assert_eq!(None, typecast_s32.result_type(DataType::S32));

        let typecast_u16 = Typecast::new(DataType::U16);
        assert_eq!(Some(DataType::U16), typecast_u16.result_type(DataType::U32));
        assert_eq!(Some(DataType::U16), typecast_u16.result_type(DataType::S16));
        assert_eq!(None, typecast_u16.result_type(DataType::F32));

        let typecast_s64 = Typecast::new(DataType::S64);
        assert_eq!(Some(DataType::S64), typecast_s64.result_type(DataType::F64));
        assert_eq!(Some(DataType::S64), typecast_s64.result_type(DataType::S32));
        assert_eq!(None, typecast_s64.result_type(DataType::F32));
    }

    #[test]
    fn expression() {
        let expression = Expression::BinaryOp(Box::new(
            BinaryOp::<TypedAST>::new(
                BinaryOpType::Addition,
                Expression::UnaryOp(Box::new(
                    UnaryOp::<TypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(DataType::S64)),
                        Expression::Literal(Literal::S32(5)),
                    )
                    .unwrap(),
                )),
                Expression::UnaryOp(Box::new(
                    UnaryOp::<TypedAST>::new(
                        UnaryOpType::Typecast(Typecast::new(DataType::S64)),
                        Expression::Literal(Literal::F64(10.3)),
                    )
                    .unwrap(),
                )),
            )
            .unwrap(),
        ));
        assert_eq!(DataType::S64, expression.data_type());
    }
}
