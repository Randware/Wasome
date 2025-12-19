use crate::data_type::{DataType, Typed};
use crate::symbol::FunctionSymbol;
use crate::{ASTNode, ASTType, SemanticEquality, TypedAST, UntypedAST, eq_return_option};
use std::rc::Rc;

/// This represents an expression as per section 2 of the lang spec
///
/// # Equality
///
/// Two different Expressions are never equal.
/// Use semantic_equals from [`SemanticEquality`] to check semantics only
#[derive(Debug, PartialEq)]
pub enum Expression<Type: ASTType> {
    /// This is only valid if there is a return value and not void
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

impl<Type: ASTType> SemanticEquality for Expression<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        use Expression as Exp;
        match (self, other) {
            (Exp::FunctionCall(inner), Exp::FunctionCall(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            // For Variables, we want to ensure that the same symbols are used
            // For Literals, there are no inner structs with ids
            (Exp::Variable(_), Exp::Variable(_)) | (Exp::Literal(_), Exp::Literal(_)) => {
                self == other
            }
            (Exp::UnaryOp(inner), Exp::UnaryOp(other_inner)) => inner.semantic_equals(other_inner),
            (Exp::BinaryOp(inner), Exp::BinaryOp(other_inner)) => {
                inner.semantic_equals(other_inner)
            }
            _ => false,
        }
    }
}

/// A literal
/// The value of the literal is contained within this enum
#[derive(Debug, PartialEq)]
pub enum Literal {
    /// All integer literals are of type S32
    S32(i32),
    Bool(bool),
    /// A UTF-8 character
    Char(u32),
    F32(f32),
    F64(f64),
}

impl Literal {
    pub fn data_type(&self) -> DataType {
        use DataType as DT;
        match self {
            Literal::S32(_) => DT::S32,
            Literal::Bool(_) => DT::Bool,
            Literal::Char(_) => DT::Char,
            Literal::F32(_) => DT::F32,
            Literal::F64(_) => DT::F64,
        }
    }
}

/// This is a type of operator that only takes one input
///
/// # Validity
///
/// A typed UnaryOp is only valid if the type of the result of the expression can be processed by
/// the provided operator
///
/// # Equality
///
/// Two different UnaryOps are never equal.
/// Use semantic_equals from [`SemanticEquality`] to check semantics only
#[derive(Debug, PartialEq)]
pub struct UnaryOp<Type: ASTType> {
    // The type of the expression
    op_type: UnaryOpType<Type>,
    // The expression to "process"
    input: ASTNode<Expression<Type>>,
}

impl UnaryOp<TypedAST> {
    /// Creates a new instance of UnaryOp
    ///
    /// # Parameters:
    ///
    /// - op_type
    ///     - The type of this expression
    /// - input
    ///     - The expression to base this on
    ///
    /// # Return:
    ///
    /// - Some(unaryOp) if the provided type can be processed to the output type
    /// - None if the processed type can't be processed
    ///
    pub fn new(
        op_type: UnaryOpType<TypedAST>,
        input: ASTNode<Expression<TypedAST>>,
    ) -> Option<Self> {
        // Can't process
        op_type.result_type(input.data_type())?;
        Some(Self { op_type, input })
    }
}

impl<Type: ASTType> SemanticEquality for UnaryOp<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.op_type == other.op_type && self.input.semantic_equals(&other.input)
    }
}

impl UnaryOp<UntypedAST> {
    /// Creates a new instance of UnaryOp
    ///
    /// # Parameters
    ///
    /// - op_type
    ///     - The type of this expression
    /// - input
    ///     - The expression to base this on
    pub fn new(op_type: UnaryOpType<UntypedAST>, input: ASTNode<Expression<UntypedAST>>) -> Self {
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

/// The type of a unary operator
///
/// It only provides the operator type and not the operands
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
    /// Gets the type from processing the provided type
    ///
    /// # Parameter
    ///
    /// - to_process: The provided data type
    ///
    /// # Return
    ///
    /// - Some(output data type) if the provided type can be processed to the output type
    /// - None if the processed type can't be processed
    pub fn result_type(&self, to_process: DataType) -> Option<DataType> {
        match self {
            UnaryOpType::Negative => Self::minus_type(to_process),
            UnaryOpType::Not => Self::neg_type(to_process),
            UnaryOpType::Typecast(inner) => inner.result_type(to_process),
        }
    }

    /// Returns the type of data from putting the input type through a negation operator
    /// (lang spec, section 3)
    fn neg_type(to_process: DataType) -> Option<DataType> {
        match to_process {
            DataType::Bool => Some(DataType::Bool),
            _ => None,
        }
    }

    /// Returns the type of data from putting the input type through a minus operator
    /// (lang spec, section 3)
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

/// A typecast that changes data type of the result of an expression
///
/// Note that this only stores the target type and requires the expression to be supplied
/// externally.
///
/// # Type conversions
///
/// Not all type conversions are valid.
/// - Consult the lang spec for more information
/// - Typecast however can always exist as it only stores the target data tyoe
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Typecast<Type: ASTType> {
    target: Type::GeneralDataType,
}

impl<Type: ASTType> Typecast<Type> {
    /// Creates a new Typecast
    pub fn new(target: Type::GeneralDataType) -> Self {
        Self { target }
    }
}

impl Typecast<TypedAST> {
    /// Returns what a specific type typecasted with self would return
    ///
    /// # Parameters
    ///
    /// - to_process
    ///     - The Type to process
    ///
    /// # Return
    ///
    /// - Some(result) if to_process can be casted to result
    /// - None if there is no cast available
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

/// This is an operator that takes two inputs
///
/// # Equality
///
/// Two different BinaryOps are never equal.
/// Use semantic_equals from [`SemanticEquality`] to check semantics only
#[derive(Debug, PartialEq)]
pub struct BinaryOp<Type: ASTType> {
    // The type of the expression
    op_type: BinaryOpType,
    // The left expression to process
    left: ASTNode<Expression<Type>>,
    // The left expression to process
    right: ASTNode<Expression<Type>>,
}

impl<Type: ASTType> SemanticEquality for BinaryOp<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.op_type == other.op_type
            && self.left.semantic_equals(&other.left)
            && self.right.semantic_equals(&other.right)
    }
}

impl BinaryOp<TypedAST> {
    /// Creates a new instance of UnaryOp
    ///
    /// # Parameters
    ///
    /// - op_type
    ///   -The type of this expression
    /// - input
    ///   - The expression to base this on
    ///
    /// # Return
    ///
    /// - Some(output data type) if the provided type can be processed to the output type
    /// - None if the processed type can't be processed
    pub fn new(
        op_type: BinaryOpType,
        left: ASTNode<Expression<TypedAST>>,
        right: ASTNode<Expression<TypedAST>>,
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
    /// Creates a new instance of UnaryOp
    ///
    /// # Parameters
    ///
    /// - op_type
    ///     - The type of this expression
    /// - input
    ///     - The expression to base this on
    pub fn new(
        op_type: BinaryOpType,
        left: ASTNode<Expression<UntypedAST>>,
        right: ASTNode<Expression<UntypedAST>>,
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

/// This is the type of an unary operator
///
/// It only provides the operator type and not the operands
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
    /// Gets the type from processing the provided type
    ///   
    /// # Parameters
    ///   
    /// - left, right:
    ///     - The provided data types
    ///   
    /// # Return
    ///
    /// - Some(output data type) if the provided types can be processed to the output type
    /// - None if the processed type can't be processed
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

    /// Returns the type of data from putting the two input types through an arethmetic operator
    /// (lang spec, section 3)
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

    /// Returns the type of data from putting the two input types through an integer only operator
    /// (lang spec, section 3)
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

    /// Returns the type of data from putting the two input types through a bool only (such as or) operator
    /// (lang spec, section 3)
    fn bool_op_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        match left {
            DataType::Bool => Some(left),
            _ => None,
        }
    }

    /// Returns the type of data from putting the two input types through a comparison operator
    /// (lang spec, section 3)
    fn comparison_op_type(left: DataType, right: DataType) -> Option<DataType> {
        eq_return_option(left, right)?;
        if left == DataType::Bool || left == DataType::Char {
            return None;
        }
        Some(DataType::Bool)
    }
}

/// A function call
///
/// This is only a part of an expression if it has a return value
/// - Otherwise, it is a statement part
#[derive(Debug, PartialEq)]
pub struct FunctionCall<Type: ASTType> {
    function: Type::FunctionCallSymbol,
    args: Vec<ASTNode<Expression<Type>>>,
}

impl<Type: ASTType> FunctionCall<Type> {
    pub fn function(&self) -> &Type::FunctionCallSymbol {
        &self.function
    }

    pub fn args(&self) -> &Vec<ASTNode<Expression<Type>>> {
        &self.args
    }
}

impl<Type: ASTType> SemanticEquality for FunctionCall<Type> {
    fn semantic_equals(&self, other: &Self) -> bool {
        self.function == other.function && self.args.semantic_equals(&other.args)
    }
}

impl FunctionCall<TypedAST> {
    /** Creates a new function call
       Checks if the provided and expected params are the same number and have the same data types
       Returns None if these checks failed
       Some(new instance) otherwise
    */
    pub fn new(
        function: Rc<FunctionSymbol<TypedAST>>,
        args: Vec<ASTNode<Expression<TypedAST>>>,
    ) -> Option<Self> {
        if function.params().len() != args.len()
            || !function
                .params()
                .iter()
                .zip(args.iter())
                .all(|(expected, provided)| *expected.data_type() == provided.data_type())
        {
            return None;
        }
        Some(Self { function, args })
    }
}

impl FunctionCall<UntypedAST> {
    /** Creates a new function call
     */
    pub fn new(function: String, args: Vec<ASTNode<Expression<UntypedAST>>>) -> Self {
        Self { function, args }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::VariableSymbol;
    use crate::test_shared::sample_codearea;
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
        let literal_f32 = Literal::F32(5.0);
        assert_eq!(DataType::F32, literal_f32.data_type());

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
        let expression = generate_sample_expression();
        assert_eq!(DataType::S32, expression.data_type());
    }

    #[test]
    fn two_expression_not_equal() {
        // Two expressions have different ids and are not equal
        let expression_1 = generate_sample_expression();
        let expression_2 = generate_sample_expression();
        assert_ne!(expression_1, expression_2)
    }

    #[test]
    fn two_expression_semantically_equal() {
        // Two expressions have different ids and are not equal
        let expression_1 = generate_sample_expression();
        let expression_2 = generate_sample_expression();
        assert!(expression_1.semantic_equals(&expression_2))
    }

    fn generate_sample_expression() -> Expression<TypedAST> {
        Expression::BinaryOp(Box::new(
            BinaryOp::<TypedAST>::new(
                BinaryOpType::Addition,
                ASTNode::new(Expression::Literal(Literal::S32(5)), sample_codearea()),
                ASTNode::new(
                    Expression::UnaryOp(Box::new(
                        UnaryOp::<TypedAST>::new(
                            UnaryOpType::Typecast(Typecast::new(DataType::S32)),
                            ASTNode::new(
                                Expression::Literal(Literal::F32(10.3)),
                                sample_codearea(),
                            ),
                        )
                        .unwrap(),
                    )),
                    sample_codearea(),
                ),
            )
            .unwrap(),
        ))
    }

    #[test]
    fn create_function_call_untyped() {
        let name = "test".to_string();
        let arg = ASTNode::new(
            Expression::<UntypedAST>::Literal("10".to_string()),
            sample_codearea(),
        );
        let call = FunctionCall::<UntypedAST>::new(name, vec![arg]);
        assert_eq!("test", call.function());
        assert_eq!(1, call.args().len());
    }

    #[test]
    fn create_function_call_typed_wrong_args() {
        let symbol = Rc::new(FunctionSymbol::new(
            "test".to_string(),
            None,
            vec![Rc::new(VariableSymbol::new(
                "test1".to_string(),
                DataType::Bool,
            ))],
        ));
        let arg = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::S32(10)),
            sample_codearea(),
        );
        let call = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg]);
        assert_eq!(None, call);

        let call_empty = FunctionCall::<TypedAST>::new(symbol, Vec::new());
        assert_eq!(None, call_empty)
    }

    #[test]
    fn create_function_call_typed() {
        let symbol = Rc::new(FunctionSymbol::new(
            "test".to_string(),
            None,
            vec![Rc::new(VariableSymbol::new(
                "test1".to_string(),
                DataType::Bool,
            ))],
        ));
        let arg = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::Bool(true)),
            sample_codearea(),
        );
        let call = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg]);
        assert_eq!(None, call.as_ref().unwrap().function().return_type());
        assert_eq!("test", call.as_ref().unwrap().function().name());

        let arg2 = ASTNode::new(
            Expression::<TypedAST>::Literal(Literal::Bool(true)),
            sample_codearea(),
        );
        let call2 = FunctionCall::<TypedAST>::new(symbol.clone(), vec![arg2]);
        assert!(call.semantic_equals(&call2));
    }
}
