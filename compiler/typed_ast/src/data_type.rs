/** This represents the data types specified in section one of the lang spec
*/
#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq)]
pub enum DataType
{
    Char,
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    Bool,
    F32,
    F64
}

/** This represents some wasome concept with a data type
*/
pub trait Type
{
    /** This method gets the type
    */
    fn data_type(&self) -> DataType;
}