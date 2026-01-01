use ast::data_type::DataType;

/// A helper function that resolves the type names into the right types.
///
/// # Parameters
/// * `to_analyze` - The string representation of the data type (e.g., "s32", "bool").
///
/// # Returns
/// * `Some(DataType)` if the string matches a known type.
/// * `None` otherwise.
pub(crate) fn analyze_data_type(to_analyze: &str) -> Option<DataType> {
    match to_analyze {
        "char" => Some(DataType::Char),
        "u8" => Some(DataType::U8),
        "s8" => Some(DataType::S8),
        "u16" => Some(DataType::U16),
        "s16" => Some(DataType::S16),
        "u32" => Some(DataType::U32),
        "s32" => Some(DataType::S32),
        "u64" => Some(DataType::U64),
        "s64" => Some(DataType::S64),
        "bool" => Some(DataType::Bool),
        "f32" => Some(DataType::F32),
        "f64" => Some(DataType::F64),
        _ => None,
    }
}
