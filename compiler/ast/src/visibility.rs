/// The visibility of a syntax element
///
/// This is used for both typed and untyped ASTs as we need to do importing on the untyped AST
/// and using strings there would make that harder
#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum Visibility {
    /// Only visible inside the same file
    Private,
    /// Visible in the entire application
    Public,
}

/// Types that have a specific visibility
pub trait Visible {
    fn visibility(&self) -> Visibility;
}
