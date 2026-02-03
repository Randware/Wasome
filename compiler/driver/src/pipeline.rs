use crate::pipeline::basic::{FromFunc, FromInfallibleFunc};
use crate::pipeline::combined::{Boxed, Then};

mod basic;
mod combined;

/// Allows for data to be processed
///
/// It turns an input into either an output or an error
///
/// This is used instead of normal functions to allow for better composability with [`Self::then`]
///
/// Pipelines can also be created with [`from_func`] and [`from_func_infallible`]
pub trait Pipeline<Input, Error> {
    type Output;
    fn process(&self, input: Input) -> Result<Self::Output, Error>;

    /// Creates a new pipeline that chains two pipelines together
    ///
    /// The result will put `Input` into `Self` and its output into `then`. The output of this
    /// operation will then be returned
    fn then<SecondOutput, Second: Pipeline<Self::Output, Error, Output = SecondOutput>>(
        self,
        then: Second,
    ) -> Then<Self, Second>
    where
        Self: Sized,
    {
        Then::new(self, then)
    }

    /// Boxes self as a trait object to perform type erasure. This makes code more readable.
    fn boxed(self) -> Boxed<Input, Self::Output, Error>
    where
        Self: 'static + Sized,
    {
        Boxed::new(self)
    }
}

/// Creates a pipeline from a function
pub fn from_func<Input, Output, Error, Func: Fn(Input) -> Result<Output, Error>>(
    func: Func,
) -> FromFunc<Func>
where
{
    FromFunc::new(func)
}

/// Creates a pipeline from a function that can never fail.
pub fn from_infallible_func<Input, Output, Error, Func: Fn(Input) -> Output>(
    func: Func,
) -> FromInfallibleFunc<Func>
where
{
    FromInfallibleFunc::new(func)
}
