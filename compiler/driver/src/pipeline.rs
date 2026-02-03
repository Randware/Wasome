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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_func_success() {
        let pipeline = from_func(|x: i32| -> Result<i32, ()> { Ok(x + 1) });
        assert_eq!(pipeline.process(1), Ok(2));
    }

    #[test]
    fn test_from_func_error() {
        let pipeline = from_func(|_| -> Result<i32, &str> { Err("error") });
        assert_eq!(pipeline.process(1), Err("error"));
    }

    #[test]
    fn test_from_infallible_func() {
        let pipeline = from_infallible_func::<_, _, (), _>(|x: i32| x * 2);
        let result: Result<i32, ()> = pipeline.process(2);
        assert_eq!(result, Ok(4));
    }

    #[test]
    fn test_then_chaining_success() {
        let p1 = from_infallible_func::<_, _, (), _>(|x: i32| x + 1);
        let p2 = from_infallible_func::<_, _, (), _>(|x: i32| x * 2);
        let pipeline = <FromInfallibleFunc<_> as Pipeline<i32, ()>>::then::<
            i32,
            FromInfallibleFunc<_>,
        >(p1, p2);

        let result: Result<i32, ()> = pipeline.process(1);
        // (1 + 1) * 2 = 4
        assert_eq!(result, Ok(4));
    }

    #[test]
    fn test_then_chaining_first_fail() {
        let p1 = from_func(|_| -> Result<i32, &str> { Err("fail first") });
        let p2 = from_infallible_func::<_, _, String, _>(|x: i32| x * 2);
        let pipeline = p1.then(p2);

        assert_eq!(pipeline.process(1), Err("fail first"));
    }

    #[test]
    fn test_then_chaining_second_fail() {
        let p1 = from_infallible_func::<_, _, String, _>(|x: i32| x + 1);
        let p2 = from_func(|_| -> Result<i32, &str> { Err("fail second") });
        let pipeline = p1.then(p2);

        // First succeeds (1+1=2), second fails
        assert_eq!(pipeline.process(1), Err("fail second"));
    }

    #[test]
    fn test_boxed() {
        let pipeline = from_infallible_func::<_, _, (), _>(|x: i32| x + 1).boxed();
        let result: Result<i32, ()> = pipeline.process(10);
        assert_eq!(result, Ok(11));
    }
}
