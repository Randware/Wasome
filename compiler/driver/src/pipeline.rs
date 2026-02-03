use crate::pipeline::basic::{FromFunc, FromInfallibleFunc};
use crate::pipeline::combined::{Boxed, Then};

mod basic;
mod combined;

pub(crate) trait Pipeline<Input, Output, Error> {
    fn process(&self, input: Input) -> Result<Output, Error>;

    fn from_func<Func: Fn(Input) -> Result<Output, Error>>(
        func: Func,
    ) -> FromFunc<Input, Output, Error, Func>
    where
        // Retain dyn compatibility of the trait
        Self: Sized,
    {
        FromFunc::new(func)
    }

    fn from_infallible_func<Func: Fn(Input) -> Output>(
        func: Func,
    ) -> FromInfallibleFunc<Input, Output, Func>
    where
    // Retain dyn compatibility of the trait
        Self: Sized,
    {
        FromInfallibleFunc::new(func)
    }

    fn then<SecondOutput, Second: Pipeline<Output, SecondOutput, Error>>(
        self,
        then: Second,
    ) -> Then<Input, Output, SecondOutput, Error, Self, Second>
    where
        Self: Sized,
    {
        Then::new(self, then)
    }

    fn boxed(self) -> Boxed<Input, Output, Error>
    where
        Self: 'static + Sized,
    {
        Boxed::new(self)
    }
}
