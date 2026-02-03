use crate::pipeline::basic::{FromFunc, FromInfallibleFunc};
use crate::pipeline::combined::{Boxed, Then};

mod basic;
mod combined;

pub trait Pipeline<Input, Error> {
    type Output;
    fn process(&self, input: Input) -> Result<Self::Output, Error>;

    fn then<SecondOutput, Second: Pipeline<Self::Output, Error, Output = SecondOutput>>(
        self,
        then: Second,
    ) -> Then<Self, Second>
    where
        Self: Sized,
    {
        Then::new(self, then)
    }

    fn boxed(self) -> Boxed<Input, Self::Output, Error>
    where
        Self: 'static + Sized,
    {
        Boxed::new(self)
    }
}

pub fn from_func<Input, Output, Error, Func: Fn(Input) -> Result<Output, Error>>(
    func: Func,
) -> FromFunc<Func>
where
{
    FromFunc::new(func)
}

pub fn from_infallible_func<Input, Output, Error, Func: Fn(Input) -> Output>(
    func: Func,
) -> FromInfallibleFunc<Func>
where
{
    FromInfallibleFunc::new(func)
}
