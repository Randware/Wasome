use std::marker::PhantomData;
use crate::pipeline::Pipeline;

pub(crate) struct FromInfallibleFunc<Input, Output, Func: Fn(Input) -> Output> {
    func: Func,
    /// Prevent type parameter not used problems
    /// Only required on the input as associated type usage silences the error and
    /// return from Fn is associated type usage
    _input_phantom: PhantomData<Input>
}

impl<Input, Output, Func: Fn(Input) -> Output> FromInfallibleFunc<Input, Output, Func> {
    pub fn new(func: Func) -> Self {
        Self { func, _input_phantom: PhantomData }
    }
}

impl<Input, Output, Error, Func: Fn(Input) -> Output> Pipeline<Input, Output, Error> for FromInfallibleFunc<Input, Output, Func> {
    fn process(&self, input: Input) -> Result<Output, Error> {
        Ok((self.func)(input))
    }
}

pub(crate) struct FromFunc<Input, Output, Error, Func: Fn(Input) -> Result<Output, Error>> {
    func: Func,
    /// Prevent type parameter not used problems
    /// Only required on the input as associated type usage silences the error and
    /// return from Fn is associated type usage
    _input_phantom: PhantomData<Input>
}

impl<Input, Output, Error, Func: Fn(Input) -> Result<Output, Error>> FromFunc<Input, Output, Error, Func> {
    pub fn new(func: Func) -> Self {
        Self { func, _input_phantom: PhantomData }
    }
}

impl<Input, Output, Error, Func: Fn(Input) -> Result<Output, Error>> Pipeline<Input, Output, Error> for FromFunc<Input, Output, Error, Func> {
    fn process(&self, input: Input) -> Result<Output, Error> {
        (self.func)(input)
    }
}