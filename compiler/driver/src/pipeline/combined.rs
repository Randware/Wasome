use crate::pipeline::Pipeline;
use std::marker::PhantomData;

pub(crate) struct Then<
    Input,
    FirstOutput,
    SecondOutput,
    Error,
    First: Pipeline<Input, FirstOutput, Error>,
    Second: Pipeline<FirstOutput, SecondOutput, Error>,
> {
    first: First,
    second: Second,
    _input_phantom: PhantomData<Input>,
    _first_output_phantom: PhantomData<FirstOutput>,
    _second_output_phantom: PhantomData<SecondOutput>,
    _error_phantom: PhantomData<Error>,
}

impl<
    Input,
    FirstOutput,
    SecondOutput,
    Error,
    First: Pipeline<Input, FirstOutput, Error>,
    Second: Pipeline<FirstOutput, SecondOutput, Error>,
> Then<Input, FirstOutput, SecondOutput, Error, First, Second>
{
    pub fn new(first: First, second: Second) -> Self {
        Self {
            first,
            second,
            _input_phantom: PhantomData,
            _first_output_phantom: PhantomData,
            _second_output_phantom: PhantomData,
            _error_phantom: PhantomData,
        }
    }
}

impl<
    Input,
    FirstOutput,
    SecondOutput,
    Error,
    First: Pipeline<Input, FirstOutput, Error>,
    Second: Pipeline<FirstOutput, SecondOutput, Error>,
> Pipeline<Input, SecondOutput, Error>
    for Then<Input, FirstOutput, SecondOutput, Error, First, Second>
{
    fn process(&self, input: Input) -> Result<SecondOutput, Error> {
        self.first
            .process(input)
            .and_then(|first_output| self.second.process(first_output))
    }
}

/// A pipeline as trait object
///
/// Allows to perform type erasure and shorten types
pub(crate) struct Boxed<Input, Output, Error> {
    inner: Box<dyn Pipeline<Input, Output, Error>>,
}

impl<Input, Output, Error> Boxed<Input, Output, Error> {
    pub fn new(inner: impl Pipeline<Input, Output, Error> + 'static) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }
}

impl<Input, Output, Error> Pipeline<Input, Output, Error> for Boxed<Input, Output, Error> {
    fn process(&self, input: Input) -> Result<Output, Error> {
        self.inner.process(input)
    }
}
