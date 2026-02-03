use crate::pipeline::Pipeline;

pub struct Then<First, Second> {
    first: First,
    second: Second,
}

impl<First, Second> Then<First, Second> {
    pub const fn new(first: First, second: Second) -> Self {
        Self { first, second }
    }
}

impl<Input, FirstOutput, SecondOutput, Error, First, Second> Pipeline<Input, Error>
    for Then<First, Second>
where
    First: Pipeline<Input, Error, Output = FirstOutput>,
    Second: Pipeline<First::Output, Error, Output = SecondOutput>,
{
    type Output = SecondOutput;
    fn process(&self, input: Input) -> Result<SecondOutput, Error> {
        self.first
            .process(input)
            .and_then(|first_output| self.second.process(first_output))
    }
}

/// A pipeline as trait object
///
/// Allows to perform type erasure and shorten types
pub struct Boxed<Input, Output, Error> {
    inner: Box<dyn Pipeline<Input, Error, Output = Output>>,
}

impl<Input, Output, Error> Boxed<Input, Output, Error> {
    pub fn new(inner: impl Pipeline<Input, Error, Output = Output> + 'static) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }
}

impl<Input, Output, Error> Pipeline<Input, Error> for Boxed<Input, Output, Error> {
    type Output = Output;
    fn process(&self, input: Input) -> Result<Output, Error> {
        self.inner.process(input)
    }
}
