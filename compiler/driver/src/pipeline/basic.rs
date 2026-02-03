use crate::pipeline::Pipeline;

pub struct FromInfallibleFunc<Func> {
    func: Func,
}

impl<Func> FromInfallibleFunc<Func> {
    pub const fn new(func: Func) -> Self {
        Self { func }
    }
}

impl<Input, Output, Error, Func> Pipeline<Input, Error> for FromInfallibleFunc<Func>
where
    Func: Fn(Input) -> Output,
{
    type Output = Output;
    fn process(&self, input: Input) -> Result<Output, Error> {
        Ok((self.func)(input))
    }
}

pub struct FromFunc<Func> {
    func: Func,
}

impl<Func> FromFunc<Func> {
    pub const fn new(func: Func) -> Self {
        Self { func }
    }
}

impl<Input, Output, Error, Func> Pipeline<Input, Error> for FromFunc<Func>
where
    Func: Fn(Input) -> Result<Output, Error>,
{
    type Output = Output;
    fn process(&self, input: Input) -> Result<Output, Error> {
        (self.func)(input)
    }
}
