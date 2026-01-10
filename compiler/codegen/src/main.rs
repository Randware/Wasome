use inkwell::context::Context;
use temp::Cli;

mod context;
mod generators;
mod memory;
mod temp;

fn main() {
    let config = Cli::mockup();
    context::LLVMContext::new(&Context::create(), config);
}
