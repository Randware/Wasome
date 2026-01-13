use inkwell::context::Context;
use temp::Cli;

mod context;
mod errors;
mod generators;
mod memory;
mod temp;
mod types;

fn main() {
    let config = Cli::mockup();
    context::LLVMContext::new(&Context::create(), config);
}
