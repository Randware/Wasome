#[cfg(feature = "runtime")]
mod runtime;
#[cfg(feature = "runtime")]
pub use runtime::*;

#[cfg(feature = "web")]
mod web;
#[cfg(feature = "web")]
pub use web::*;

#[cfg(all(feature = "runtime", feature = "web"))]
compile_error!("Can't both target the runtime and web at the same time!");