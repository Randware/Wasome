#[cfg(feature = "runtime")]
use crate::command::RunArgs;
#[cfg(feature = "runtime")]
use crate::error::CliResult;

#[cfg(feature = "runtime")]
pub fn run(args: &RunArgs, wasm_bytes: &[u8]) -> CliResult<()> {
    use error::diagnostic::{Diagnostic, Level};
    use wasmtime::{Config, Engine, Linker, Module, Store};
    use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder};

    struct WasiState {
        ctx: wasmtime_wasi::p1::WasiP1Ctx,
    }

    let mut config = Config::new();
    config.wasm_component_model(false);
    let engine = Engine::new(&config).map_err(|e| {
        let _ = Diagnostic::builder()
            .level(Level::Error)
            .message(format!("Failed to initialize Wasmtime engine: {}", e))
            .build()
            .print();
        crate::error::CliError::CompilationFailed
    })?;

    let mut linker = Linker::<WasiState>::new(&engine);

    // Use wasi preview 1
    wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |s| &mut s.ctx).map_err(|e| {
        let _ = Diagnostic::builder()
            .level(Level::Error)
            .message(format!("Failed to add WASI to linker: {}", e))
            .build()
            .print();
        crate::error::CliError::CompilationFailed
    })?;

    let module = Module::new(&engine, wasm_bytes).map_err(|e| {
        let _ = Diagnostic::builder()
            .level(Level::Error)
            .message(format!("Failed to load wasm module: {}", e))
            .build()
            .print();
        crate::error::CliError::CompilationFailed
    })?;

    let mut builder = WasiCtxBuilder::new();
    
    if !args.no_stdio {
        builder.inherit_stdio();
    }
    
    if args.net {
        builder.inherit_network();
        builder.allow_ip_name_lookup(true);
    }

    for arg in &args.args {
        builder.arg(arg);
    }

    for env in &args.env {
        if let Some((k, v)) = env.split_once('=') {
            builder.env(k, v);
        } else {
            let _ = Diagnostic::builder()
                .level(Level::Warning)
                .message(format!(
                    "Invalid environment variable format: '{}', expected KEY=VALUE",
                    env
                ))
                .build()
                .print();
        }
    }

    for dir in &args.dir {
        if let Err(e) = builder.preopened_dir(dir, dir, DirPerms::all(), FilePerms::all()) {
            let _ = Diagnostic::builder()
                .level(Level::Warning)
                .message(format!("Failed to preopen directory {}: {}", dir, e))
                .build()
                .print();
        }
    }

    // Default permission: access to current directory
    if !args.no_cwd {
        let _ = builder.preopened_dir(".", ".", DirPerms::all(), FilePerms::all());
    }

    let mut store = Store::new(
        &engine,
        WasiState {
            ctx: builder.build_p1(),
        },
    );

    let instance = linker.instantiate(&mut store, &module).map_err(|e| {
        let _ = Diagnostic::builder()
            .level(Level::Error)
            .message(format!("Failed to instantiate module: {}", e))
            .build()
            .print();
        crate::error::CliError::CompilationFailed
    })?;

    let start_func = instance
        .get_typed_func::<(), ()>(&mut store, "_start")
        .map_err(|e| {
            let _ = Diagnostic::builder()
                .level(Level::Error)
                .message(format!(
                    "Failed to find _start function in module (is it a binary?): {}",
                    e
                ))
                .build()
                .print();
            crate::error::CliError::CompilationFailed
        })?;

    if let Err(e) = start_func.call(&mut store, ()) {
        if let Some(exit_code) = e.downcast_ref::<wasmtime_wasi::I32Exit>() {
            if exit_code.0 != 0 {
                let _ = Diagnostic::builder()
                    .level(Level::Error)
                    .message(format!("WASM program exited with code: {}", exit_code.0))
                    .build()
                    .print();
                return Err(crate::error::CliError::CompilationFailed);
            }
        } else {
            let _ = Diagnostic::builder()
                .level(Level::Error)
                .message(format!("Execution failed: {}", e))
                .build()
                .print();
            return Err(crate::error::CliError::CompilationFailed);
        }
    }

    Ok(())
}
