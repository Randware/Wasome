<p align="center">
  <img src=".github/assets/Wasome_Yellow.png" alt="Wasome" width="120" />
</p>

<h1 align="center">Wasome</h1>

<p align="center">
  <strong>The WebAssembly language for everyone.</strong><br/>
  Write it. Compile it. Run it anywhere.
</p>

<p align="center">
  <a href="https://wasome.dev"><img src="https://img.shields.io/badge/wasome.dev-EAB308?style=for-the-badge&logoColor=000000" alt="Website"/></a>
  <a href="https://wasome.dev/docs"><img src="https://img.shields.io/badge/Docs-EAB308?style=for-the-badge&logoColor=000000" alt="Docs"/></a>
  <a href="https://wasome.dev/tour"><img src="https://img.shields.io/badge/Tour-EAB308?style=for-the-badge&logoColor=000000" alt="Tour"/></a>
  <a href="https://wasome.dev/playground"><img src="https://img.shields.io/badge/Playground-EAB308?style=for-the-badge&logoColor=000000" alt="Playground"/></a>
  <a href="https://wasome.dev/examples"><img src="https://img.shields.io/badge/Examples-EAB308?style=for-the-badge&logoColor=000000" alt="Examples"/></a>
</p>

<p align="center">
  <a href="https://github.com/wasomedev/wasome/actions"><img src="https://img.shields.io/github/actions/workflow/status/wasomedev/wasome/ci.yml?style=flat-square&label=CI&color=EAB308&labelColor=1a1a1a" alt="CI"/></a>
  <img src="https://img.shields.io/badge/License-MIT%20%7C%20Apache%202.0-EAB308?style=flat-square&labelColor=1a1a1a" alt="License"/>
  <img src="https://img.shields.io/badge/Built%20with-Rust-EAB308?style=flat-square&labelColor=1a1a1a&logo=rust&logoColor=EAB308" alt="Rust"/>
  <img src="https://img.shields.io/badge/Target-WebAssembly-EAB308?style=flat-square&labelColor=1a1a1a&logo=webassembly&logoColor=EAB308" alt="WebAssembly"/>
</p>

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

<br/>

Wasome is a programming language built from scratch to compile to WebAssembly. The idea is simple: a clean, expressive syntax paired with a strong type system that catches your mistakes before they become problems.

Whether you're targeting the browser, a server, or the edge — you write Wasome, compile to Wasm, and it just works.

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Features

<table>
<tr>
<td width="50%" valign="top">

**WebAssembly Native**
Compiles directly to WebAssembly. No intermediate runtime, no bloat. Near-native speed in any Wasm-compatible environment.

**Type-Safe**
Strong static types — `s32`, `u64`, `f64`, `bool`, `char` — plus user-defined types. Bugs get caught at compile time, not at 3 AM.

**Structs & Enums**
Define your own structs and enums, attach methods to them, and model your data the way it makes sense to you.

</td>
<td width="50%" valign="top">

**Generics**
Full generic support for functions, structs, and enums. Write flexible, reusable code without giving up type safety.

**Modules & Imports**
Split your code across multiple files and projects. Importing modules is straightforward and clean.

**Built-in Formatter**
Ships with a code formatter out of the box. Consistent style, zero configuration. Just the way it should be.

</td>
</tr>
</table>

<br/>

> Tested on **Linux**, **macOS**, **Windows**, **FreeBSD**, **ARM (aarch64)**, and **ARMv7**. If it runs code, chances are Wasome runs on it.

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Syntax at a Glance

Wasome aims to be readable and expressive. A few things to know upfront: `<-` is assignment, `->` is return.

```wasome
struct User {
    s32 id
    bool is_active
}

fn main() -> s32 {
    User u <- new User { id <- 42, is_active <- true }

    if (u.is_active) {
        -> u.id * 2
    }
    -> 0
}
```

<br/>

<details>
<summary><strong>Fibonacci sequence</strong></summary>
<br/>

```wasome
fn fibonacci(u8 n) -> u64 {
    u64 curr <- 1 as u32 as u64
    u64 prev <- 0 as u32 as u64
    loop (n > 0) {
        u64 temp <- curr
        curr <- curr + prev
        prev <- temp
        n <- n - 1 as u32 as u16 as u8
    }
    -> curr
}
```

</details>

<details>
<summary><strong>Generics</strong></summary>
<br/>

```wasome
struct Box[T] {
    T value
}

enum Option[T] {
    Some(T)
    None
}

pub fn identity[T](T val) -> T {
    -> val
}

fn main() {
    Box[s32] b <- new Box[s32] { value <- 10 }
    Option[f64] o <- Option[f64]::Some(5.5)
    s32 id <- identity[s32](b.value)
}
```

</details>

<details>
<summary><strong>Multi-file modules</strong></summary>
<br/>

```wasome
// main.waso
import "./utils" as u

fn main() -> s32 {
    u.add(1, 2)
    -> 0
}
```

</details>

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Explore Wasome

Everything you need to learn, experiment, and build with Wasome lives at [wasome.dev](https://wasome.dev).

| | |
|---|---|
| [**Documentation**](https://wasome.dev/docs) | Guides, references, and everything you need to go from zero to your first project. |
| [**Language Tour**](https://wasome.dev/tour) | An interactive walkthrough of the language basics, guided by Bit, your friendly star companion. |
| [**Playground**](https://wasome.dev/playground) | Write and run Wasome code directly in your browser. No install needed. Basically a mini IDE. |

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Examples

The `docs/examples/` directory has a growing collection of real programs, organized by complexity.

```
docs/examples/
├── single_file/        # Standalone .waso files: loops, structs, enums, generics, operators
├── single_project/     # Multi-file projects with module imports
└── multi-project/      # Workspace-style projects with multiple modules
```

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Installation

Getting started is easy. Install the Wasome toolchain with a single command:

```sh
curl -fsSL https://get.wasome.org/install | sh
```

Then verify everything is working:

```sh
wasome --version
```

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## Architecture

The Wasome compiler is a modular Rust workspace. Source flows through each stage in sequence:

```
Source ──► Lexer ──► Parser ──► Semantic Analyzer ──► Code Gen ──► Linker
```

| Stage | Crate | Role |
|---|---|---|
| Source | `source` | Load and manage `.waso` files |
| Lexer | `lexer` | Tokenize source |
| Parser | `parser` | Build untyped AST |
| Semantic Analyzer | `semantic_analyzer` | Type check, resolve symbols, validate |
| Code Gen | *(in progress)* | Lower to object code |
| Linker | *(in progress)* | Produce final binary |

**Supporting crates:**

- **`ast`** — typed & untyped AST definitions with traversal helpers
- **`driver`** — orchestrates the full pipeline end-to-end
- **`error`** — structured error reporting and diagnostics
- **`io`** — file I/O utilities
- **`formatter`** — automatic code formatting for `.waso` files, because nobody likes arguing about style

<br/>

<p align="center">
  <img src=".github/assets/divider.svg" width="100%" alt=""/>
</p>

## License

Dual-licensed under your choice of [MIT](LICENSE-MIT) or [Apache 2.0](LICENSE-APACHE).

<br/>

<p align="center">
  Built with love and Rust &nbsp;·&nbsp; <a href="https://wasome.dev">wasome.dev</a>
</p>
