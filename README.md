<div align="center">

<img src="https://capsule-render.vercel.app/api?type=waving&color=eab308&height=220&section=header&text=Wasome&fontSize=90&fontColor=000000&animation=fadeIn&desc=The%20WebAssembly%20Language%20for%20Everyone&descSize=24&descAlignY=72&descColor=000000&fontAlignY=42" width="100%">

<br>

<img src=".github/assets/Wasome_Yellow.png" alt="Wasome Logo" width="120">

<br><br>

<a href="https://wasome.dev">
  <img src="https://readme-typing-svg.demolab.com?font=Fira+Code&size=18&duration=3000&pause=1000&color=EAB308&center=true&vCenter=true&multiline=false&width=540&lines=A+modern%2C+type-safe+WebAssembly+language.;Write+it.+Compile+it.+Run+it+anywhere.;Strong+types.+Zero+runtime+bloat.;Built+in+Rust.+Runs+everywhere+Wasm+does." alt="Typing SVG">
</a>

<br><br>

[![CI](https://img.shields.io/github/actions/workflow/status/Randware/Wasome/ci.yml?branch=main&style=for-the-badge&logo=github&label=CI&color=eab308&logoColor=black)](https://github.com/Randware/Wasome/actions/workflows/ci.yml)
[![Website](https://img.shields.io/badge/wasome.dev-visit-eab308?style=for-the-badge&logo=googlechrome&logoColor=black)](https://wasome.dev)
[![License](https://img.shields.io/badge/MIT%20%2F%20Apache--2.0-license-eab308?style=for-the-badge&logoColor=black)](https://github.com/Randware/Wasome/blob/main/LICENSE-MIT)
[![Status](https://img.shields.io/badge/Alpha-status-eab308?style=for-the-badge&logoColor=black)](https://wasome.dev)

<br>

<a href="https://wasome.dev">Website</a>&ensp;·&ensp;<a href="https://wasome.dev/docs">Docs</a>&ensp;·&ensp;<a href="https://wasome.dev/tour">Tour</a>&ensp;·&ensp;<a href="https://wasome.dev/playground">Playground</a>&ensp;·&ensp;<a href="https://wasome.dev/examples">Examples</a>&ensp;·&ensp;<a href="https://wasome.dev/install">Install</a>

<br>

</div>

<img src=".github/assets/divider.svg" width="100%">

<br>

**Wasome** is a programming language built from scratch to compile to [WebAssembly](https://webassembly.org/). The idea is simple: a clean, expressive syntax paired with a strong type system that catches your mistakes before they become problems.

Whether you're targeting the browser, a server, or the edge — you write Wasome, compile to Wasm, and it just works.

<br>

<div align="center">

| [🚀 Features](#-features) | [📝 Syntax](#-syntax-at-a-glance) | [🌐 Explore](#-explore-wasome) | [📚 Examples](#-examples) | [⬇️ Install](#️-installation) | [🏗️ Architecture](#️-architecture) | [🔨 Built With](#-built-with) | [📄 License](#-license) |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|

</div>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>🚀 Features</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

<table>
<tr>
<td width="50%" valign="top">

### ⚡ WebAssembly Native

Compiles directly to WebAssembly. No intermediate runtime, no bloat. Near-native speed in any Wasm-compatible environment.

</td>
<td width="50%" valign="top">

### 🛡️ Type-Safe

Strong static types (`s32`, `u64`, `f64`, `bool`, `char`) plus user-defined types. Bugs get caught at compile time, not at 3 AM.

</td>
</tr>
<tr>
<td width="50%" valign="top">

### 📦 Structs & Enums

Define your own structs and enums, attach methods to them, and model your data the way it makes sense to you.

</td>
<td width="50%" valign="top">

### 🧬 Generics

Full generic support for functions, structs, and enums. Write flexible, reusable code without giving up type safety.

</td>
</tr>
<tr>
<td width="50%" valign="top">

### 📂 Modules & Imports

Split your code across multiple files and projects. A filesystem-based module system that's straightforward and clean.

</td>
<td width="50%" valign="top">

### 🔧 Built-in Formatter

Ships with a code formatter out of the box. Consistent style, zero configuration. Just the way it should be.

</td>
</tr>
</table>

<br>

<div align="center">

🖥️ &nbsp;Tested on **Linux** &nbsp;·&nbsp; **macOS** &nbsp;·&nbsp; **Windows** &nbsp;·&nbsp; **FreeBSD** &nbsp;·&nbsp; **ARM (aarch64)** &nbsp;·&nbsp; **ARMv7**

*If it runs code, chances are Wasome runs on it.*

<br>

![Linux](https://img.shields.io/badge/Linux-eab308?style=flat-square&logo=linux&logoColor=black)
![macOS](https://img.shields.io/badge/macOS-eab308?style=flat-square&logo=apple&logoColor=black)
![Windows](https://img.shields.io/badge/Windows-eab308?style=flat-square&logo=windows&logoColor=black)
![FreeBSD](https://img.shields.io/badge/FreeBSD-eab308?style=flat-square&logo=freebsd&logoColor=black)
![ARM64](https://img.shields.io/badge/ARM64-eab308?style=flat-square&logo=arm&logoColor=black)
![ARMv7](https://img.shields.io/badge/ARMv7-eab308?style=flat-square&logo=arm&logoColor=black)

</div>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>📝 Syntax at a Glance</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

Wasome aims to be readable and expressive. Here's a quick taste:

```rust
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

> [!NOTE]
> Wasome uses `<-` for assignment and `->` for return. Newlines are statement separators — no semicolons needed.

<details>
<summary><b>More examples</b> &nbsp;—&nbsp; Fibonacci, Generics, Modules, Methods, Pattern Matching</summary>

<br>

**Fibonacci sequence:**

```rust
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

**Generics:**

```rust
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

**Structs with methods:**

```rust
struct Point {
    s32 x
    s32 y

    pub fn get_x() -> s32 {
        -> self.x
    }
}

fn main() {
    Point point <- new Point { x <- 10, y <- 20 }
    s32 x_coordinate <- (point).get_x()
}
```

**Enums with pattern matching (`if let`):**

```rust
enum Status {
    Ok
    Err(s32)
}

fn main() {
    Status result <- Status::Err(404)

    if (let Status::Err(s32 code) <- result) {
        // code is bound here — Wasome's take on pattern matching
    }
}
```

**Multi-file modules:**

```rust
// main.waso
import "./utils" as u

fn main() -> s32 {
    u.add(1, 2)
    -> 0
}
```

</details>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>🌐 Explore Wasome</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

Everything you need to learn, experiment, and build with Wasome lives at **[wasome.dev](https://wasome.dev)**.

<br>

<table>
<tr>
<td align="center" width="33%" valign="top">

**📖 Documentation**

Guides, references, and everything you need to go from zero to your first project.

<br>

[**Read the Docs →**](https://wasome.dev/docs)

</td>
<td align="center" width="33%" valign="top">

**🗺️ Language Tour**

An interactive walkthrough of the language basics, guided by **Bit**, your friendly star companion! ⭐

<br>

[**Take the Tour →**](https://wasome.dev/tour)

</td>
<td align="center" width="33%" valign="top">

**🎮 Playground**

Write and run Wasome code directly in your browser. No install needed. Basically a mini IDE!

<br>

[**Try it out →**](https://wasome.dev/playground)

</td>
</tr>
</table>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>📚 Examples</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

The [`docs/examples/`](docs/examples/) directory has a growing collection of real programs, organized by complexity:

<br>

<table>
<tr>
<td align="center" width="33%" valign="top">

**📄 Single File**

Standalone `.waso` files: loops, structs, enums, generics, operators.

[**Browse →**](docs/examples/single_file/)

</td>
<td align="center" width="33%" valign="top">

**📁 Single Project**

Multi-file projects with module imports.

[**Browse →**](docs/examples/single_project/)

</td>
<td align="center" width="33%" valign="top">

**🗂️ Multi-Project**

Workspace-style projects with multiple modules.

[**Browse →**](docs/examples/multi-project/)

</td>
</tr>
</table>

<br>

<div align="center">

[![Browse on the Web](https://img.shields.io/badge/🔍_Browse_Examples-wasome.dev%2Fexamples-eab308?style=for-the-badge&logoColor=black)](https://wasome.dev/examples)

</div>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>⬇️ Installation</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

Getting started is easy. Install the Wasome toolchain with a single command:

```bash
curl -fsSL https://get.wasome.org/install | sh
```

Then verify everything is working:

```bash
wasome --version
```

<br>

<div align="center">

[![Full Install Guide](https://img.shields.io/badge/📥_Full_Install_Guide-wasome.dev%2Finstall-eab308?style=for-the-badge&logoColor=black)](https://wasome.dev/install)

</div>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>🏗️ Architecture</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

The Wasome compiler is built in **Rust** as a modular workspace. Source flows through each stage in sequence:

<br>

<div align="center">

```
  .waso source files
          │
          ▼
  ┌───────────────┐
  │    source     │  load & manage .waso files · assign file IDs · byte-precise spans
  └───────┬───────┘
          │
          ▼
  ┌───────────────┐
  │     lexer     │  DFA-based tokenizer (Logos) → token stream
  └───────┬───────┘
          │
          ▼
  ┌───────────────┐
  │    parser     │  combinator parsing (Chumsky) → untyped AST
  └───────┬───────┘
          │
          ▼
  ┌─────────────────────────┐
  │   semantic_analyzer     │  two-pass: collect symbols → type-check → typed AST
  └───────────┬─────────────┘
              │
              ▼
  ┌───────────────┐
  │   code gen    │  lower to object code                      ← coming soon
  └───────┬───────┘
          │
          ▼
  ┌───────────────┐
  │    linker     │  produce final .wasm binary                ← coming soon
  └───────────────┘
```

</div>

<br>

**Supporting crates:**

| Crate | Role |
|---|---|
| `ast` | Typed & untyped AST definitions with traversal helpers. A single generic `AST<Type>` serves both stages. |
| `driver` | Orchestrates the full pipeline end-to-end via a composable `Pipeline` trait |
| `error` | Structured error reporting with source snippets and precise byte-range annotations |
| `io` | File I/O abstraction (swappable for testing) |
| `formatter` | Token-level automatic code formatter for `.waso` files — works even on syntactically invalid code |

<br>

> [!TIP]
> The typed AST is **self-validating by construction** — typed node constructors return `Option<Self>` and perform type validation at build time. If an `AST<TypedAST>` exists, it is guaranteed to be type-correct.

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>🔨 Built With</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

<div align="center">

[![Rust](https://img.shields.io/badge/Rust-000000?style=for-the-badge&logo=rust&logoColor=white)](https://www.rust-lang.org/)
[![WebAssembly](https://img.shields.io/badge/WebAssembly-654FF0?style=for-the-badge&logo=webassembly&logoColor=white)](https://webassembly.org/)
[![Logos](https://img.shields.io/badge/Logos-lexer-eab308?style=for-the-badge&logoColor=black)](https://github.com/maciejhirsz/logos)
[![Chumsky](https://img.shields.io/badge/Chumsky-parser-eab308?style=for-the-badge&logoColor=black)](https://github.com/zesterer/chumsky)

</div>

<br>

<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">
<div align="center"><h2>📄 License</h2></div>
<img src="https://capsule-render.vercel.app/api?type=rect&color=eab308&height=3" width="100%">

<br>

This project is dual-licensed under your choice of **[MIT](LICENSE-MIT)** or **[Apache 2.0](LICENSE-APACHE)**.

<br>

<div align="center">

<img src="https://capsule-render.vercel.app/api?type=waving&color=eab308&height=160&section=footer&text=Built%20with%20❤️%20and%20Rust&fontSize=26&fontColor=000000&animation=fadeIn&desc=wasome.dev&descSize=18&descColor=000000&descAlignY=72" width="100%">

</div>
