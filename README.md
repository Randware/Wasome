<p align="center">
  <img src=".github/assets/Wasome_Yellow.png" alt="Wasome Logo" width="200">
</p>

<h1 align="center">Wasome</h1>

<p align="center">
  <strong>The WebAssembly Language for Everyone</strong>
</p>

<p align="center">
  A modern, type-safe, high-performance language that compiles directly to WebAssembly.<br>
  Expressive syntax. Blazing fast. Built for the web.
</p>

<p align="center">
  <a href="https://github.com/Randware/Wasome/actions/workflows/ci.yml"><img src="https://img.shields.io/github/actions/workflow/status/Randware/Wasome/ci.yml?branch=main&style=flat-square&logo=github&label=CI&color=%23eab308" alt="CI Status"></a>&nbsp;
  <a href="https://wasome.dev"><img src="https://img.shields.io/badge/Website-wasome.dev-%23eab308?style=flat-square&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNCIgaGVpZ2h0PSIyNCIgdmlld0JveD0iMCAwIDI0IDI0IiBmaWxsPSJub25lIiBzdHJva2U9IndoaXRlIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PGNpcmNsZSBjeD0iMTIiIGN5PSIxMiIgcj0iMTAiLz48cGF0aCBkPSJNMiAxMmgyMCIvPjxwYXRoIGQ9Ik0xMiAyYTE1LjMgMTUuMyAwIDAgMSA0IDEwIDE1LjMgMTUuMyAwIDAgMS00IDEwIDE1LjMgMTUuMyAwIDAgMS00LTEwIDE1LjMgMTUuMyAwIDAgMSA0LTEweiIvPjwvc3ZnPg==" alt="Website"></a>&nbsp;
  <a href="https://github.com/Randware/Wasome/blob/main/LICENSE-MIT"><img src="https://img.shields.io/badge/License-MIT%2FApache--2.0-%23eab308?style=flat-square" alt="License"></a>&nbsp;
  <img src="https://img.shields.io/badge/Status-Alpha-%23eab308?style=flat-square" alt="Alpha">
</p>

<p align="center">
  <a href="https://wasome.dev">Website</a> · <a href="https://wasome.dev/docs">Docs</a> · <a href="https://wasome.dev/tour">Tour</a> · <a href="https://wasome.dev/playground">Playground</a> · <a href="https://wasome.dev/examples">Examples</a> · <a href="https://wasome.dev/install">Install</a>
</p>

<br>

<p align="center">
  <img src=".github/assets/banner.png" alt="Wasome — The WebAssembly Language" width="850">
</p>

<br>

## ✨ What is Wasome?

**Wasome** is a modern programming language designed from the ground up to compile to [WebAssembly](https://webassembly.org/). It combines a clean, expressive syntax with a strong static type system — making it approachable for newcomers while being powerful enough for real applications.

Write in Wasome, compile to Wasm, run anywhere — in the browser, on the server, or at the edge.

<br>

## 🚀 Features

<table>
  <tr>
    <td width="50%" valign="top">
      <h3>⚡ WebAssembly Native</h3>
      <p>Compiles directly to WebAssembly — no intermediate runtime, no bloat. Your code runs at near-native speed in any Wasm-compatible environment.</p>
    </td>
    <td width="50%" valign="top">
      <h3>🛡️ Type-Safe</h3>
      <p>Strong static type system with numeric types (<code>s32</code>, <code>u64</code>, <code>f64</code>), <code>bool</code>, <code>char</code>, and user-defined types — catching errors at compile time.</p>
    </td>
  </tr>
  <tr>
    <td width="50%" valign="top">
      <h3>📦 Structs & Enums</h3>
      <p>First-class data structures with methods. Define custom types, attach behavior, and model your domain cleanly.</p>
    </td>
    <td width="50%" valign="top">
      <h3>🧬 Generics</h3>
      <p>Full generic type support for functions, structs, and enums. Write flexible, reusable code without sacrificing type safety.</p>
    </td>
  </tr>
  <tr>
    <td width="50%" valign="top">
      <h3>📂 Modules & Imports</h3>
      <p>Organize code across multiple files and projects. Import modules with a simple, clean syntax.</p>
    </td>
    <td width="50%" valign="top">
      <h3>🔧 Built-in Formatter</h3>
      <p>Ship with a code formatter out of the box — consistent style across every project, zero configuration.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" align="center">
      <h3>🖥️ Cross-Platform</h3>
      <p>Tested on <strong>Linux</strong>, <strong>macOS</strong>, <strong>Windows</strong>, <strong>FreeBSD</strong>, <strong>ARM (aarch64)</strong>, and <strong>ARMv7</strong> — runs everywhere you need it.</p>
    </td>
  </tr>
</table>

<br>

## 📝 Syntax at a Glance

Wasome's syntax is clean and expressive. Here's a taste:

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

<details>
<summary><strong>More examples</strong> — Fibonacci, Generics, Modules</summary>

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

> **Note:** Wasome uses `<-` for assignment and `->` for return.

<br>

## 🌐 Website & Documentation

<a href="https://wasome.dev">
  <img src=".github/assets/website_hero.png" alt="Wasome Website" width="850">
</a>

Visit **[wasome.dev](https://wasome.dev)** for full documentation, guides, and API reference. Everything you need to get productive with Wasome — from first steps to advanced topics.

<p align="center">
  <a href="https://wasome.dev/docs"><img src="https://img.shields.io/badge/📖_Read_the_Docs-wasome.dev/docs-%23eab308?style=for-the-badge" alt="Read the Docs"></a>
</p>

<br>

## 🗺️ Language Tour

New to Wasome? The **interactive language tour** walks you through the basics step-by-step — guided by **Bit**, your friendly star companion. ⭐

<p align="center">
  <img src=".github/assets/tour.png" alt="Wasome Language Tour" width="750">
</p>

<p align="center">
  <a href="https://wasome.dev/tour"><img src="https://img.shields.io/badge/🚀_Take_the_Tour-wasome.dev/tour-%23eab308?style=for-the-badge" alt="Take the Tour"></a>
</p>

<br>

## 🎮 Playground

Write, edit, and run Wasome code **directly in your browser** — no installation required. The playground features a full code editor with file explorer, syntax highlighting, and live console output.

<p align="center">
  <img src=".github/assets/playground.png" alt="Wasome Playground" width="750">
</p>

<p align="center">
  <a href="https://wasome.dev/playground"><img src="https://img.shields.io/badge/⚡_Try_the_Playground-wasome.dev/playground-%23eab308?style=for-the-badge" alt="Try the Playground"></a>
</p>

<br>

## 📚 Examples

Explore real Wasome programs to see the language in action — from simple scripts to multi-file projects.

<p align="center">
  <img src=".github/assets/examples.png" alt="Wasome Examples" width="750">
</p>

The [`docs/examples/`](docs/examples/) directory contains a growing collection organized by complexity:

| Category | Description |
|----------|-------------|
| [`single_file`](docs/examples/single_file/) | Standalone `.waso` files — loops, structs, enums, generics, operators |
| [`single_project`](docs/examples/single_project/) | Multi-file projects with module imports |
| [`multi-project`](docs/examples/multi-project/) | Workspace-style projects with multiple modules |

<p align="center">
  <a href="https://wasome.dev/examples"><img src="https://img.shields.io/badge/🔍_Browse_Examples-wasome.dev/examples-%23eab308?style=for-the-badge" alt="Browse Examples"></a>
</p>

<br>

## ⬇️ Installation

Install the Wasome toolchain with a single command:

```bash
curl -fsSL https://get.wasome.org/install | sh
```

Verify your installation:

```bash
wasome --version
```

<p align="center">
  <a href="https://wasome.dev/install"><img src="https://img.shields.io/badge/📥_Full_Install_Guide-wasome.dev/install-%23eab308?style=for-the-badge" alt="Install Guide"></a>
</p>

<br>

## 🏗️ Architecture

The Wasome compiler is built in **Rust** and organized as a modular workspace of focused crates:

```
Source (.waso) → Lexer → Parser → Untyped AST → Semantic Analyzer → Typed AST → Code Gen → .o File → Linker → Binary
```

| Crate | Role |
|-------|------|
| `source` | Source file loading and management |
| `lexer` | Tokenization of `.waso` source files |
| `parser` | Parsing token streams into an untyped AST |
| `ast` | Typed & untyped AST representations with traversal helpers |
| `semantic_analyzer` | Type checking, symbol resolution, and semantic validation |
| `driver` | Compiler orchestration — ties the pipeline together |
| `error` | Structured error reporting and diagnostics |
| `io` | File I/O utilities |

Additionally, the **`formatter`** crate provides automatic code formatting for `.waso` files.

<br>

## 📄 License

Wasome is dual-licensed under your choice of:

- **[MIT License](LICENSE-MIT)**
- **[Apache License 2.0](LICENSE-APACHE)**

<br>

---

<p align="center">
  Built with ❤️ and Rust&nbsp;&nbsp;·&nbsp;&nbsp;<a href="https://wasome.dev">wasome.dev</a>&nbsp;&nbsp;·&nbsp;&nbsp;<a href="https://github.com/Randware/Wasome">GitHub</a>
</p>
