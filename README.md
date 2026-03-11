<div align="center">

<img src="https://capsule-render.vercel.app/api?type=waving&color=0:0d0d0d,40:1a1400,100:EAB308&height=220&section=header&text=WASOME&fontSize=80&fontColor=ffffff&fontAlign=50&fontAlignY=38&desc=The%20WebAssembly%20Language%20for%20Everyone&descSize=19&descFontColor=EAB308&descAlignY=62&animation=fadeIn&stroke=EAB308&strokeWidth=1" width="100%" />

<br/>

<img src=".github/assets/Wasome_Yellow.png" width="148" alt="Wasome" />

<br/><br/>

[![Typing SVG](https://readme-typing-svg.demolab.com?font=Fira+Code&weight=600&size=22&pause=2000&color=EAB308&center=true&vCenter=true&width=620&height=60&lines=Write+it.+Compile+it.+Run+it+anywhere.;Type-safe+WebAssembly%2C+for+everyone.;Compiles+to+Wasm.+Runs+everywhere.;Clean+syntax.+Strong+types.+Zero+bloat.)](https://wasome.dev)

<br/>

[![Website](https://img.shields.io/badge/wasome.dev-EAB308?style=for-the-badge&logo=googlechrome&logoColor=000000)](https://wasome.dev)
[![Docs](https://img.shields.io/badge/Docs-111111?style=for-the-badge&logo=bookstack&logoColor=EAB308)](https://wasome.dev/docs)
[![Tour](https://img.shields.io/badge/Tour-111111?style=for-the-badge&logo=compass&logoColor=EAB308)](https://wasome.dev/tour)
[![Playground](https://img.shields.io/badge/Playground-111111?style=for-the-badge&logo=codepen&logoColor=EAB308)](https://wasome.dev/playground)
[![Examples](https://img.shields.io/badge/Examples-111111?style=for-the-badge&logo=github&logoColor=EAB308)](https://wasome.dev/examples)
[![Install](https://img.shields.io/badge/Install-EAB308?style=for-the-badge&logo=rust&logoColor=000000)](https://wasome.dev/install)

<br/>

[![License: MIT](https://img.shields.io/badge/License-MIT-EAB308.svg?style=flat-square)](LICENSE-MIT)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-EAB308.svg?style=flat-square)](LICENSE-APACHE)
[![Built with Rust](https://img.shields.io/badge/Built%20with-Rust-EAB308?style=flat-square&logo=rust&logoColor=000000)](https://www.rust-lang.org)

</div>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

<div align="center">
<p>
Wasome is a programming language built from scratch to compile to WebAssembly.<br/>
A clean, expressive syntax paired with a strong type system that catches your mistakes before they become problems.<br/><br/>
Whether you're targeting the browser, a server, or the edge,<br/>
you write Wasome, compile to Wasm, and it <strong>just works</strong>.
</p>
</div>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Features

<br/>

<div align="center">

| &nbsp; | &nbsp; | &nbsp; |
|:---:|:---:|:---:|
| **WebAssembly Native** | **Type-Safe** | **Structs & Enums** |
| Compiles directly to Wasm. No runtime, no bloat. Near-native speed in any Wasm-compatible environment. | Strong static types — `s32` `u64` `f64` `bool` `char` — plus user-defined types. Bugs caught at compile time, not at 3 AM. | Define your own types and attach methods to them. Model your data exactly the way it makes sense to you. |
| **Generics** | **Modules & Imports** | **Built-in Formatter** |
| Full generic support for functions, structs, and enums. Flexible, reusable code without sacrificing type safety. | Split your code across multiple files and projects. Importing modules is straightforward and clean. | Ships with a formatter out of the box. Consistent style, zero configuration. Just the way it should be. |

</div>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Syntax at a Glance

<div align="right"><sub><b>Note:</b> Wasome uses <code>&lt;-</code> for assignment and <code>-&gt;</code> for return.</sub></div>

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

<br/>

<details>
<summary><b>Fibonacci Sequence</b></summary>
<br/>

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

</details>

<details>
<summary><b>Generics</b></summary>
<br/>

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

</details>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Explore Wasome

Everything you need to learn, experiment, and build with Wasome lives at [wasome.dev](https://wasome.dev).

<br/>

<table align="center" border="0" cellpadding="24" cellspacing="0">
<tr>
<td align="center" valign="top" width="33%">

**📚 Documentation**

<br/>

From zero to your first project.<br/>Guides, references,<br/>and the full language spec.

<br/>

[![Read the Docs](https://img.shields.io/badge/Read%20the%20Docs-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/docs)

</td>
<td align="center" valign="top" width="33%">

**🌟 Language Tour**

<br/>

An interactive walkthrough<br/>guided by **Bit**, your<br/>friendly star companion.

<br/>

[![Take the Tour](https://img.shields.io/badge/Take%20the%20Tour-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/tour)

</td>
<td align="center" valign="top" width="33%">

**🧪 Playground**

<br/>

Write and run Wasome<br/>right in your browser.<br/>No install. A mini IDE.

<br/>

[![Open Playground](https://img.shields.io/badge/Open%20Playground-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/playground)

</td>
</tr>
</table>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Examples

The `docs/examples/` directory has a growing collection of real programs, organized by complexity:

<br/>

<table border="0" cellpadding="8" cellspacing="0">
<tr>
<td width="10%" align="center">📄</td>
<td width="20%"><b>Single File</b></td>
<td>Standalone <code>.waso</code> files covering loops, structs, enums, generics, and operators. The perfect starting point.</td>
</tr>
<tr>
<td align="center">📁</td>
<td><b>Single Project</b></td>
<td>Multi-file projects with module imports. See how Wasome scales beyond a single file.</td>
</tr>
<tr>
<td align="center">🗂️</td>
<td><b>Multi-Project</b></td>
<td>Workspace-style projects with multiple modules. Real-world architecture, Wasome style.</td>
</tr>
</table>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Installation

Getting started is easy. Install the Wasome toolchain with a single command:

<br/>

<div align="center">

```sh
curl -fsSL https://get.wasome.org/install | sh
```

</div>

<br/>

Then verify everything is working:

```sh
wasome --version
```

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Architecture

The Wasome compiler is a modular Rust workspace. Source flows through six stages:

<br/>

<div align="center">
<img src=".github/assets/pipeline.svg" width="100%" alt="Wasome compiler pipeline: Source → Lexer → Parser → Semantic Analyzer → Code Gen → Linker" />
</div>

<br/>

**Supporting crates:**

| Crate | Purpose |
|---|---|
| `ast` | Typed and untyped AST definitions with traversal helpers |
| `driver` | Orchestrates the full pipeline end-to-end |
| `error` | Structured error reporting and diagnostics |
| `io` | File I/O utilities |
| `formatter` | Automatic code formatting for `.waso` files |

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Platform Support

Tested and verified across a wide range of platforms and architectures:

<br/>

<div align="center">

[![Linux](https://img.shields.io/badge/Linux-EAB308?style=for-the-badge&logo=linux&logoColor=000000)](https://wasome.dev)
[![macOS](https://img.shields.io/badge/macOS-111111?style=for-the-badge&logo=apple&logoColor=EAB308)](https://wasome.dev)
[![Windows](https://img.shields.io/badge/Windows-111111?style=for-the-badge&logo=windows11&logoColor=EAB308)](https://wasome.dev)
[![FreeBSD](https://img.shields.io/badge/FreeBSD-111111?style=for-the-badge&logo=freebsd&logoColor=EAB308)](https://wasome.dev)
[![ARM aarch64](https://img.shields.io/badge/ARM%20aarch64-111111?style=for-the-badge&logo=arm&logoColor=EAB308)](https://wasome.dev)
[![ARMv7](https://img.shields.io/badge/ARMv7-111111?style=for-the-badge&logo=arm&logoColor=EAB308)](https://wasome.dev)

</div>

<br/>

<div align="center"><i>If it runs code, chances are Wasome runs on it.</i></div>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ License

This project is dual-licensed under your choice of:

<br/>

<div align="center">

[![MIT](https://img.shields.io/badge/License-MIT-EAB308?style=for-the-badge)](LICENSE-MIT)
&nbsp;&nbsp;
[![Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-EAB308?style=for-the-badge)](LICENSE-APACHE)

</div>

<br/><br/>

<div align="center">

<img src=".github/assets/divider.svg" width="60%" />

<br/><br/>

<sub>Built with love and Rust &nbsp;·&nbsp; <a href="https://wasome.dev">wasome.dev</a></sub>

<br/><br/>

</div>
