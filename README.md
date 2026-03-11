<div align="center">

<img src="https://capsule-render.vercel.app/api?type=waving&color=0:1a1a1a,50:2d2300,100:EAB308&height=200&section=header&text=WASOME&fontSize=72&fontColor=EAB308&fontAlign=50&fontAlignY=38&desc=The%20WebAssembly%20Language%20for%20Everyone&descSize=18&descFontColor=ffffff&descAlignY=62&animation=fadeIn" width="100%" />

<br/>

<img src=".github/assets/Wasome_Dark.png" width="160" alt="Wasome Logo" />

<br/><br/>

[![Typing SVG](https://readme-typing-svg.demolab.com?font=Fira+Code&weight=600&size=22&pause=2000&color=EAB308&center=true&vCenter=true&width=600&height=60&lines=Write+it.+Compile+it.+Run+it+anywhere.;Type-safe+WebAssembly%2C+for+everyone.;Compiles+to+Wasm.+Runs+everywhere.;Clean+syntax.+Strong+types.+Zero+bloat.)](https://wasome.dev)

<br/>

[![Website](https://img.shields.io/badge/wasome.dev-EAB308?style=for-the-badge&logo=googlechrome&logoColor=000000)](https://wasome.dev)
[![Docs](https://img.shields.io/badge/Docs-1a1a1a?style=for-the-badge&logo=bookstack&logoColor=EAB308)](https://wasome.dev/docs)
[![Tour](https://img.shields.io/badge/Tour-1a1a1a?style=for-the-badge&logo=compass&logoColor=EAB308)](https://wasome.dev/tour)
[![Playground](https://img.shields.io/badge/Playground-1a1a1a?style=for-the-badge&logo=codepen&logoColor=EAB308)](https://wasome.dev/playground)
[![Examples](https://img.shields.io/badge/Examples-1a1a1a?style=for-the-badge&logo=github&logoColor=EAB308)](https://wasome.dev/examples)
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
Whether you're targeting the browser, a server, or the edge —<br/>
you write Wasome, compile to Wasm, and it <strong>just works</strong>.
</p>
</div>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Features

<br/>

<table>
<tr>
<td width="50%" valign="top">

### ⚡ WebAssembly Native
Compiles directly to WebAssembly. No intermediate runtime, no bloat. Near-native speed in any Wasm-compatible environment.

</td>
<td width="50%" valign="top">

### 🛡️ Type-Safe
Strong static types — `s32`, `u64`, `f64`, `bool`, `char` — plus user-defined types. Bugs get caught at compile time, not at 3 AM.

</td>
</tr>
<tr>
<td width="50%" valign="top">

### 🧱 Structs & Enums
Define your own structs and enums, attach methods to them, and model your data exactly the way it makes sense to you.

</td>
<td width="50%" valign="top">

### 🔀 Generics
Full generic support for functions, structs, and enums. Write flexible, reusable code without giving up type safety.

</td>
</tr>
<tr>
<td width="50%" valign="top">

### 📦 Modules & Imports
Split your code across multiple files and projects. Importing modules is straightforward and clean.

</td>
<td width="50%" valign="top">

### 🎨 Built-in Formatter
Ships with a code formatter out of the box. Consistent style, zero configuration. Just the way it should be.

</td>
</tr>
</table>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Syntax at a Glance

Wasome is designed to be readable and expressive from the first line. Note: Wasome uses `<-` for assignment and `->` for return.

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
<summary><b>📐 Fibonacci Sequence</b></summary>
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
<summary><b>🔀 Generics</b></summary>
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
<summary><b>📦 Multi-file Modules</b></summary>
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

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Explore Wasome

Everything you need to learn, experiment, and build with Wasome lives at [wasome.dev](https://wasome.dev).

<br/>

<table>
<tr>
<td width="33%" align="center" valign="top">

### 📚 Documentation
Guides, references, and everything you need to go from zero to your first project.

[![Docs →](https://img.shields.io/badge/Read%20the%20Docs-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/docs)

</td>
<td width="33%" align="center" valign="top">

### 🌟 Language Tour
An interactive walkthrough of the language basics, guided by **Bit**, your friendly star companion!

[![Take the Tour →](https://img.shields.io/badge/Take%20the%20Tour-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/tour)

</td>
<td width="33%" align="center" valign="top">

### 🧪 Playground
Write and run Wasome code directly in your browser. No install needed. Basically a mini IDE!

[![Open Playground →](https://img.shields.io/badge/Open%20Playground-EAB308?style=for-the-badge&logoColor=000000)](https://wasome.dev/playground)

</td>
</tr>
</table>

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Examples

The `docs/examples/` directory has a growing collection of real programs, organized by complexity:

<br/>

<table>
<tr>
<td width="10%" align="center">📄</td>
<td width="20%"><b>Single File</b></td>
<td>Standalone <code>.waso</code> files — loops, structs, enums, generics, operators. The perfect starting point.</td>
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

The Wasome compiler is built in Rust as a modular workspace. Source flows through each stage in sequence:

<br/>

<div align="center">

```
  .waso source
      │
      ▼
  ┌─────────┐
  │ Source  │  Load & manage .waso files
  └────┬────┘
       │
       ▼
  ┌─────────┐
  │  Lexer  │  Tokenize source into a stream of tokens
  └────┬────┘
       │
       ▼
  ┌─────────┐
  │ Parser  │  Build an untyped AST
  └────┬────┘
       │
       ▼
  ┌──────────────────┐
  │ Semantic Analyzer│  Type check · resolve symbols · validate
  └────────┬─────────┘
           │
           ▼
  ┌──────────┐
  │ Code Gen │  Lower to object code
  └────┬─────┘
       │
       ▼
  ┌─────────┐
  │ Linker  │  Produce the final .wasm binary
  └─────────┘
```

</div>

<br/>

**Supporting crates:**

| Crate | Purpose |
|---|---|
| `ast` | Typed & untyped AST definitions with traversal helpers |
| `driver` | Orchestrates the full pipeline end-to-end |
| `error` | Structured error reporting and diagnostics |
| `io` | File I/O utilities |
| `formatter` | Automatic code formatting for `.waso` files — because nobody likes arguing about style |

<br/>

<img src=".github/assets/divider.svg" width="100%" />

<br/>

## ✦ Platform Support

Tested and verified across a wide range of platforms and architectures:

<br/>

<div align="center">

[![Linux](https://img.shields.io/badge/Linux-EAB308?style=for-the-badge&logo=linux&logoColor=000000)](https://wasome.dev)
[![macOS](https://img.shields.io/badge/macOS-1a1a1a?style=for-the-badge&logo=apple&logoColor=EAB308)](https://wasome.dev)
[![Windows](https://img.shields.io/badge/Windows-1a1a1a?style=for-the-badge&logo=windows11&logoColor=EAB308)](https://wasome.dev)
[![FreeBSD](https://img.shields.io/badge/FreeBSD-1a1a1a?style=for-the-badge&logo=freebsd&logoColor=EAB308)](https://wasome.dev)
[![ARM aarch64](https://img.shields.io/badge/ARM%20aarch64-1a1a1a?style=for-the-badge&logo=arm&logoColor=EAB308)](https://wasome.dev)
[![ARMv7](https://img.shields.io/badge/ARMv7-1a1a1a?style=for-the-badge&logo=arm&logoColor=EAB308)](https://wasome.dev)

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

<br/>

<img src="https://capsule-render.vercel.app/api?type=waving&color=0:EAB308,50:2d2300,100:1a1a1a&height=140&section=footer&reversal=true" width="100%" />

<div align="center">

<sub>Built with love and Rust &nbsp;·&nbsp; <a href="https://wasome.dev">wasome.dev</a></sub>

</div>
