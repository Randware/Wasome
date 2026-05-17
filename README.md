<div align="center">

<h3>Work in Progress</h3>

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

<div align="center">
<p>
Wasome is a programming language built from scratch to compile to WebAssembly.<br/>
Whether you're targeting the browser, a server, or the edge,<br/>
Write Wasome, compile to Wasm, and it <strong>just works</strong>.
</p>

</div>

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

## ✦ Features

<table border="0" cellpadding="12" cellspacing="0" width="100%">
<tr>
<td width="8%" align="center">⚡</td>
<td width="22%"><a href="https://webassembly.org/"><b>WebAssembly Native</b></a></td>
<td width="70%">Compiles directly to Wasm with no runtime overhead.</td>
</tr>
<tr>
<td width="8%" align="center">🛡️</td>
<td width="22%"><a href="https://wasome.dev/docs#datatypes"><b>Type-Safe</b></a></td>
<td width="70%">Strong static typing catches errors before your code reaches runtime.</td>
</tr>
<tr>
<td width="8%" align="center">🧱</td>
<td width="22%"><a href="https://wasome.dev/docs#structs"><b>Structs & Enums</b></a></td>
<td width="70%">Custom data models with methods and expressive patterns.</td>
</tr>
<tr>
<td width="8%" align="center">🔀</td>
<td width="22%"><b>Generics</b></td>
<td width="70%">Reusable abstractions across functions, structs, and enums.</td>
</tr>
<tr>
<td width="8%" align="center">📦</td>
<td width="22%"><b>Modules & Imports</b></td>
<td width="70%">Organize code cleanly across files and projects.</td>
</tr>
<tr>
<td width="8%" align="center">✨</td>
<td width="22%"><b>Built-in Formatter</b></td>
<td width="70%">Consistent style, automatically, with zero configuration.</td>
</tr>
</table>

<br/>


## ✦ Explore Wasome

Everything you need to learn, experiment, and build with Wasome lives at [wasome.dev](https://wasome.dev).

<br/>

<table border="0" cellpadding="12" cellspacing="0" width="100%">
<tr>
<td width="8%" align="center">📚</td>
<td width="22%"><a href="https://wasome.dev/docs"><b>Documentation</b></a></td>
<td width="70%">Guides, references, and the full language spec. From zero to your first project.</td>
</tr>
<tr>
<td width="8%" align="center">🌟</td>
<td width="22%"><a href="https://wasome.dev/tour"><b>Language Tour</b></a></td>
<td width="70%">An interactive language walkthrough, guided by <b>Bit</b>, your star companion.</td>
</tr>
<tr>
<td width="8%" align="center">🧪</td>
<td width="22%"><a href="https://wasome.dev/playground"><b>Playground</b></a></td>
<td width="70%">Write and run Wasome in your browser. No install needed; mini IDE included.</td>
</tr>
</table>

<br/>

## ✦ Examples

The `docs/examples/` directory has a growing collection of real programs, organized by complexity:

<br/>

<table border="0" cellpadding="12" cellspacing="0" width="100%">
<tr>
<td width="8%" align="center">📄</td>
<td width="22%"><a href="docs/examples/single_file"><b>Single File</b></a></td>
<td width="70%">Standalone <code>.waso</code> programs for loops, structs, enums, generics, and operators.</td>
</tr>
<tr>
<td width="8%" align="center">📁</td>
<td width="22%"><a href="docs/examples/single_project"><b>Single-Project</b></a></td>
<td width="70%">Multi-file projects with module imports; see how Wasome scales beyond one file.</td>
</tr>
<tr>
<td width="8%" align="center">🗂️</td>
<td width="22%"><a href="docs/examples/multi-project"><b>Multi-Project</b></a></td>
<td width="70%">Workspace-style projects with modules for real-world architecture.</td>
</tr>
</table>

<br/>

## ✦ Installation

Install the Wasome toolchain with a single command:

<br/>

<div align="center">

```sh
curl -fsSL https://get.wasome.dev/install | sh
```

</div>

<br/>

Then verify everything is working:

```sh
waso --version
```

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

<sub>Built with ❤️ and Rust &nbsp;·&nbsp; <a href="https://wasome.dev">wasome.dev</a></sub>

<br/><br/>

</div>
