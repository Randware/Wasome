<div align="center">
  <img src=".github/assets/Wasome_Yellow.png" width="148" alt="Wasome" />

  [![Typing SVG](https://readme-typing-svg.demolab.com?font=Fira+Code&weight=600&size=22&pause=2200&color=EAB308&center=true&vCenter=true&width=620&height=60&lines=Write+it.+Compile+it.+Run+it+anywhere.;Type-safe+WebAssembly%2C+for+everyone.;Clean+syntax.+Strong+types.+Zero+bloat.)](https://wasome.dev)

  [![Website](https://img.shields.io/badge/wasome.dev-EAB308?style=for-the-badge&logo=googlechrome&logoColor=000000)](https://wasome.dev)
  [![Docs](https://img.shields.io/badge/Docs-111111?style=for-the-badge&logo=bookstack&logoColor=EAB308)](https://wasome.dev/docs)
  [![Playground](https://img.shields.io/badge/Playground-111111?style=for-the-badge&logo=codepen&logoColor=EAB308)](https://wasome.dev/playground)
  [![Install](https://img.shields.io/badge/Install-EAB308?style=for-the-badge&logo=rust&logoColor=000000)](https://wasome.dev/install)

  [![License: MIT](https://img.shields.io/badge/License-MIT-EAB308.svg?style=flat-square)](LICENSE-MIT)
  [![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-EAB308.svg?style=flat-square)](LICENSE-APACHE)
  [![Built with Rust](https://img.shields.io/badge/Built%20with-Rust-EAB308?style=flat-square&logo=rust&logoColor=000000)](https://www.rust-lang.org)
</div>

Wasome is a programming language built from scratch for WebAssembly.
It gives you expressive syntax, strong static typing, and clean compilation to Wasm for browser, server, and edge runtimes.

## ✦ Syntax at a Glance

> Wasome uses `<-` for assignment and `->` for return.

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
<summary><b>More examples (Fibonacci + Generics)</b></summary>

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

struct Box[T] {
    T value
}

pub fn identity[T](T val) -> T {
    -> val
}
```

</details>

## ✦ Features

- ⚡ **WebAssembly-native**: compile directly to Wasm with no runtime overhead.
- 🛡️ **Strong static typing**: catch errors before runtime.
- 🧱 **Structs, enums, generics**: model real data with reusable abstractions.
- 📦 **Modules and imports**: scale from one file to multi-project workspaces.
- ✨ **Built-in formatter**: consistent style with zero configuration.

## ✦ Explore Wasome

Everything lives at [wasome.dev](https://wasome.dev):

- 📚 [Documentation](https://wasome.dev/docs) - language guide, references, full spec.
- 🌟 [Language Tour](https://wasome.dev/tour) - interactive walkthrough with Bit.
- 🧪 [Playground](https://wasome.dev/playground) - run Wasome in your browser.
- 🧩 [Examples](https://wasome.dev/examples) - practical code to learn by reading.

## ✦ Local Examples

The repo also includes `docs/examples/`:

- `docs/examples/single_file` - focused, standalone `.waso` programs.
- `docs/examples/single_project` - multi-file projects with imports.
- `docs/examples/multi-project` - workspace-style setups.

## ✦ Installation

```sh
curl -fsSL https://get.wasome.dev/install | sh
waso --version
```

## ✦ Platform Support

Tested across Linux, macOS, Windows, FreeBSD, ARM aarch64, and ARMv7.

<div align="center">
  <i>If it runs code, chances are Wasome runs on it.</i>
</div>

## ✦ License

Dual-licensed under your choice of [MIT](LICENSE-MIT) or [Apache 2.0](LICENSE-APACHE).

<div align="center">
  <sub>Built with love and Rust · <a href="https://wasome.dev">wasome.dev</a></sub>
</div>
