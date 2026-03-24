<div align="center">

<img src="https://capsule-render.vercel.app/api?type=waving&color=0:EAB308,35:F6DD79,100:111111&height=200&section=header&text=WASOME&fontSize=58&fontColor=ffffff&fontAlignY=37&desc=The%20WebAssembly%20Language%20for%20Everyone&descAlignY=58&animation=fadeIn" alt="Wasome header" />

<img src=".github/assets/Wasome_Yellow.png" width="148" alt="Wasome" />

<br/><br/>

[![Typing SVG](https://readme-typing-svg.demolab.com?font=Fira+Code&weight=600&size=22&pause=1900&color=EAB308&center=true&vCenter=true&width=760&height=60&lines=Write+it.+Compile+it.+Run+it+anywhere.;Type-safe+WebAssembly%2C+for+everyone.;Clean+syntax.+Strong+types.+Zero+bloat.)](https://wasome.dev)

<br/>

[![Website](https://img.shields.io/badge/Website-wasome.dev-EAB308?style=for-the-badge&logo=googlechrome&logoColor=111111)](https://wasome.dev)
[![Docs](https://img.shields.io/badge/Docs-Reference-111111?style=for-the-badge&logo=gitbook&logoColor=EAB308)](https://wasome.dev/docs)
[![Tour](https://img.shields.io/badge/Tour-Interactive-111111?style=for-the-badge&logo=storybook&logoColor=EAB308)](https://wasome.dev/tour)
[![Playground](https://img.shields.io/badge/Playground-Browser-111111?style=for-the-badge&logo=stackblitz&logoColor=EAB308)](https://wasome.dev/playground)
[![Examples](https://img.shields.io/badge/Examples-Projects-111111?style=for-the-badge&logo=github&logoColor=EAB308)](https://wasome.dev/examples)
[![Install](https://img.shields.io/badge/Install-1--Command-EAB308?style=for-the-badge&logo=gnubash&logoColor=111111)](https://wasome.dev/install)

<br/>

[![WebAssembly](https://img.shields.io/badge/WebAssembly-Native-111111?style=flat-square&logo=webassembly&logoColor=EAB308)](https://wasome.dev)
[![Type Safe](https://img.shields.io/badge/Type%20System-Static-EAB308?style=flat-square&logo=typescript&logoColor=111111)](https://wasome.dev/docs)
[![Generics](https://img.shields.io/badge/Generics-Full-111111?style=flat-square&logo=polymerproject&logoColor=EAB308)](https://wasome.dev/docs)
[![Modules](https://img.shields.io/badge/Modules-Clean%20Imports-EAB308?style=flat-square&logo=filezilla&logoColor=111111)](https://wasome.dev/docs)
[![Formatter](https://img.shields.io/badge/Formatter-Built--in-111111?style=flat-square&logo=prettier&logoColor=EAB308)](https://wasome.dev)
[![Rust](https://img.shields.io/badge/Built%20with-Rust-EAB308?style=flat-square&logo=rust&logoColor=111111)](https://www.rust-lang.org)
[![Linux](https://img.shields.io/badge/Linux-Supported-111111?style=flat-square&logo=linux&logoColor=EAB308)](https://wasome.dev)
[![macOS](https://img.shields.io/badge/macOS-Supported-EAB308?style=flat-square&logo=apple&logoColor=111111)](https://wasome.dev)
[![Windows](https://img.shields.io/badge/Windows-Supported-111111?style=flat-square&logo=windows11&logoColor=EAB308)](https://wasome.dev)
[![FreeBSD](https://img.shields.io/badge/FreeBSD-Supported-EAB308?style=flat-square&logo=freebsd&logoColor=111111)](https://wasome.dev)
[![License: MIT](https://img.shields.io/badge/License-MIT-111111.svg?style=flat-square)](LICENSE-MIT)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-EAB308.svg?style=flat-square)](LICENSE-APACHE)

</div>

<p align="center">
  <img src=".github/assets/divider.svg" alt="divider" width="100%" />
</p>

<div align="center">

**A modern, type-safe language that compiles directly to WebAssembly.**

</div>

## Features

- **WebAssembly native:** direct compilation, no intermediate runtime.
- **Type-safe:** `s32`, `u64`, `f64`, `bool`, `char`, plus user-defined types.
- **Structs, enums, generics:** expressive and reusable without losing safety.
- **Modules and imports:** split code cleanly across files and projects.
- **Built-in formatter:** zero-config, consistent style out of the box.

<p align="center">
  <img src=".github/assets/divider.svg" alt="divider" width="100%" />
</p>

## Syntax

<sub>Wasome uses <code>&lt;-</code> for assignment and <code>-&gt;</code> for return.</sub>

```waso
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
<summary><b>More snippets</b></summary>

```waso
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

```waso
struct Box[T] { T value }

enum Option[T] {
    Some(T)
    None
}
```

```waso
// main.waso
import "./utils" as u

fn main() -> s32 {
    u.add(1, 2)
    -> 0
}
```

</details>

<p align="center">
  <img src=".github/assets/divider.svg" alt="divider" width="100%" />
</p>

## Explore

- **Website:** https://wasome.dev
- **Docs:** https://wasome.dev/docs
- **Tour:** https://wasome.dev/tour
- **Playground:** https://wasome.dev/playground
- **Examples:** https://wasome.dev/examples and `docs/examples/`

<p align="center">
  <img src=".github/assets/divider.svg" alt="divider" width="100%" />
</p>

## Install

```sh
curl -fsSL https://get.wasome.org/install | sh
wasome --version
```

<p align="center">
  <img src=".github/assets/pipeline.svg" alt="Wasome compiler pipeline" width="100%" />
</p>

## License

Dual-licensed under MIT or Apache 2.0.

<div align="center">
<sub>Built with love and Rust · <a href="https://wasome.dev">wasome.dev</a></sub>
</div>
