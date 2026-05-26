export const steps = [
  {
    title: "Welcome to Wasome",
    content: "Hi there! I'm Bit, your guide to Wasome! 🌟 Wasome is a simple, fun language designed to compile straight to WebAssembly.\n\nOur journey starts with the `main` function—the entry point of every program. Notice the curly brackets `{}` enclosing our block, and `//` for comments. They're just for us humans!",
    code: `fn main() {
    // Hello! This is a comment.
    // Our journey begins here!
}`
  },
  {
    title: "Variables",
    content: "Let's store some values! In Wasome, you declare a variable by stating its type first, then its name, and assigning a value with the left arrow `<-`.\n\nWe have types like `s32` (integer), `f64` (float), and `bool` (true/false). Variables are mutable by default, so you can change them anytime with that same arrow!",
    code: `fn main() {
    s32 count <- 0
    count <- count + 1
    
    f64 pi <- 3.14159
    bool wasome_is_awesome <- true
}`
  },
  {
    title: "Typecasting",
    content: "Sometimes numbers want to wear a different hat! In Wasome, convert one numerical type to another using the `as` keyword.\n\nNote that you cannot cast directly between types of different bit-widths (like `f64` to `s32`)—you must take small steps by casting to an intermediate size like `s64` first! Narrowing chops off decimals and can overflow.",
    code: `fn main() {
    f64 precise_pi <- 3.14159
    s32 rough_pi <- precise_pi as s64 as s32
    f64 normal_pi <- rough_pi as s64 as f64
}`
  },
  {
    title: "Functions",
    content: "Functions are the primary building blocks of reusable logic! We declare them with the `fn` keyword, followed by the function name and its typed parameter list.\n\nIf a function returns a value, put the return type after the arrow `->` next to the parameters. Inside, use the return statement `-> <expression>` to pass the result back!",
    code: `fn double(s32 number) -> s32 {
    -> number * 2
}

fn main() {
    s32 result <- double(21)
}`
  },
  {
    title: "Loops",
    content: "Need to repeat actions? Wasome keeps loops simple with just one keyword: `loop`!\n\nThere are no separate while or for keywords—just pass a condition in parentheses. The code repeats as long as it is true. Exit instantly at any time using `break`!",
    code: `fn main() {
    s32 i <- 0
    loop (i < 5) {
        i <- i + 1
    }
}`
  },
  {
    title: "Structs",
    content: "Bundle related data together into custom types using the `struct` keyword!\n\nCreate a new instance using the `new` keyword followed by field assignments in curly braces. Once created, you can access and modify fields instantly using dot `.` notation!",
    code: `struct Point {
    s32 x
    s32 y
}

fn main() {
    Point p <- new Point { x <- 10, y <- 20 }
    p.x <- p.x + 5
}`
  },
  {
    title: "Methods & Self",
    content: "Structs aren't just data bags—they can also have their own functions, called methods!\n\nMethods declared inside a struct automatically get a special, implicit parameter called `self`. Note that to call a method, you must enclose the instance in parentheses, like `(tracker).increment()`, otherwise it would be parsed as a field access!",
    code: `struct Counter {
    s32 count
    
    fn increment() {
        self.count <- self.count + 1
    }
}

fn main() {
    Counter tracker <- new Counter { count <- 10 }
    (tracker).increment()
}`
  },
  {
    title: "Generics & Enums",
    content: "Let's add some serious flexibility! Generics let structs and enums accept type parameters in square brackets, like `struct Box[T]`.\n\nMeanwhile, enums are tagged unions—each variant can carry different data payloads! When constructing a variant of a generic enum, you must provide the type parameter, like `Option[s32]::Some(value)`.",
    code: `enum Option[T] {
    Some(T)
    None
}

struct Box[T] {
    T item
}

fn main() {
    Option[s32] wrapped <- Option[s32]::Some(42)
    Box[s32] container <- new Box[s32] { item <- 100 }
}`
  },
  {
    title: "Modules",
    content: "As your program grows, split it into files and import them using the `import` statement!\n\nTo share a function or struct with other files, export it by prefixing its declaration with `pub`. You can then import it and use `as` to give the module a neat, custom alias!",
    code: `import "./math" as m

fn main() {
    // imported code:
    // pub fn double(s32 x) -> s32 { -> x * 2 }
    
    s32 doubled <- m.double(5)
}`
  }
];
