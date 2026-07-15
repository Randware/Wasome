export const steps = [
  {
    title: "Welcome to Wasome",
    content: "Hi there! I'm Bit, your guide to Wasome! Wasome is a simple, fun language designed to compile straight to WebAssembly.\n\nOur journey starts with the `main` function, which is the entry point of every program. Notice the curly brackets `{}` enclosing our block, and `//` for comments. They're just for us humans!",
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
    content: "Sometimes numbers want to wear a different hat! In Wasome, convert one numerical type to another using the `as` keyword.\n\nYou can cast between any two numeric types directly—no need for an intermediate size (like `f64` to `s32` in one go)! Narrowing chops off decimals and can overflow.\n\nYou can also convert `bool` to/from `u8`/`s8` (0 is false, 1 is true; when casting into `bool` only the least significant bit matters, so casting values greater than 1 or less than 0 into a `bool` is discouraged). And a `char` can be cast to `u32` using UTF-8, padding shorter chars with zeros.",
    code: `fn main() {
    f64 precise_pi <- 3.14159
    s32 rough_pi <- precise_pi as s32
    f64 normal_pi <- rough_pi as f64

    u8 flag <- 1
    bool active <- flag as bool
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
    content: "Need to repeat actions? Wasome keeps loops simple with just one keyword: `loop`!\n\nThere are no separate while or for keywords, instead, just pass a condition in parentheses. The code repeats as long as it is true. Exit instantly at any time using `break`!",
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
    content: "Structs aren't just data bags, because they can also have their own functions, called methods!\n\nMethods declared inside a struct automatically get a special, implicit parameter called `self`. Note that to call a method, you must enclose the instance in parentheses, like `(tracker).increment()`, otherwise it would be parsed as a field access!",
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
    content: "Let's add some serious flexibility! Generics let structs and enums accept type parameters in square brackets, like `struct Box[T]`.\n\nMeanwhile, enums are tagged unions, where each variant can carry different data payloads! When constructing a variant of a generic enum, you must provide the type parameter, like `Option[s32]::Some(value)`.",
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
  },
  {
    title: "Standard Library: Strings",
    content: "Welcome to the standard library! Wasome provides a mutable, heap-allocated `String` type.\n\nCreate a new string using `std.string_new()`. You can push characters using `.push_char()`, append other Strings with `.push_string()`, or format integers into strings using `.push_s32()`! Note that standard library methods require parenthesizing the instance when calling them, like `(my_string).push_char('A')`.",
    code: `import "std" as std

fn main() {
    std.String greeting <- std.string_new()
    (greeting).push_char('H')
    (greeting).push_char('i')
}`
  },
  {
    title: "Standard Library: Printing",
    content: "Let's make our output visible! The standard library provides basic printing functions.\n\nUse `std.print_char()` to output a single ASCII character, or `std.print_string()` to output an entire `std.String` buffer. Combining `std.String` building with `std.print_string()` is the standard way to print numbers, variables, and complex text logs to the terminal!",
    code: `import "std" as std

fn main() {
    // Print a single character
    std.print_char('W')
    
    // Print a dynamic string
    std.String message <- std.string_new()
    (message).push_char('o')
    (message).push_char('r')
    (message).push_char('l')
    (message).push_char('d')
    (message).push_char('!')
    std.print_string(message)
}`
  },
  {
    title: "Standard Library: Vectors",
    content: "Dynamic arrays in Wasome are managed by the `Vec` container!\n\nDeclare a vector with a generic type parameter in square brackets, like `std.Vec[s32]`, and initialize it using `std.vec_new[s32]()`. Add items to the end with `.push()`, retrieve them by index using `.get(index)`, or inspect the size with `.len()`!",
    code: `import "std" as std

fn main() {
    // Create a vector of integers (Vec)
    std.Vec[s32] numbers <- std.vec_new[s32]()
    (numbers).push(10)
    (numbers).push(20)
    
    // Retrieve the first element (10)
    s32 first <- (numbers).get(0 as u32)
}`
  },
  {
    title: "Beyond the Stars",
    content: "Congratulations! You have completed the Wasome language tour!\n\nWe have covered everything from basic variables and functions to structs, generics, modules, and the standard library. You now have all the tools you need to write and compile code for WebAssembly.\n\nWhat's next?\n* Head over to the Docs page to explore the full language specification.\n* Check out the Examples tab to see larger solvers (like Fibonacci or modular arithmetic).\n* Open the Playground to start writing your own programs.\n\nIf you find a bug, have a suggestion, or want to contribute to the compiler, come say hello and open an issue on our GitHub repository. We are always happy to collaborate!\n\nThank you for taking the time to explore Wasome. Happy coding!",
    code: `import "std" as std

fn main() {
    // Thank you for taking the tour!
    // The universe is waiting for your code.
    std.print_char('H')
    std.print_char('A')
    std.print_char('V')
    std.print_char('E')
    std.print_char(' ')
    std.print_char('F')
    std.print_char('U')
    std.print_char('N')
    std.print_char('!')
}`
  }
];
