export const steps = [
  {
    title: "Welcome to Wasome",
    content: "Hi there! I'm Bit, your guide to the Wasome programming language. Wasome is designed to be a simple yet powerful language that compiles directly to WebAssembly.\n\nLet's start with the basics: The `main` function. This is the entry point of every program.",
    code: `fn main() {
    // This is a comment
    s32 result <- 42
    -> result
}`
  },
  {
    title: "Variables & Types",
    content: "In Wasome, we like to be explicit about our types. `s32` is a signed 32-bit integer. We use the arrow `<-` to assign values.\n\nVariables are mutable by default, so you can change them later!",
    code: `fn main() {
    s32 count <- 0
    count <- count + 1
    
    f64 pi <- 3.14159
    bool is_awesome <- true
    
    -> count
}`
  },
  {
    title: "Functions",
    content: "Functions are declared with `fn`. They can take arguments and return values. The return type comes after the arrow `->`.\n\nTo return a value, use the `->` statement.",
    code: `fn add(s32 a, s32 b) -> s32 {
    -> a + b
}

fn main() {
    s32 sum <- add(10, 20)
    -> sum
}`
  },
  {
    title: "Control Flow",
    content: "We keep it simple. `if` statements work just like you'd expect. For loops, we just have one keyword: `loop`. It's versatile!",
    code: `fn main() {
    s32 i <- 0
    
    loop (i < 5) {
        i <- i + 1
    }
    
    if (i == 5) {
        // Success!
    }
    
    -> i
}`
  },
  {
    title: "Structs",
    content: "Group data together with `struct`. You can create new instances using the `new` keyword and a simplified initialization block.",
    code: `struct Point {
    s32 x
    s32 y
}

fn main() {
    Point p <- new Point { x <- 10, y <- 20 }
    
    p.x <- p.x * 2
    
    -> p.x
}`
  }
];
