use crate::format;

// Compare output while keeping indentation strict.
fn assert_fmt(input: &str, expected: &str) {
    let actual = format(input.to_string());
    let actual_trimmed = actual.trim_end();
    let expected_trimmed = expected.trim_end();
    assert_eq!(
        actual_trimmed, expected_trimmed,
        "\n--- INPUT ---\n{input}\n--- EXPECTED ---\n{expected_trimmed}\n--- ACTUAL ---\n{actual_trimmed}\n"
    );
}

// Spec examples.

#[test]
fn ex01_basic_assignment() {
    assert_fmt("s32    my_var<-10+20*3", "s32 my_var <- 10 + 20 * 3");
}

#[test]
fn ex02_unary_vs_binary_minus() {
    assert_fmt("s32 result<--5--10", "s32 result <- -5 - -10");
}

#[test]
fn ex03_control_flow() {
    assert_fmt(
        "if(x<=10){->true}else{->false}",
        "\
if (x <= 10) {
    -> true
} else {
    -> false
}",
    );
}

#[test]
fn ex04_generics_paths_methods() {
    assert_fmt(
        "point.x<-List[ s32 ]::new( )",
        "point.x <- List[s32]::new()",
    );
}

#[test]
fn ex05_struct_init() {
    assert_fmt(
        "Point p<-new Point{x<-10,y<-20}",
        "Point p <- new Point { x <- 10, y <- 20 }",
    );
}

#[test]
fn ex07_type_casting() {
    assert_fmt("u64 val<-1 as u32 as u64", "u64 val <- 1 as u32 as u64");
}

#[test]
fn ex08_comment_preservation() {
    assert_fmt(
        "s32 x<-10// initialize x\ns32 y<-20",
        "\
s32 x <- 10 // initialize x
s32 y <- 20",
    );
}

#[test]
fn ex09_newline_sanitization() {
    assert_fmt(
        "fn a() -> u8 { -> 1 }\n\n\n\nfn b() -> u8 { -> 2 }",
        "\
fn a() -> u8 {
    -> 1
}

fn b() -> u8 {
    -> 2
}",
    );
}

#[test]
fn ex10_nested_indentation() {
    assert_fmt(
        "loop(true){if(x==0){loop(false){->0}}}",
        "\
loop (true) {
    if (x == 0) {
        loop (false) {
            -> 0
        }
    }
}",
    );
}

#[test]
fn ex11_complex_binary_math() {
    assert_fmt("u32 math<-10*5/2%3+1", "u32 math <- 10 * 5 / 2 % 3 + 1");
}

#[test]
fn ex12_boolean_logic() {
    assert_fmt(
        "bool check<-a==b&&c!=d||e>=f",
        "bool check <- a == b && c != d || e >= f",
    );
}

#[test]
fn ex13_bitwise() {
    assert_fmt("u8 mask<-10&5|15<<2", "u8 mask <- 10 & 5 | 15 << 2");
}

#[test]
fn ex14_unary_not() {
    assert_fmt("bool inv<-! true", "bool inv <- !true");
}

#[test]
fn ex15_string_integrity() {
    assert_fmt(
        "String msg<-\"hello     world\"",
        "String msg <- \"hello     world\"",
    );
}

#[test]
fn ex16_char_integrity() {
    assert_fmt("char whitespace<-' '", "char whitespace <- ' '");
}

#[test]
fn ex17_empty_struct() {
    assert_fmt(
        "pub struct Data{}",
        "\
pub struct Data {
}",
    );
}

#[test]
fn ex18_enum_variants() {
    assert_fmt(
        "enum Color{Red,Green,Blue}",
        "\
enum Color {
    Red,
    Green,
    Blue
}",
    );
}

#[test]
fn ex19_extern_fn() {
    assert_fmt(
        "extern fn print_msg(String msg)",
        "extern fn print_msg(String msg)",
    );
}

#[test]
fn ex20_import() {
    assert_fmt("import io::console", "import io::console");
}

#[test]
fn ex21_nested_generics() {
    assert_fmt(
        "Map[String,List[s32]] data<-new Map",
        "Map[String, List[s32]] data <- new Map",
    );
}

#[test]
fn ex22_parens_in_math() {
    assert_fmt("s32 val<-( 1+2 )*3", "s32 val <- (1 + 2) * 3");
}

#[test]
fn ex23_float_assignment() {
    assert_fmt("f64 pi<-3.1415926", "f64 pi <- 3.1415926");
}

#[test]
fn ex24_if_else_if_else() {
    assert_fmt(
        "if(x){->1}else if(y){->2}else{->3}",
        "\
if (x) {
    -> 1
} else if (y) {
    -> 2
} else {
    -> 3
}",
    );
}

#[test]
fn ex25_self_methods() {
    assert_fmt(
        "fn calculate(Self self)->Self{->self}",
        "\
fn calculate(Self self) -> Self {
    -> self
}",
    );
}

#[test]
fn ex26_standalone_return() {
    assert_fmt(
        "if(done){->}",
        "\
if (done) {
    ->
}",
    );
}

#[test]
fn ex27_double_unary() {
    assert_fmt("s32 result<--5*-5", "s32 result <- -5 * -5");
}

#[test]
fn ex28_empty_params() {
    assert_fmt("fn run( )->bool", "fn run() -> bool");
}

#[test]
fn ex29_chaining() {
    assert_fmt("app.build().run()", "app.build().run()");
}

// Extra edge cases.

#[test]
fn double_unary_not() {
    assert_fmt(
        "fn unary(bool a)->bool{->!!a}",
        "\
fn unary(bool a) -> bool {
    -> !!a
}",
    );
}

#[test]
fn enum_newline_separated() {
    // Variants split by newlines.
    assert_fmt(
        "enum Weekday{\nMonday\nTuesday\nWednesday\n}",
        "\
enum Weekday {
    Monday
    Tuesday
    Wednesday
}",
    );
}

#[test]
fn struct_with_fields() {
    assert_fmt(
        "struct Point{\ns32 x\ns32 y\n}",
        "\
struct Point {
    s32 x
    s32 y
}",
    );
}

#[test]
fn for_loop_semicolons() {
    assert_fmt(
        "loop(s32 i<-0;i<n;i<-i+1){sum<-sum+i}",
        "\
loop (s32 i <- 0; i < n; i <- i + 1) {
    sum <- sum + i
}",
    );
}

#[test]
fn method_in_struct() {
    assert_fmt(
        "struct Wrapper{\nfn get()->s32{->self.x}\n}",
        "\
struct Wrapper {
    fn get() -> s32 {
        -> self.x
    }
}",
    );
}

#[test]
fn generic_struct_def() {
    assert_fmt(
        "struct Box[T]{\nT value\n}",
        "\
struct Box[T] {
    T value
}",
    );
}

#[test]
fn collapse_blank_lines() {
    assert_fmt(
        "s32 a<-1\n\n\n\n\ns32 b<-2",
        "\
s32 a <- 1

s32 b <- 2",
    );
}

#[test]
fn pub_extern_fn() {
    assert_fmt("pub extern fn test()->u64", "pub extern fn test() -> u64");
}

#[test]
fn else_after_newlines() {
    // Keep `} else {` on one line.
    assert_fmt(
        "if(x){->1}\n\n\nelse{->2}",
        "\
if (x) {
    -> 1
} else {
    -> 2
}",
    );
}

#[test]
fn infinite_loop_empty() {
    assert_fmt(
        "loop{}",
        "\
loop {
}",
    );
}

#[test]
fn nested_struct_init() {
    assert_fmt(
        "Point p<-new Point{x<-10,y<-20}",
        "Point p <- new Point { x <- 10, y <- 20 }",
    );
}

#[test]
fn comment_at_start_of_line() {
    assert_fmt(
        "// This is a comment\ns32 x<-10",
        "\
// This is a comment
s32 x <- 10",
    );
}

#[test]
fn path_separator_chaining() {
    assert_fmt("Option[U]::None", "Option[U]::None");
}

#[test]
fn idempotent_fibonacci() {
    let formatted = "\
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
";
    let result = format(formatted.to_string());
    assert_eq!(result, formatted, "Formatting should be idempotent");
}

#[test]
fn idempotent_if() {
    let formatted = "\
fn main() {
    char wasome <- showcase_if_conditionals()
}

fn showcase_if_conditionals() -> char {
    bool wasome_is_awesome <- true

    if (wasome_is_awesome) {
        -> '✨'
    } else {
        -> '🤥'
    }
    // Keep explicit final return.
    -> ' '
}
";
    let result = format(formatted.to_string());
    assert_eq!(result, formatted, "Formatting should be idempotent");
}

// Regression tests.

#[test]
fn brace_on_own_line_pulled_back() {
    // Pull opening brace back.
    assert_fmt(
        "fn main()\n{\n    s32 num <- 10\n}",
        "\
fn main() {
    s32 num <- 10
}",
    );
}

#[test]
fn brace_on_own_line_with_blanks() {
    // Pull brace back after blanks.
    assert_fmt(
        "fn main()\n\n\n{\n    s32 x <- 1\n}",
        "\
fn main() {
    s32 x <- 1
}",
    );
}

#[test]
fn unary_minus_at_line_start() {
    // Treat line-start `-` as unary.
    assert_fmt(
        "fn main() {\n    s32 num <- 10\n    -10\n}",
        "\
fn main() {
    s32 num <- 10
    -10
}",
    );
}

#[test]
fn unary_not_at_line_start() {
    // Keep line-start unary `!`.
    assert_fmt(
        "fn main() {\n    bool x <- true\n    !x\n}",
        "\
fn main() {
    bool x <- true
    !x
}",
    );
}

#[test]
fn if_brace_on_own_line() {
    assert_fmt(
        "if (x)\n{\n    -> 1\n}",
        "\
if (x) {
    -> 1
}",
    );
}

#[test]
fn else_brace_on_own_line() {
    assert_fmt(
        "if (x) {\n    -> 1\n}\nelse\n{\n    -> 2\n}",
        "\
if (x) {
    -> 1
} else {
    -> 2
}",
    );
}

#[test]
fn loop_brace_on_own_line() {
    assert_fmt(
        "loop (true)\n{\n    s32 x <- 1\n}",
        "\
loop (true) {
    s32 x <- 1
}",
    );
}

#[test]
fn struct_brace_on_own_line() {
    assert_fmt(
        "struct Point\n{\n    s32 x\n    s32 y\n}",
        "\
struct Point {
    s32 x
    s32 y
}",
    );
}

#[test]
fn blank_line_between_top_level_items() {
    // Insert blank line between items.
    assert_fmt(
        "struct Point {\n    s32 x\n    s32 y\n}\nfn main() {\n    s32 x <- 10\n}",
        "\
struct Point {
    s32 x
    s32 y
}

fn main() {
    s32 x <- 10
}",
    );
}

#[test]
fn blank_line_between_struct_and_fn_with_methods() {
    // Reported struct-to-fn spacing case.
    assert_fmt(
        "struct Point {\n    s32 x\n    s32 y\n\n    pub fn get_x() -> s32 {\n        -> self.x\n    }\n}\nfn main() {\n    Point point <- new Point { x <- 10, y <- 20 }\n}",
        "\
struct Point {
    s32 x
    s32 y

    pub fn get_x() -> s32 {
        -> self.x
    }
}

fn main() {
    Point point <- new Point { x <- 10, y <- 20 }
}",
    );
}

#[test]
fn blank_line_not_doubled() {
    // Do not duplicate blank lines.
    assert_fmt(
        "struct A {\n}\n\nfn b() {\n}",
        "\
struct A {
}

fn b() {
}",
    );
}

#[test]
fn blank_line_before_method_in_struct() {
    // Add blank line before first method.
    assert_fmt(
        "struct Point {\n    s32 x\n    s32 y\n    pub fn get_x() -> s32 {\n        -> self.x\n    }\n}",
        "\
struct Point {
    s32 x
    s32 y

    pub fn get_x() -> s32 {
        -> self.x
    }
}",
    );
}

#[test]
fn no_blank_line_before_first_method_in_struct() {
    // First method needs no extra blank.
    assert_fmt(
        "struct Ops {\n    fn run() -> s32 {\n        -> 1\n    }\n}",
        "\
struct Ops {
    fn run() -> s32 {
        -> 1
    }
}",
    );
}

#[test]
fn blank_line_between_methods_in_struct() {
    assert_fmt(
        "struct S {\n    fn a() -> s32 {\n        -> 1\n    }\n    fn b() -> s32 {\n        -> 2\n    }\n}",
        "\
struct S {
    fn a() -> s32 {
        -> 1
    }

    fn b() -> s32 {
        -> 2
    }
}",
    );
}

#[test]
fn strip_blank_lines_between_struct_fields() {
    // Strip field gaps, keep method gap.
    assert_fmt(
        "struct Point {\n\n    s32 x\n\n    s32 y\n\n    pub fn get_x() -> s32 {\n        -> self.x\n    }\n}",
        "\
struct Point {
    s32 x
    s32 y

    pub fn get_x() -> s32 {
        -> self.x
    }
}",
    );
}

#[test]
fn strip_blank_lines_between_enum_variants() {
    assert_fmt(
        "enum Weekday {\n\n    Monday\n    Tuesday\n\n\n    Wednesday\n    Thursday\n    Friday\n    Saturday\n    Sunday\n}",
        "\
enum Weekday {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
}",
    );
}

// Newline cleanup rules.

#[test]
fn strip_blank_lines_at_start_of_file() {
    assert_fmt(
        "\n\n\nfn main() {\n    s32 x <- 1\n}",
        "\
fn main() {
    s32 x <- 1
}",
    );
}

#[test]
fn strip_blank_lines_after_open_scope() {
    assert_fmt(
        "fn main() {\n\n\n    s32 x <- 1\n}",
        "\
fn main() {
    s32 x <- 1
}",
    );
}

#[test]
fn strip_blank_lines_before_close_scope() {
    assert_fmt(
        "fn main() {\n    s32 x <- 1\n\n\n}",
        "\
fn main() {
    s32 x <- 1
}",
    );
}

#[test]
fn preserve_single_blank_line_in_body() {
    // Keep intentional group spacing.
    assert_fmt(
        "fn main() {\n    s32 x <- 1\n\n    s32 y <- 2\n}",
        "\
fn main() {
    s32 x <- 1

    s32 y <- 2
}",
    );
}

#[test]
fn collapse_multiple_blank_lines_in_body() {
    // Collapse many blank lines to one.
    assert_fmt(
        "fn main() {\n    s32 x <- 1\n\n\n\n    s32 y <- 2\n}",
        "\
fn main() {
    s32 x <- 1

    s32 y <- 2
}",
    );
}

#[test]
fn messy_fibonacci_full() {
    assert_fmt(
        "fn      fibonacci( u8    n)->u64{\n\n\n  u64 curr<-1 as u32    as u64 // init curr\nu64 prev<-0 as   u32 as     u64\n\n    loop(n>0  ){\nu64 temp<-curr\n            curr<-curr+prev\n\n\n      prev<-temp\n  n<-n-1 as u32 as u16   as u8\n }\n        ->curr\n}",
        "\
fn fibonacci(u8 n) -> u64 {
    u64 curr <- 1 as u32 as u64 // init curr
    u64 prev <- 0 as u32 as u64

    loop (n > 0) {
        u64 temp <- curr
        curr <- curr + prev

        prev <- temp
        n <- n - 1 as u32 as u16 as u8
    }
    -> curr
}",
    );
}

#[test]
fn parens_empty_with_newlines() {
    assert_fmt(
        "fn main(\n\n\n) {\n}",
        "\
fn main() {
}",
    );
}

#[test]
fn inline_comment_moved_to_own_line() {
    assert_fmt(
        "fn main(){s32 x<-1//note\n->x}",
        "\
fn main() {
    s32 x <- 1 //note
    -> x
}",
    );
}
