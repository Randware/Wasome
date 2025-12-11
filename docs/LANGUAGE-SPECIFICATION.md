# The Wasome Language specification

## Language features

### Datatypes

Wasome has a standard set of primitive data types.
Their internal representation and further implementation details are based on the [canonical ABI](https://component-model.bytecodealliance.org/advanced/canonical-abi.html).

The following are supported:

- `u8` - An unsigned 8-bit integer.
- `u16` - An unsigned 16-bit integer.
- `u32` - An unsigned 32-bit integer.
- `u64` - An unsigned 64-bit integer.
- `s8` - A signed 8-bit integer.
- `s16` - A signed 16-bit integer.
- `s32` - A signed 32-bit integer.
- `s64` - A signed 64-bit integer.
- `f32` - A 32-bit floating-point number (based on `IEEE 754 binary32`).
- `f64` - A 64-bit floating-point number (based on `IEEE 754 binary64`).
- `bool` - Either `true` or `false`.
- `char` - A single Unicode code point (source representation encoded in UTF-8).

Wasome does not have a built in array and string type. They have to be provided by the standard library.

---

### Expressions

Expressions are constructs that evaluate to a value in their surrounding context.
Some basic expressions are:

#### Directly specified values

These values are written directly in the source and evaluate to themselves. The following are supported:

1. **Integer literals** - formatted as a whole number and evaluate to type `s32`.
2. **Floating-point literals** - formatted as a decimal number and evaluate to type `f64`.
3. **Logical (boolean) literals** - `true` and `false`, type `bool`.

More complex expressions (function calls, member access, casts, etc.) are described later.

---

### Operators

Operators combine expressions into a single expression. Operands must be of appropriate and compatible types (valid conversions will be covered later).

#### Operator categories

- **Arithmetic:** `+`, `-`, `*`, `/`, `%`
- **Bitwise:** `<<`, `>>`, `|`, `&`, `^`, `~`
- **Logical:** `||`, `&&`, `!`
- **Comparison / equality:** `>`, `<`, `>=`, `<=`, `==`, `!=`

#### Notes on arithmetic operators

- Binary arithmetic operators expect two numeric operands of the same type.
- Unary minus (`-`) negates a numeric operand (equivalent to multiplying by `-1`).
- `/` performs integer division for integer operands. The result is truncated toward zero (the fractional part is discarded).
- `%` is defined only for integer operands and yields the remainder.

#### Other operator rules

- `!` negates a boolean.
- `==` and `!=` work across value types when meaningful (see type system rules for specifics, still TODO).

#### Operator precedence (high → low)

1. Unary: `-` (unary), `!`, `~`
2. Multiplicative: `*`, `/`, `%` (left-associative)
3. Additive: `+`, `-` (left-associative)
4. Shifts: `<<`, `>>` (left-associative)
5. Comparison: `>`, `<`, `>=`, `<=` (left-associative)
6. Equality: `==`, `!=` (left-associative)
7. Bitwise AND: `&` (left-associative)
8. Bitwise XOR: `^` (left-associative)
9. Bitwise OR: `|` (left-associative)
10. Logical AND: `&&` (left-associative)
11. Logical OR: `||` (left-associative)

---

### Statements

A program is a sequence of statements. Expressions can also be used as statements.

---

### Blocks

Blocks are delimited by `{` and `}` and push a new lexical scope.

#### Code block

A code block contains newline-separated statements and declarations. A code block itself is a statement.

---

### Functions

Function declaration syntax:

```
fn <function-name>(<parameter-list>) -> <return-type> <code block>
```

- `<function-name>` - identifier composed of alphanumeric characters and `_`. The first character cannot be a digit. Use `snake_case`.
- `<parameter-list>` - comma-separated list of `<type> <parameter-name>` pairs.
- `-> <return-type>` is optional. Omit both `->` and `<return-type>` for functions that do not return a value.

A function body is a code block. Parameters are bound in the function's code block scope.

Function call syntax:

```
<function-name>(<arg-list>)
```

- `<arg-list>` - comma-separated list of expressions (no types).

Returns:

- Use `-> <expression>` inside a function body to return a value.
- If a function declares a return type, every possible control path must produce a returned value.
- Functions without a declared return type may use `->` to perform an early return (no value is returned).
- Return instructions are statements.

---

### Variables

Declaration syntax:

```
<type> <variable-name> <- <expression>
```

- `<type>` - the declared type of the variable.
- `<variable-name>` - identifier composed of alphanumeric characters and `_`. The first character cannot be a digit. Use `snake_case`.
- `<expression>` - evaluated and stored in the variable.

Assignment syntax (after declaration):

```
<variable-name> <- <expression>
```

- Variables declared in a code block are scoped to that block and any nested blocks.
- Reading a variable is done by writing its name as an expression.
- Variables are mutable (assignment after declaration is allowed).

---

### If-statements

Syntax:

```
if (<logical-expression>) <code block> else <code block>
```

- `<logical-expression>` must evaluate to `bool`.
- `else <code block>` is optional.
- The `if` statement itself is a statement and can appear wherever statements are allowed.

---

### Structs

Declaration syntax:

```
struct <StructName> <struct-block>
```

- `<StructName>` - identifier (use `PascalCase`).
- `<struct-block>` - a block containing newline-separated field declarations and method declarations.

Field declaration form:

```
<field-type> <field-name>
```

Methods are regular functions declared inside the struct block. All methods have a special `self` variable in their scope, that is a mutable reference to the struct instance itself.

The `self` parameter does not have to be explicitly specified in the parameter list.

These methods are otherwise exactly the same as regular functions.

Methods and fields can be accessed using the `.` syntax on an instance of a struct.

Instantiation syntax:

```
new <StructName> <parameter-block>
```

- `<parameter-block>` - block with comma-separated `field-name <- <expression>` pairs.

Semantics:

- A `struct` type is its own type (named by the struct).
- Struct instances are passed by reference. Mutating a struct through any reference affects all references.

---

### Enums

Declaration syntax:

```
enum <EnumName> <enum-block>
```

- `<EnumName>` - identifier (use `PascalCase`).
- `<enum-block>` - block containing newline-separated variant declarations.

Variant syntax:

```
<VariantName>(<type-list>)
```

- `<VariantName>` - identifier (use `PascalCase`).
- `<type-list>` - comma-separated types (zero or more).

The `<type-list>`, along with the brackets, can be omitted in case a variant does not have any types associated with it.

Enums implement tagged-union semantics.

---

### Typecasting

Cast syntax:

```
<expression> as <type>
```

- Only allowed conversions are valid (see conversion table).
- The `as` expression evaluates to the value converted to `<type>` if the conversion is defined.

Valid conversions:

- Widening conversions (from a smaller to a larger integer or floating-point type, or from integer to floating-point) are allowed.
- Narrowing conversions (from a larger to a smaller type, or from floating-point to integer) are allowed.
- Signed and unsigned conversions are allowed.

If a conversion does not fit into the target type, the value will overflow or underflow.

This `as` syntax is an expression.

---

### Generics

Structs support type parameters:

```
struct <StructName><T, U, ...> <struct-block>
```

- Type parameter identifiers use `PascalCase`.
- When instantiating a generic struct, provide concrete types:

```
new <StructName><ConcreteType, ...> <parameter-block>
```

Type parameters become part of the instantiated struct's type.

---

### Loops

Loops execute code repeatedly. Wasome uses the `loop` keyword and supports three forms.

The `break` keyword can be used in any type of loop to exit the innermost loop at any time.
Wasome does not have a `continue` keyword.

#### Infinite loop

Syntax:

```
loop <code block>
```

- The `<code block>` executes repeatedly without an intrinsic termination condition.
- Using `break` is required to exit the loop.

#### While loop

Syntax:

```
loop (<expression>) <code block>
```

- `<expression>` must evaluate to `bool`.
- The `<code block>` executes while `<expression>` is `true`.

#### For-style loop

Syntax:

```
loop (<init-statement>; <expression>; <post-statement>) <code block>
```

- `<init-statement>` runs once before the first iteration.
- `<expression>` is evaluated before each iteration and must yield `bool`. The loop runs while it is `true`.
- `<post-statement>` runs after each iteration.

---

### Multi-File

Use `import` to access `public` structs, enums, and functions from other files.

Syntax:

```
import "<path>"
```

`<path>` is a `/`-separated string consisting of:

1. **Root** - where lookup begins:
   - `~/` - current file location
   - `<project-name>/` - project-relative
   - `std/` - standard-library relative

2. **Directories** - zero or more directory segments.

3. **File** - file name without extension.

4. **Element name** - the identifier of the struct, enum, or function to import.

After import, the element is usable like any local element.
