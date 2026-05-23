<script>
  import { onMount, onDestroy } from "svelte";
  import { highlight } from "../lib/highlighter";
  import { 
    ChevronDown, 
    ChevronRight, 
    BookOpen, 
    Layers, 
    Terminal, 
    Shuffle, 
    GitFork, 
    Code2, 
    Brackets, 
    FolderTree, 
    RefreshCw 
  } from "lucide-svelte";

  // Detailed multi-level section configuration
  const sections = [
    {
      id: "datatypes",
      title: "1. Datatypes",
      icon: Layers,
      subsections: [
        { id: "primitives", title: "1.1 Primitives" },
        { id: "memory-representation", title: "1.2 Memory Representation" },
        { id: "strings-arrays", title: "1.3 Strings & Arrays" }
      ]
    },
    {
      id: "variables-mutability",
      title: "2. Variables & Mutability",
      icon: Terminal,
      subsections: [
        { id: "declarations", title: "2.1 Declarations" },
        { id: "scoping", title: "2.2 Scoping" },
        { id: "shadowing", title: "2.3 Shadowing" }
      ]
    },
    {
      id: "operators",
      title: "3. Operators",
      icon: Shuffle,
      subsections: [
        { id: "categories", title: "3.1 Categories" },
        { id: "precedence", title: "3.2 Precedence" }
      ]
    },
    {
      id: "control-flow",
      title: "4. Control Flow",
      icon: GitFork,
      subsections: [
        { id: "conditionals", title: "4.1 Conditionals" },
        { id: "loops", title: "4.2 Loops" },
        { id: "pattern-matching", title: "4.3 Pattern Matching (if let)" }
      ]
    },
    {
      id: "functions",
      title: "5. Functions",
      icon: Code2,
      subsections: [
        { id: "structure", title: "5.1 Structure" },
        { id: "extern-functions", title: "5.2 Extern Functions" },
        { id: "wasm-export-suffixes", title: "5.3 WASM Export Suffixes" }
      ]
    },
    {
      id: "structs",
      title: "6. Structs",
      icon: Brackets,
      subsections: [
        { id: "declaring-instantiating", title: "6.1 Declaring & Instantiating" },
        { id: "generics", title: "6.2 Generics [T]" },
        { id: "methods-self", title: "6.3 Methods & self" },
        { id: "lifecycle-predrop", title: "6.4 Lifecycle & predrop" }
      ]
    },
    {
      id: "enums",
      title: "7. Enums",
      icon: FolderTree,
      subsections: [
        { id: "tagged-unions", title: "7.1 Tagged Unions" },
        { id: "option-result", title: "7.2 Option[T] and Result[T, E]" }
      ]
    },
    {
      id: "imports-modules",
      title: "8. Imports & Modules",
      icon: RefreshCw,
      subsections: [
        { id: "namespaces", title: "8.1 Namespaces" },
        { id: "aliasing", title: "8.2 Aliasing" }
      ]
    }
  ];

  // Svelte 5 state management
  let activeSection = $state("datatypes");

  // Map each subtopic ID to its parent category ID
  const parentMap = {};
  sections.forEach(sec => {
    parentMap[sec.id] = sec.id;
    if (sec.subsections) {
      sec.subsections.forEach(sub => {
        parentMap[sub.id] = sec.id;
      });
    }
  });

  // Reactive derived expansion map: only the active category is expanded, auto-collapsing all others
  let expandedSections = $derived.by(() => {
    const parentId = parentMap[activeSection];
    const result = {};
    sections.forEach(sec => {
      result[sec.id] = (parentId === sec.id);
    });
    return result;
  });

  function scrollTo(id, e) {
    if (e) e.preventDefault();
    activeSection = id;
    
    const el = document.getElementById(id);
    if (el) {
      const y = el.getBoundingClientRect().top + window.pageYOffset - 100;
      window.scrollTo({ top: y, behavior: "smooth" });
    }
  }

  let sectionObserver;

  onMount(() => {
    // 2. Observe each section & subsection to update activeSection (Rule 1)
    sectionObserver = new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        if (entry.isIntersecting) {
          activeSection = entry.target.id;
        }
      });
    }, {
      rootMargin: "-120px 0px -50% 0px", // triggers when elements hit top/middle viewport
      threshold: 0
    });

    sections.forEach(sec => {
      const el = document.getElementById(sec.id);
      if (el) sectionObserver.observe(el);
      sec.subsections.forEach(sub => {
        const subEl = document.getElementById(sub.id);
        if (subEl) sectionObserver.observe(subEl);
      });
    });
  });

  onDestroy(() => {
    if (sectionObserver) sectionObserver.disconnect();
  });
</script>

<section class="container page-content page-animate">
  <!-- Interactive Left-hand Sidebar (Rule 2: svelte 5 event handlers) -->
  <aside class="sidebar">
    <div class="sidebar-inner">
      <h4>Wasome Docs</h4>
      <nav class="sidebar-nav">
        {#each sections as sec}
          <div class="sidebar-group" class:expanded={expandedSections[sec.id]}>
            <div class="sidebar-item-header" class:active={activeSection === sec.id || parentMap[activeSection] === sec.id}>
              <a href="#{sec.id}" onclick={(e) => scrollTo(sec.id, e)} class="sidebar-link">
                <span>{sec.title.substring(3)}</span>
              </a>
            </div>
            
            {#if expandedSections[sec.id]}
              <div class="sidebar-subsections">
                {#each sec.subsections as sub}
                  <a href="#{sub.id}" onclick={(e) => scrollTo(sub.id, e)} class="sidebar-sublink" class:active={activeSection === sub.id}>
                    {sub.title.substring(4)}
                  </a>
                {/each}
              </div>
            {/if}
          </div>
        {/each}
      </nav>
    </div>
  </aside>

  <!-- Main Content Area -->
  <article class="docs-content">
    <h1>Documentation</h1>
    <p class="intro">
      This is the official reference manual for the Wasome programming language. Wasome is a statically-typed, low-level systems programming language designed to compile directly to efficient WebAssembly linear representation.
    </p>

    <!-- 1. Datatypes -->
    <div id="datatypes" class="doc-section">
      <h2>1. Datatypes</h2>
      <p>
        Wasome is a statically-typed language. The type system comprises primitive types and user-defined structural types. The internal representation and byte alignment of all data types conform to the WebAssembly Canonical Application Binary Interface (ABI) specification.
      </p>

      <div id="primitives" class="doc-subsection">
        <h3>1.1 Primitives</h3>
        <p>
          Primitive data types are predefined by the language. Wasome supports the following numeric, boolean, and character primitives:
        </p>

        <div class="table-wrapper">
          <table>
            <thead>
              <tr>
                <th>Type</th>
                <th>Description</th>
                <th>Bit Width</th>
                <th>WASM Representation</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td><code>u8</code></td>
                <td>Unsigned 8-bit integer</td>
                <td>8</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>u16</code></td>
                <td>Unsigned 16-bit integer</td>
                <td>16</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>u32</code></td>
                <td>Unsigned 32-bit integer</td>
                <td>32</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>u64</code></td>
                <td>Unsigned 64-bit integer</td>
                <td>64</td>
                <td><code>i64</code></td>
              </tr>
              <tr>
                <td><code>s8</code></td>
                <td>Signed 8-bit integer (two's complement)</td>
                <td>8</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>s16</code></td>
                <td>Signed 16-bit integer (two's complement)</td>
                <td>16</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>s32</code></td>
                <td>Signed 32-bit integer (two's complement)</td>
                <td>32</td>
                <td><code>i32</code></td>
              </tr>
              <tr>
                <td><code>s64</code></td>
                <td>Signed 64-bit integer (two's complement)</td>
                <td>64</td>
                <td><code>i64</code></td>
              </tr>
              <tr>
                <td><code>f32</code></td>
                <td>32-bit floating-point number (IEEE 754 binary32)</td>
                <td>32</td>
                <td><code>f32</code></td>
              </tr>
              <tr>
                <td><code>f64</code></td>
                <td>64-bit floating-point number (IEEE 754 binary64)</td>
                <td>64</td>
                <td><code>f64</code></td>
              </tr>
              <tr>
                <td><code>bool</code></td>
                <td>Logical boolean values (<code>true</code> or <code>false</code>)</td>
                <td>8</td>
                <td><code>i32</code> (0 or 1)</td>
              </tr>
              <tr>
                <td><code>char</code></td>
                <td>Unicode scalar value (encoded in UTF-8 in source text)</td>
                <td>32</td>
                <td><code>i32</code></td>
              </tr>
            </tbody>
          </table>
        </div>
        <p>
          Character literals represent single Unicode scalar values enclosed in single quotes (e.g., <code>'a'</code>). Integer literals represent decimal whole numbers. Floating-point literals represent decimal fractional numbers. Alternate numeric bases (hexadecimal, octal, or binary) and digit separators are not supported.
        </p>
      </div>

      <div id="memory-representation" class="doc-subsection">
        <h3>1.2 Memory Representation</h3>
        <p>
          Memory representation of data types is governed by the WebAssembly Canonical ABI. Primitive types are stored inline and passed by value. User-defined structural types (structs and enums) are reference-typed, allocated on the WebAssembly linear heap, and passed by reference.
        </p>
        <p>
          Heap-allocated types employ Automatic Reference Counting (ARC) to manage their lifecycles. The compiler automatically generates and injects reference increment (retain) and decrement (release) instructions. Memory reclamation is executed immediately when the reference count of an instance reaches zero.
        </p>
      </div>

      <div id="strings-arrays" class="doc-subsection">
        <h3>1.3 Strings & Arrays</h3>
        <p>
          Wasome does not provide built-in string or array datatypes at the language level.
        </p>
        <p>
          Strings and dynamic arrays must be provided by the standard library (<code>std/string</code> and <code>std/array</code>). These types are implemented as reference-counted structures residing in WebAssembly linear memory, relying on ARC for memory management of their underlying byte storage.
        </p>
      </div>
    </div>

    <!-- 2. Variables & Mutability -->
    <div id="variables-mutability" class="doc-section">
      <h2>2. Variables & Mutability</h2>
      <p>
        Variables are named storage locations that hold values of a statically declared type.
      </p>

      <div id="declarations" class="doc-subsection">
        <h3>2.1 Declarations</h3>
        <p>
          A variable declaration specifies the variable type, identifier, and an initial value assigned via the bind arrow operator (<code>&lt;-</code>).
        </p>
        <pre><code>{@html highlight(`<type> <variable-name> <- <expression>`, "wasome")}</code></pre>
        <p>
          Type declarations must be explicit; type inference is not supported. All variables declared in Wasome are mutable. Re-assignment is executed using the bind arrow operator:
        </p>
        <pre><code>{@html highlight(`s32 value <- 42
value <- 100`, "wasome")}</code></pre>
        <p>
          Declarations are private to their containing file unless prefixed with the <code>pub</code> keyword. Struct fields default to private unless explicitly declared public.
        </p>
      </div>

      <div id="scoping" class="doc-subsection">
        <h3>2.2 Scoping</h3>
        <p>
          Lexical blocks delimited by curly braces (<code>&#123;</code> and <code>&#125;</code>) define scoping boundaries. Variables are accessible only within the block in which they are declared and any nested blocks.
        </p>
        <p>
          Upon exiting a lexical block, the scope is popped and the compiler automatically inserts release instructions for all active reference-counted heap allocations declared within that scope.
        </p>
      </div>

      <div id="shadowing" class="doc-subsection">
        <h3>2.3 Shadowing</h3>
        <p>
          Variable shadowing is permitted. Declaring a variable in an inner lexical block with the same identifier as a variable in an outer block masks the outer variable. The outer variable remains inaccessible until the inner block terminates.
        </p>
        <pre><code>{@html highlight(`s32 scope_val <- 10
{
    s32 scope_val <- 20
    // scope_val evaluates to 20
}
// scope_val evaluates to 10`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 3. Operators -->
    <div id="operators" class="doc-section">
      <h2>3. Operators</h2>
      <p>
        Operators construct expressions by combining operands. Operands must be of compatible types.
      </p>

      <div id="categories" class="doc-subsection">
        <h3>3.1 Categories</h3>
        <p>
          Wasome supports arithmetic, bitwise, logical, and comparison/equality operators.
        </p>
        <div class="operator-compact-container">
          <div class="operator-compact-row">
            <div class="operator-compact-header">Arithmetic</div>
            <div class="operator-compact-body">
              <div class="operator-items">
                <span class="operator-item"><code>+</code> Addition</span>
                <span class="operator-item"><code>-</code> Subtraction / Negation</span>
                <span class="operator-item"><code>*</code> Multiplication</span>
                <span class="operator-item"><code>/</code> Division</span>
                <span class="operator-item"><code>%</code> Modulo</span>
              </div>
              <p class="operator-note">Binary arithmetic operators require two operands of identical numeric types. Division on integers truncates toward zero. Modulo (%) is defined only for integer types.</p>
            </div>
          </div>

          <div class="operator-compact-row">
            <div class="operator-compact-header">Bitwise</div>
            <div class="operator-compact-body">
              <div class="operator-items">
                <span class="operator-item"><code>&lt;&lt;</code> Left Shift</span>
                <span class="operator-item"><code>&gt;&gt;</code> Right Shift</span>
                <span class="operator-item"><code>|</code> Bitwise OR</span>
                <span class="operator-item"><code>&amp;</code> Bitwise AND</span>
                <span class="operator-item"><code>^</code> Bitwise XOR</span>
                <span class="operator-item"><code>~</code> Bitwise NOT</span>
              </div>
              <p class="operator-note">Shifting an unsigned integer performs a logical shift. Shifting a signed integer performs an arithmetic shift.</p>
            </div>
          </div>

          <div class="operator-compact-row">
            <div class="operator-compact-header">Logical</div>
            <div class="operator-compact-body">
              <div class="operator-items">
                <span class="operator-item"><code>||</code> Logical OR</span>
                <span class="operator-item"><code>&amp;&amp;</code> Logical AND</span>
                <span class="operator-item"><code>!</code> Logical NOT</span>
              </div>
              <p class="operator-note">Logical AND (&&) and Logical OR (||) are short-circuiting.</p>
            </div>
          </div>

          <div class="operator-compact-row">
            <div class="operator-compact-header">Comparison</div>
            <div class="operator-compact-body">
              <div class="operator-items">
                <span class="operator-item"><code>&gt;</code> Greater Than</span>
                <span class="operator-item"><code>&lt;</code> Less Than</span>
                <span class="operator-item"><code>&gt;=</code> Greater or Equal</span>
                <span class="operator-item"><code>&lt;=</code> Less or Equal</span>
                <span class="operator-item"><code>==</code> Equal</span>
                <span class="operator-item"><code>!=</code> Not Equal</span>
              </div>
              <p class="operator-note">Equality operators (== and !=) operate across compatible value types when meaningful.</p>
            </div>
          </div>
        </div>
      </div>

      <div id="precedence" class="doc-subsection">
        <h3>3.2 Precedence</h3>
        <p>
          Operator evaluation priority is defined by the following precedence levels, ordered from highest to lowest. Binary operators are left-associative unless otherwise specified:
        </p>
        <div class="precedence-flow-card">
          <div class="precedence-flow">
            <div class="precedence-node">
              <span class="precedence-num">1</span>
              <span class="precedence-pills"><code>-</code><code>!</code><code>~</code></span>
              <span class="precedence-name">Unary</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">2</span>
              <span class="precedence-pills"><code>*</code><code>/</code><code>%</code></span>
              <span class="precedence-name">Multiplicative</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">3</span>
              <span class="precedence-pills"><code>+</code><code>-</code></span>
              <span class="precedence-name">Additive</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">4</span>
              <span class="precedence-pills"><code>&lt;&lt;</code><code>&gt;&gt;</code></span>
              <span class="precedence-name">Shifts</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">5</span>
              <span class="precedence-pills"><code>&gt;</code><code>&lt;</code><code>&gt;=</code><code>&lt;=</code></span>
              <span class="precedence-name">Comparison</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">6</span>
              <span class="precedence-pills"><code>==</code><code>!=</code></span>
              <span class="precedence-name">Equality</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">7</span>
              <span class="precedence-pills"><code>&amp;</code></span>
              <span class="precedence-name">Bitwise AND</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">8</span>
              <span class="precedence-pills"><code>^</code></span>
              <span class="precedence-name">Bitwise XOR</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">9</span>
              <span class="precedence-pills"><code>|</code></span>
              <span class="precedence-name">Bitwise OR</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">10</span>
              <span class="precedence-pills"><code>&amp;&amp;</code></span>
              <span class="precedence-name">Logical AND</span>
            </div>
            <div class="precedence-connector">›</div>

            <div class="precedence-node">
              <span class="precedence-num">11</span>
              <span class="precedence-pills"><code>||</code></span>
              <span class="precedence-name">Logical OR</span>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- 4. Control Flow -->
    <div id="control-flow" class="doc-section">
      <h2>4. Control Flow</h2>
      <p>
        Control flow statements govern the order of execution.
      </p>

      <div id="conditionals" class="doc-subsection">
        <h3>4.1 Conditionals</h3>
        <p>
          Conditionals execute code blocks selectively based on a boolean value.
        </p>
        <pre><code>{@html highlight(`if (<logical-expression>) <code block> else <code block>`, "wasome")}</code></pre>
        <p>
          The condition expression must evaluate strictly to a value of type <code>bool</code>. The <code>else</code> block is optional. The <code>if</code> construct is a statement, not an expression, and does not yield a value.
        </p>
        <pre><code>{@html highlight(`if (x > 10) {
    y <- 1
} else {
    y <- 0
}`, "wasome")}</code></pre>
      </div>

      <div id="loops" class="doc-subsection">
        <h3>4.2 Loops</h3>
        <p>
          Loops execute code blocks repeatedly. The <code>loop</code> keyword is the sole looping primitive and supports three forms.
        </p>
        <pre><code>{@html highlight(`// Infinite loop
loop {
    if (completed) {
        break
    }
}

// While-style loop
loop (index < 10) {
    index <- index + 1
}

// For-style loop
loop (s32 i <- 0; i < 10; i <- i + 1) {
    accumulator <- accumulator + i
}`, "wasome")}</code></pre>
        <p>
          The variable declared in the initialization statement of a for-style loop is scoped strictly within the loop body.
        </p>
        <p>
          The <code>break</code> keyword exits the innermost loop immediately. Wasome does not support a <code>continue</code> statement.
        </p>
      </div>

      <div id="pattern-matching" class="doc-subsection">
        <h3>4.3 Pattern Matching (if let)</h3>
        <p>
          Pattern matching destructures tagged union values conditionally. If the value matches the specified variant pattern, its inner payloads are bound to local variables.
        </p>
        <pre><code>{@html highlight(`if (let exhaustive.Status::Err(s32 code) <- status_val) {
    p.x <- code
}`, "wasome")}</code></pre>
        <p>
          For enum variants containing no payload, the destructuring pattern omits parentheses:
        </p>
        <pre><code>{@html highlight(`if (let Option::None <- maybe_number) {
    y <- -1
}`, "wasome")}</code></pre>
        <p>
          All variables bound in an <code>if let</code> pattern match are scoped strictly within the <code>if</code> branch block.
        </p>
      </div>
    </div>

    <!-- 5. Functions -->
    <div id="functions" class="doc-section">
      <h2>5. Functions</h2>
      <p>
        Functions are subprograms that encapsulate parameterized operations.
      </p>

      <div id="structure" class="doc-subsection">
        <h3>5.1 Structure</h3>
        <p>
          Functions are defined using the <code>fn</code> keyword, parameter lists, and return types.
        </p>
        <pre><code>{@html highlight(`fn <function-name>(<parameter-list>) -> <return-type> <code block>`, "wasome")}</code></pre>
        <p>
          The return type and the <code>-></code> operator may be omitted for functions that do not return a value. Return statements use the <code>-></code> operator:
        </p>
        <pre><code>{@html highlight(`pub fn sum(s32 a, s32 b) -> s32 {
    -> a + b
}`, "wasome")}</code></pre>
        <p>
          If a function declares a return type, every control path must terminate in a return statement yielding a value of the declared type. Functions without a return type may use <code>-></code> alone to execute an early return. Multiple return values are not supported.
        </p>
      </div>

      <div id="extern-functions" class="doc-subsection">
        <h3>5.2 Extern Functions</h3>
        <p>
          Extern functions declare interfaces implemented in the host WebAssembly environment. They are defined without a function body.
        </p>
        <pre><code>{@html highlight(`pub extern fn log_message(u32 ptr, s32 len)`, "wasome")}</code></pre>
      </div>

      <div id="wasm-export-suffixes" class="doc-subsection">
        <h3>5.3 WASM Export Suffixes</h3>
        <p>
          At compile time, extern function declarations and exported symbols have suffixes appended to their names in the WebAssembly binary. These suffixes map to the parameter types and sizes:
        </p>
        <ul>
          <li><code>1</code>: For 8-bit types (<code>u8</code>, <code>s8</code>)</li>
          <li><code>2</code>: For 16-bit types (<code>u16</code>, <code>s16</code>)</li>
          <li><code>4</code>: For 32-bit types (<code>u32</code>, <code>s32</code>, <code>f32</code>, <code>char</code>)</li>
          <li><code>8</code>: For 64-bit types (<code>u64</code>, <code>s64</code>, <code>f64</code>)</li>
          <li><code>ptr</code>: For structural heap types (structs, enums)</li>
        </ul>
        <p>
          Suffixes are combined using underscores in order of parameter appearance:
        </p>
        <pre><code>{@html highlight(`// register_user(u8, s32, struct Point)
// Compiles to WASM symbol: register_user_1_4_ptr`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 6. Structs -->
    <div id="structs" class="doc-section">
      <h2>6. Structs</h2>
      <p>
        Structs are user-defined aggregate types that group related fields and methods under a single heap-allocated reference.
      </p>

      <div id="declaring-instantiating" class="doc-subsection">
        <h3>6.1 Declaring & Instantiating</h3>
        <p>
          Struct declarations specify the struct name in PascalCase and a block containing member fields and methods. Fields default to private unless prefixed with the <code>pub</code> keyword.
        </p>
        <pre><code>{@html highlight(`pub struct Vector3 {
    pub f32 x
    pub f32 y
    pub f32 z
}`, "wasome")}</code></pre>
        <p>
          Instantiation allocates memory on the heap and returns a reference. Instantiations are written using the <code>new</code> keyword followed by the struct name and a field initialization block:
        </p>
        <pre><code>{@html highlight(`Vector3 v <- new Vector3 { x <- 1.0, y <- 2.0, z <- 3.0 }`, "wasome")}</code></pre>
        <p>
          Fields are accessed using dot notation: <code>v.x</code>.
        </p>
      </div>

      <div id="generics" class="doc-subsection">
        <h3>6.2 Generics [T]</h3>
        <p>
          Structs support type parameterization. Generics are specified within square brackets (<code>[]</code>).
        </p>
        <pre><code>{@html highlight(`pub struct Cell[T] {
    pub T element
}

fn initialize() {
    Cell[s32] cell <- new Cell[s32] { element <- 100 }
}`, "wasome")}</code></pre>
      </div>

      <div id="methods-self" class="doc-subsection">
        <h3>6.3 Methods & self</h3>
        <p>
          Methods are functions declared within a struct block. All methods implicitly have a special <code>self</code> variable in scope, which is a mutable reference to the struct instance itself.
        </p>
        <pre><code>{@html highlight(`pub struct Rectangle {
    pub f32 width
    pub f32 height

    pub fn area() -> f32 {
        -> self.width * self.height
    }
}`, "wasome")}</code></pre>
      </div>

      <div id="lifecycle-predrop" class="doc-subsection">
        <h3>6.4 Lifecycle & predrop</h3>
        <p>
          Structs can implement a lifecycle method named <code>predrop</code>. This method must take no parameters (aside from the implicit <code>self</code> reference) and return no value.
        </p>
        <p>
          The <code>predrop</code> method executes immediately before the struct instance is dropped when its reference count falls to zero. If the reference count is increased back up during <code>predrop</code>, the dropping operation is canceled.
        </p>
        <pre><code>{@html highlight(`pub struct Database {
    pub s32 connection_id

    fn predrop() {
        sys_close(self.connection_id)
    }
}`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 7. Enums -->
    <div id="enums" class="doc-section">
      <h2>7. Enums</h2>
      <p>
        Enums define tagged-union types that hold type-safe disjoint variant values. Enums are reference-counted heap types.
      </p>

      <div id="tagged-unions" class="doc-subsection">
        <h3>7.1 Tagged Unions</h3>
        <p>
          Enum declarations define one or more variants. Each variant can encapsulate zero or more concrete types.
        </p>
        <pre><code>{@html highlight(`pub enum Address {
    V4(u8, u8, u8, u8)
    V6(std.String)
    None
}`, "wasome")}</code></pre>
        <p>
          Enums are reference-counted (ARC) heap-allocated types. Variants are constructed using namespace path resolution:
        </p>
        <pre><code>{@html highlight(`Address ip <- Address::V4(127, 0, 0, 1)`, "wasome")}</code></pre>
      </div>

      <div id="option-result" class="doc-subsection">
        <h3>7.2 Option[T] and Result[T, E]</h3>
        <p>
          Option and Result patterns represent optional values and fallible operations via generic enums.
        </p>
        <pre><code>{@html highlight(`pub enum Option[T] {
    Some(T)
    None
}

pub enum Result[T, E] {
    Ok(T)
    Err(E)
}`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 8. Imports & Modules -->
    <div id="imports-modules" class="doc-section">
      <h2>8. Imports & Modules</h2>
      <p>
        Modules manage namespace scoping and access control across compilation boundaries.
      </p>

      <div id="namespaces" class="doc-subsection">
        <h3>8.1 Namespaces</h3>
        <p>
          The <code>import</code> statement specifies a path referencing directories and files.
        </p>
        <pre><code>{@html highlight(`import "<path>"`, "wasome")}</code></pre>
        <p>
          Import paths are <code>/</code>-separated and begin with one of the following root designators:
        </p>
        <ul>
          <li><code>./</code>: Relative to the current directory (default)</li>
          <li><code>&lt;project-name&gt;/</code>: Project root relative</li>
        </ul>
        <p>
          Importing a file namespaces its public elements under the file's base name.
        </p>
        <div class="dependent-code-container">
          <div class="threaded-top-wrapper">
            <div class="code-file-label">main.waso</div>
            <pre class="threaded-pre-top"><code>{@html highlight(`import "./math"

fn evaluate() -> s32 {
    -> math.add(10, 5)
}`, "wasome")}</code></pre>
          </div>

          <div class="code-file-label threaded-label-bottom">/math/add.waso</div>
          <pre class="threaded-pre-bottom"><code>{@html highlight(`// The pub keyword is required to export the function for external use
pub fn add(s32 a, s32 b) -> s32 {
    -> a + b
}`, "wasome")}</code></pre>
        </div>
      </div>

      <div id="aliasing" class="doc-subsection">
        <h3>8.2 Aliasing</h3>
        <p>
          Imported namespaces can be aliased locally using the <code>as</code> keyword. This is usable for all imports to customize module name referencing.
        </p>
        <pre><code>{@html highlight(`import "std/collections" as col

fn build() {
    col.List[s32] items <- col.create_list[s32]()
}`, "wasome")}</code></pre>
      </div>
    </div>
  </article>
</section>

<style>
  .page-content {
    display: grid;
    grid-template-columns: 260px 1fr;
    gap: 4rem;
    padding-top: 4rem;
    padding-bottom: 8rem;
    position: relative;
  }

  /* Left-hand Sticky Sidebar */
  .sidebar {
    opacity: 1;
    pointer-events: auto;
    transform: translateX(0);
    position: relative;
  }

  .sidebar-inner {
    position: sticky;
    top: 120px;
    height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 1rem;
  }

  /* Hide scrollbar for sidebar-inner but preserve scroll behaviour */
  .sidebar-inner::-webkit-scrollbar {
    width: 4px;
  }
  .sidebar-inner::-webkit-scrollbar-thumb {
    background: var(--border-light);
    border-radius: 2px;
  }

  .sidebar h4 {
    margin-bottom: 1.5rem;
    color: var(--text-main);
    text-transform: uppercase;
    font-size: 0.8rem;
    letter-spacing: 0.15em;
    border-left: 2px solid var(--primary);
    padding-left: 0.75rem;
  }

  .sidebar-nav {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .sidebar-group {
    display: flex;
    flex-direction: column;
  }

  .sidebar-item-header {
    display: flex;
    align-items: center;
    border-radius: 0 6px 6px 0;
    padding: 0.5rem 0.75rem 0.5rem 1rem;
    border-left: 4px solid transparent;
    transition: all 0.2s ease;
  }

  .sidebar-item-header:hover {
    background: rgba(255, 255, 255, 0.02);
  }

  .sidebar-item-header.active {
    border-left-color: var(--primary);
    background: linear-gradient(90deg, rgba(250, 204, 21, 0.06) 0%, rgba(250, 204, 21, 0.01) 100%);
  }

  .sidebar-link {
    display: block;
    color: var(--text-secondary);
    font-size: 1rem;
    font-weight: 500;
    flex: 1;
    padding: 0.2rem 0;
    transition: color 0.2s;
  }

  .sidebar-item-header:hover .sidebar-link {
    color: var(--text-main);
  }

  .sidebar-item-header.active .sidebar-link {
    color: var(--primary);
    font-weight: 600;
  }

  /* Indented Visual Tree (Collapse/Expand) */
  .sidebar-subsections {
    display: flex;
    flex-direction: column;
    margin-left: 1rem;
    padding-left: 1rem;
    border-left: 1px solid var(--border-light);
    margin-top: 0.35rem;
    margin-bottom: 0.35rem;
    gap: 0.3rem;
  }

  .sidebar-sublink {
    color: var(--text-muted);
    font-size: 0.88rem;
    padding: 0.3rem 0.5rem;
    border-radius: 4px;
    transition: all 0.2s ease;
  }

  .sidebar-sublink:hover {
    color: var(--primary-hover);
    padding-left: 0.75rem;
  }

  .sidebar-sublink.active {
    color: var(--primary);
    font-weight: 600;
    background: rgba(250, 204, 21, 0.05);
  }

  /* Main Docs Content Layout */
  .docs-content {
    max-width: 800px;
    min-width: 0;
  }

  h1 {
    font-size: 3.5rem;
    margin-bottom: 1.25rem;
    font-weight: 800;
    background: linear-gradient(135deg, #fff 30%, var(--text-secondary) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  .intro {
    font-size: 1.2rem;
    color: var(--text-secondary);
    line-height: 1.7;
    margin-bottom: 2.5rem;
    max-width: 900px;
  }

  /* Custom styled Operator Categories (Compact grid) */
  .operator-compact-container {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
    margin-top: 1.5rem;
  }

  .operator-compact-row {
    display: grid;
    grid-template-columns: 180px 1fr;
    align-items: start;
    gap: 1rem;
  }

  .operator-compact-row:not(:last-child) {
    border-bottom: 1px solid var(--border-light);
    padding-bottom: 1.5rem;
  }

  .operator-compact-header {
    font-size: 0.95rem;
    font-weight: 700;
    color: var(--text-main);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding-top: 0.15rem;
  }

  .operator-compact-body {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .operator-items {
    display: flex;
    flex-wrap: wrap;
    gap: 0.6rem 2.2rem;
  }

  .operator-item {
    font-size: 1rem;
    color: var(--text-secondary);
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .operator-item code {
    background: rgba(255, 255, 255, 0.07);
    color: var(--primary);
    padding: 0.2rem 0.5rem;
    border-radius: 4px;
    font-size: 0.9em;
    font-weight: 600;
  }

  .operator-note {
    font-size: 0.85rem;
    color: var(--text-muted);
    margin-top: 0.2rem;
    margin-bottom: 0;
    font-style: italic;
  }

  /* Responsive compact operators list */
  @media (max-width: 768px) {
    .operator-compact-row {
      grid-template-columns: 1fr;
      gap: 0.5rem;
    }
    
    .operator-compact-row:not(:last-child) {
      padding-bottom: 1.25rem;
    }
  }

  /* Custom Precedence Inline Flow */
  .precedence-flow-card {
    background: rgba(255, 255, 255, 0.01);
    border: 1px solid var(--border-light);
    border-radius: 8px;
    padding: 1.75rem 2rem;
    margin-top: 1.5rem;
  }

  .precedence-flow {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 1rem 0.8rem;
  }

  .precedence-node {
    display: inline-flex;
    align-items: center;
    background: rgba(255, 255, 255, 0.02);
    border: 1px solid rgba(255, 255, 255, 0.04);
    border-radius: 6px;
    padding: 0.45rem 0.9rem;
    gap: 0.6rem;
    font-size: 1.05rem;
    transition: background-color 0.2s, border-color 0.2s;
  }

  .precedence-node:hover {
    background: rgba(255, 255, 255, 0.04);
    border-color: rgba(255, 255, 255, 0.08);
  }

  .precedence-num {
    background: rgba(250, 204, 21, 0.1);
    color: var(--primary);
    font-weight: 700;
    font-size: 0.9rem;
    padding: 0.2rem 0.55rem;
    border-radius: 4px;
    border: 1px solid rgba(250, 204, 21, 0.2);
  }

  .precedence-pills {
    display: flex;
    align-items: center;
    gap: 0.3rem;
  }

  .precedence-pills code {
    background: rgba(255, 255, 255, 0.08);
    color: var(--text-main);
    padding: 0.2rem 0.5rem;
    border-radius: 4px;
    font-size: 0.9em;
    font-weight: 600;
  }

  .precedence-name {
    color: var(--text-secondary);
    font-size: 0.95rem;
    font-weight: 500;
    margin-left: 0.15rem;
  }

  .precedence-connector {
    color: var(--text-muted);
    font-size: 1.4rem;
    font-weight: 300;
    user-select: none;
    margin: 0 0.2rem;
  }

  /* Document layout elements */
  .doc-section {
    margin-bottom: 6rem;
    scroll-margin-top: 100px;
  }

  h2 {
    font-size: 2.25rem;
    margin-bottom: 1.75rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-light);
    color: var(--text-main);
  }

  .doc-subsection {
    margin-top: 3.5rem;
    margin-bottom: 3.5rem;
    scroll-margin-top: 100px;
  }

  h3 {
    font-size: 1.4rem;
    margin-bottom: 1.25rem;
    color: var(--primary);
  }

  p {
    color: var(--text-secondary);
    margin-bottom: 1.5rem;
    font-size: 1rem;
    line-height: 1.7;
  }

  ul {
    list-style: none;
    padding-left: 0;
    margin-bottom: 1.5rem;
  }

  li {
    margin-bottom: 0.75rem;
    color: var(--text-secondary);
    padding-left: 1.25rem;
    position: relative;
    font-size: 0.95rem;
  }

  li::before {
    content: "•";
    color: var(--primary);
    position: absolute;
    left: 0;
    font-weight: bold;
  }

  code {
    background: rgba(255, 255, 255, 0.05);
    padding: 0.25em 0.5em;
    border-radius: 4px;
    color: var(--primary-hover);
    font-family: var(--font-mono);
    font-size: 0.9em;
  }

  #primitives table td:first-child code,
  #primitives table td:last-child code {
    color: #2DD4BF; /* Teal-400 - Original syntax color for types */
  }

  pre {
    background: #020202;
    border: 1px solid var(--border-light);
    border-radius: 8px;
    padding: 1.5rem;
    overflow-x: auto;
    margin-bottom: 2rem;
    box-shadow: inset 0 2px 8px rgba(0, 0, 0, 0.8);
  }

  pre code {
    background: none;
    padding: 0;
    color: #e4e4e7;
    font-family: var(--font-mono);
    display: inline;
    white-space: pre;
  }

  .table-wrapper {
    background: #030303;
    border-radius: 8px;
    border: 1px solid var(--border-light);
    overflow-x: auto;
    margin: 2rem 0;
  }

  table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.95rem;
  }

  th, td {
    text-align: left;
    padding: 1rem 1.25rem;
    border-bottom: 1px solid var(--border-light);
  }

  th {
    color: var(--text-main);
    font-weight: 600;
    background: rgba(255, 255, 255, 0.01);
  }

  td {
    color: var(--text-secondary);
  }

  tr:last-child td {
    border-bottom: none;
  }

  /* IDE-style file tab labels for dependent code blocks */
  .code-file-label {
    font-size: 0.8rem;
    font-weight: 600;
    color: var(--text-muted);
    background: rgba(255, 255, 255, 0.03);
    border: 1px solid var(--border-light);
    border-bottom: none;
    display: inline-block;
    padding: 0.35rem 0.85rem;
    border-radius: 6px 6px 0 0;
    font-family: var(--font-mono);
    margin-top: 1.5rem;
  }

  .code-file-label + pre {
    margin-top: 0;
    border-top-left-radius: 0;
  }

  /* Threaded code blocks in Section 9.1 */
  .dependent-code-container {
    position: relative;
    display: flex;
    flex-direction: column;
    padding-left: 0;
  }

  .threaded-top-wrapper {
    position: relative;
    display: flex;
    flex-direction: column;
  }

  .threaded-pre-top {
    margin-bottom: 0.5rem; /* Tight gap */
  }

  .threaded-label-bottom {
    margin-top: 0.5rem; /* Tight gap */
    position: relative;
    overflow: visible; /* ensure pseudo-elements are not clipped */
  }

  /* Responsive styling details */
  @media (max-width: 1024px) {
    .page-content {
      grid-template-columns: 1fr;
      gap: 2rem;
      padding-top: 2rem;
    }

    .sidebar {
      position: relative;
      width: 100%;
      height: auto;
      transform: none;
      opacity: 1;
      pointer-events: auto;
    }

    .sidebar-inner {
      position: relative;
      top: 0;
      height: auto;
      max-height: 450px;
      overflow-y: auto;
      padding-right: 0;
      border-bottom: 1px solid var(--border-light);
      padding-bottom: 2rem;
    }

    h1 {
      font-size: 2.75rem;
    }
  }
</style>
