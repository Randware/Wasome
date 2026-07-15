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
        { id: "strings-arrays", title: "1.3 Strings & Arrays" },
        { id: "typecasting", title: "1.4 Typecasting" }
      ]
    },
    {
      id: "variables",
      title: "2. Variables",
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
        { id: "extern-functions", title: "5.2 Extern Functions" }
      ]
    },
    {
      id: "structs",
      title: "6. Structs",
      icon: Brackets,
      subsections: [
        { id: "declaring-instantiating", title: "6.1 Declaration" },
        { id: "generics", title: "6.2 Generics" },
        { id: "methods-self", title: "6.3 Methods & self" },
        { id: "lifecycle-drop", title: "6.4 Lifecycle & drop" }
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
      id: "modules",
      title: "8. Modules",
      icon: RefreshCw,
      subsections: [
        { id: "namespaces", title: "8.1 Namespaces" },
        { id: "aliasing", title: "8.2 Aliasing" }
      ]
    },
    {
      id: "stdlib",
      title: "9. Standard Library",
      icon: BookOpen,
      subsections: [
        { id: "std-io", title: "9.1 Console I/O" },
        { id: "std-vectors", title: "9.2 Vectors (Vec[T])" },
        { id: "std-strings", title: "9.3 Strings (String)" },
        { id: "std-targets", title: "9.4 Targets & Customization" }
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
    
    // Update hash in URL quietly using the parent main category ID
    if (typeof window !== "undefined") {
      const mainId = parentMap[id];
      window.history.pushState(null, "", `#${mainId}`);
    }
    
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
          // Sync URL hash on scroll using the parent main category ID
          if (typeof window !== "undefined") {
            const mainId = parentMap[entry.target.id];
            window.history.replaceState(null, "", `#${mainId}`);
          }
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

    // Check if there is an initial hash in the URL for deep linking
    if (typeof window !== "undefined" && window.location.hash) {
      const id = window.location.hash.substring(1);
      setTimeout(() => {
        scrollTo(id);
      }, 100);
    }
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
      This is the official reference manual for the Wasome programming language. Wasome is a statically-typed programming language designed to compile directly to efficient WebAssembly linear representation.
    </p>

    <!-- 1. Datatypes -->
    <div id="datatypes" class="doc-section">
      <h2>1. Datatypes</h2>

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
              </tr>
            </thead>
            <tbody>
              <tr>
                <td><code>u8</code></td>
                <td>An unsigned 8-bit integer</td>
                <td>8</td>
              </tr>
              <tr>
                <td><code>u16</code></td>
                <td>An unsigned 16-bit integer</td>
                <td>16</td>
              </tr>
              <tr>
                <td><code>u32</code></td>
                <td>An unsigned 32-bit integer</td>
                <td>32</td>
              </tr>
              <tr>
                <td><code>u64</code></td>
                <td>An unsigned 64-bit integer</td>
                <td>64</td>
              </tr>
              <tr>
                <td><code>s8</code></td>
                <td>A signed 8-bit integer</td>
                <td>8</td>
              </tr>
              <tr>
                <td><code>s16</code></td>
                <td>A signed 16-bit integer</td>
                <td>16</td>
              </tr>
              <tr>
                <td><code>s32</code></td>
                <td>A signed 32-bit integer</td>
                <td>32</td>
              </tr>
              <tr>
                <td><code>s64</code></td>
                <td>A signed 64-bit integer</td>
                <td>64</td>
              </tr>
              <tr>
                <td><code>f32</code></td>
                <td>A 32-bit floating-point number (IEEE 754 binary32)</td>
                <td>32</td>
              </tr>
              <tr>
                <td><code>f64</code></td>
                <td>A 64-bit floating-point number (IEEE 754 binary64)</td>
                <td>64</td>
              </tr>
              <tr>
                <td><code>bool</code></td>
                <td>Either <code>true</code> or <code>false</code></td>
                <td>8</td>
              </tr>
              <tr>
                <td><code>char</code></td>
                <td>A single Unicode code point (source representation encoded in UTF-8)</td>
                <td>32</td>
              </tr>
            </tbody>
          </table>
        </div>

        <p>
          Values written directly in source code evaluate to a default primitive type:
        </p>
        <ul>
          <li><strong>Integer literals</strong> are formatted as a whole number and evaluate to type <code>s32</code>.</li>
          <li><strong>Floating-point literals</strong> are formatted as a decimal number and evaluate to type <code>f64</code>.</li>
          <li><strong>Logical (boolean) literals</strong> (<code>true</code> and <code>false</code>) evaluate to type <code>bool</code>.</li>
        </ul>
      </div>

      <div id="memory-representation" class="doc-subsection">
        <h3>1.2 Memory Representation</h3>
        <p>
          Struct instances are passed by reference. Mutating a struct through any reference affects all references.
        </p>
        <p>
          Heap allocations (for structs and enums) are managed via Automatic Reference Counting (ARC). Memory is reclaimed immediately when the reference count of an instance reaches zero.
        </p>
      </div>

      <div id="strings-arrays" class="doc-subsection">
        <h3>1.3 Strings & Arrays</h3>
        <p>
          Wasome does not have a built-in array or string type. They must be provided by the standard library.
        </p>
      </div>

      <div id="typecasting" class="doc-subsection">
        <h3>1.4 Typecasting</h3>
        <p>
          Typecasting converts an expression to a target type using the <code>as</code> syntax:
        </p>
        <pre><code>{@html highlight(`<expression> as <type>`, "wasome")}</code></pre>
        <p>
          The <code>as</code> expression evaluates to the value converted to the target type if the conversion is defined. The <code>as</code> syntax is an expression. The following conversions are permitted:
        </p>
        <ul>
          <li><strong>Numeric conversions:</strong> Any numeric type can be converted directly to any other numeric type. This includes widening (smaller to larger integer or floating-point type, or integer to floating-point), narrowing (larger to smaller type, or floating-point to integer), and signed/unsigned conversions.</li>
          <li><strong>Boolean conversions:</strong> Conversions between <code>bool</code> and <code>u8</code>/<code>s8</code> are permitted. A value of <code>0</code> is <code>false</code> and <code>1</code> is <code>true</code>. When casting into <code>bool</code>, only the least significant bit is considered (e.g. <code>10</code> becomes <code>false</code>, <code>11</code> becomes <code>true</code>). Casting any value greater than <code>1</code> or less than <code>0</code> into a <code>bool</code> is discouraged.</li>
          <li><strong>Character conversions:</strong> A <code>char</code> can be converted to <code>u32</code>. It uses UTF-8, padding chars shorter than 4 bytes. The reverse conversion (<code>u32</code> to <code>char</code>) is provided by the standard library.</li>
        </ul>
        <p>
          If a converted value does not fit into the target target type, the value will overflow or underflow.
        </p>
        <p>
          Numeric conversions are performed directly, without needing an intermediate type:
        </p>
        <pre><code>{@html highlight(`f64 precise_pi <- 3.14159
s32 rough_pi <- precise_pi as s32
f64 normal_pi <- rough_pi as f64`, "wasome")}</code></pre>
        <p>
          Boolean conversions follow the same <code>as</code> syntax:
        </p>
        <pre><code>{@html highlight(`u8 flag <- 1
bool active <- flag as bool
u8 back <- active as u8`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 2. Variables -->
    <div id="variables" class="doc-section">
      <h2>2. Variables</h2>

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
      </div>

      <div id="scoping" class="doc-subsection">
        <h3>2.2 Scoping</h3>
        <p>
          Variables declared in a code block are scoped to that block and any nested blocks. Blocks are delimited by curly braces <code>&#123;</code> and <code>&#125;</code> and push a new lexical scope.
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
                <span class="operator-item"><code>%</code> Remainder</span>
              </div>
              <p class="operator-note">Binary arithmetic operators expect two numeric operands of the same type. Division on integers truncates toward zero. Remainder (%) is defined only for integer operands.</p>
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
              <p class="operator-note">Logical operators are used to evaluate boolean expressions.</p>
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
        <ul>
          <li><strong>Level 1</strong>: <code>-</code> (unary), <code>!</code></li>
          <li><strong>Level 2</strong>: <code>*</code>, <code>/</code>, <code>%</code> (left-associative)</li>
          <li><strong>Level 3</strong>: <code>+</code>, <code>-</code> (left-associative)</li>
          <li><strong>Level 4</strong>: <code>&lt;&lt;</code>, <code>&gt;&gt;</code> (left-associative)</li>
          <li><strong>Level 5</strong>: <code>&gt;</code>, <code>&lt;</code>, <code>&gt;=</code>, <code>&lt;=</code> (left-associative)</li>
          <li><strong>Level 6</strong>: <code>==</code>, <code>!=</code> (left-associative)</li>
          <li><strong>Level 7</strong>: <code>&amp;</code> (left-associative)</li>
          <li><strong>Level 8</strong>: <code>^</code> (left-associative)</li>
          <li><strong>Level 9</strong>: <code>|</code> (left-associative)</li>
          <li><strong>Level 10</strong>: <code>&amp;&amp;</code> (left-associative)</li>
          <li><strong>Level 11</strong>: <code>||</code> (left-associative)</li>
        </ul>
      </div>
    </div>

    <!-- 4. Control Flow -->
    <div id="control-flow" class="doc-section">
      <h2>4. Control Flow</h2>

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
          The variable declared in the initialization statement of a for-style loop is scoped to that loop block and any nested blocks.
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

      <div id="structure" class="doc-subsection">
        <h3>5.1 Structure</h3>
        <p>
          Functions are defined using the <code>fn</code> keyword, parameter lists, and return types.
        </p>
        <pre><code>{@html highlight(`fn <function-name>(<parameter-list>) -> <return-type> <code block>`, "wasome")}</code></pre>
        <p>
          The return type and the <code>-></code> symbol may be omitted for functions that do not return a value. Return statements use the <code>-></code> statement syntax:
        </p>
        <pre><code>{@html highlight(`fn sum(s32 a, s32 b) -> s32 {
    -> a + b
}`, "wasome")}</code></pre>
        <p>
          If a function declares a return type, every control path must terminate in a return statement yielding a value of the declared type. Functions without a return type may use <code>-></code> alone to execute an early return. Return instructions are statements. Multiple return values are not supported.
        </p>
      </div>

      <div id="extern-functions" class="doc-subsection">
        <h3>5.2 Extern Functions</h3>
        <p>
          Extern function syntax:
        </p>
        <pre><code>{@html highlight(`extern fn <function-name>(<parameter-list>) -> <return-type>`, "wasome")}</code></pre>
        <p>
          In the compiled object file, the function will have the same name but with suffixes for type parameters. These suffixes represent the size of the parameter or <code>ptr</code> for pointers:
        </p>
        <div class="table-wrapper">
          <table>
            <thead>
              <tr>
                <th>Suffix</th>
                <th>Applies To</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td><code>1</code></td>
                <td>8-bit types (<code>u8</code>, <code>s8</code>)</td>
              </tr>
              <tr>
                <td><code>2</code></td>
                <td>16-bit types (<code>u16</code>, <code>s16</code>)</td>
              </tr>
              <tr>
                <td><code>4</code></td>
                <td>32-bit types (<code>u32</code>, <code>s32</code>, <code>f32</code>, <code>char</code>)</td>
              </tr>
              <tr>
                <td><code>8</code></td>
                <td>64-bit types (<code>u64</code>, <code>s64</code>, <code>f64</code>)</td>
              </tr>
              <tr>
                <td><code>ptr</code></td>
                <td>Heap structural types (enums and structs)</td>
              </tr>
            </tbody>
          </table>
        </div>
        <p>
          Suffixes are combined with underscores in order of parameter appearance.
        </p>
        <pre><code>{@html highlight(`// extern fn process(u8 value, struct Data d)
// Symbol name in compiled object: process_1_ptr`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 6. Structs -->
    <div id="structs" class="doc-section">
      <h2>6. Structs</h2>

      <div id="declaring-instantiating" class="doc-subsection">
        <h3>6.1 Declaration</h3>
        <p>
          Struct declarations specify the struct name in PascalCase and a block containing member fields and methods.
        </p>
        <pre><code>{@html highlight(`struct Vector3 {
    f64 x
    f64 y
    f64 z
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
        <h3>6.2 Generics</h3>
        <p>
          Structs support type parameterization. Generics are specified within square brackets (<code>[]</code>).
        </p>
        <pre><code>{@html highlight(`struct Cell[T] {
    T element
}

fn initialize() {
    Cell[s32] cell <- new Cell[s32] { element <- 100 }
}`, "wasome")}</code></pre>
      </div>

      <div id="methods-self" class="doc-subsection">
        <h3>6.3 Methods & self</h3>
        <p>
          Methods are regular functions declared within a struct block. All methods implicitly have a special <code>self</code> variable in scope, which is a mutable reference to the struct instance itself. The <code>self</code> parameter does not have to be explicitly specified in the parameter list.
        </p>
        <p>
          To invoke a method on a struct instance, the instance expression must be enclosed in parentheses (e.g., <code>(instance).method()</code>). Otherwise, the dot accessor is parsed strictly as a field reference.
        </p>
        <pre><code>{@html highlight(`struct Rectangle {
    f64 width
    f64 height

    fn area() -> f64 {
        -> self.width * self.height
    }
}

fn calculate() {
    Rectangle rect <- new Rectangle { width <- 10.0, height <- 20.0 }
    f64 a <- (rect).area()
}`, "wasome")}</code></pre>
      </div>

      <div id="lifecycle-drop" class="doc-subsection">
        <h3>6.4 Lifecycle & drop</h3>
        <p>
          Structs can have a <code>drop</code> method with the same name, no parameters aside from the implicit <code>self</code> one and no return value.
        </p>
        <p>
          Having a method called <code>drop</code> that does not fulfill the other criteria is a syntax error.
        </p>
        <p>
          It is executed right before a struct is dropped.
        </p>
        <p>
          Should it increase the reference count back up, the dropping is canceled.
        </p>
        <pre><code>{@html highlight(`struct Database {
    s32 connection_id

    fn drop() {
        sys_close(self.connection_id)
    }
}`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 7. Enums -->
    <div id="enums" class="doc-section">
      <h2>7. Enums</h2>

      <div id="tagged-unions" class="doc-subsection">
        <h3>7.1 Tagged Unions</h3>
        <p>
          Enum declarations define one or more variants. Each variant can encapsulate zero or more concrete types.
        </p>
        <pre><code>{@html highlight(`enum Address {
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
          Option and Result patterns represent optional values and fallible operations via generic enums. When instantiating a variant of a generic enum, concrete type parameters must be explicitly specified:
        </p>
        <pre><code>{@html highlight(`enum Option[T] {
    Some(T)
    None
}

fn create() {
    Option[s32] wrapped <- Option[s32]::Some(42)
}`, "wasome")}</code></pre>
      </div>
    </div>

    <!-- 8. Modules -->
    <div id="modules" class="doc-section">
      <h2>8. Modules</h2>

      <div id="namespaces" class="doc-subsection">
        <h3>8.1 Namespaces</h3>
        <p>
          The <code>import</code> statement specifies a path referencing a target <strong>directory</strong>. In Wasome, you can only import entire directories; importing individual source files directly is not supported.
        </p>
        <pre><code>{@html highlight(`import "<directory-path>"`, "wasome")}</code></pre>
        <p>
          Import paths are <code>/</code>-separated and begin with one of the following root designators:
        </p>
        <ul>
          <li><code>./</code>: Relative to the importing file's directory (default)</li>
          <li><code>&lt;project-name&gt;/</code>: Project root relative</li>
          <li><code>std/</code>: Standard library relative</li>
        </ul>
        <p>
          Importing a directory namespaces all of its public elements (marked with the <code>pub</code> keyword) under the directory's base name. The compiler resolves the import by scanning the directory and compiling all of its source files together.
        </p>
        <div class="dependent-code-container">
          <div class="threaded-top-wrapper">
            <div class="code-file-label">src/main.waso</div>
            <pre class="threaded-pre-top"><code>{@html highlight(`import "./utils" as u

fn evaluate() -> s32 {
    // Accesses public functions from the src/utils directory
    -> u.add(10, 5)
}`, "wasome")}</code></pre>
          </div>

          <div class="code-file-label threaded-label-bottom">src/utils/math.waso</div>
          <pre class="threaded-pre-bottom"><code>{@html highlight(`// The pub keyword is required to export functions from files within the directory
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

    <!-- 9. Standard Library -->
    <div id="stdlib" class="doc-section">
      <h2>9. Standard Library</h2>
      <p>
        Wasome provides a standard library containing fundamental utilities for Console I/O, dynamic arrays, and string manipulation. To use standard library utilities, import the standard library using the special <code>"std"</code> identifier:
      </p>
      <pre><code>{@html highlight(`import "std" as std`, "wasome")}</code></pre>

      <div id="std-io" class="doc-subsection">
        <h3>9.1 Console I/O</h3>
        <p>
          The standard library exposes standard input/output functions to interact with the console terminal:
        </p>
        <ul>
          <li><code>pub fn print_char(char to_print)</code>: Prints a single character to standard output.</li>
          <li><code>pub fn print_string(String to_print)</code>: Prints a <code>String</code> object to standard output.</li>
          <li><code>pub fn read_line() -> String</code>: Reads a line of text from standard input, returning it as a new <code>String</code>.</li>
        </ul>
        <pre><code>{@html highlight(`import "std" as std

fn main() {
    std.print_char('H')
    std.print_char('i')
    std.print_char('\\n')
}`, "wasome")}</code></pre>
      </div>

      <div id="std-vectors" class="doc-subsection">
        <h3>9.2 Vectors (Vec[T])</h3>
        <p>
          Vectors (<code>Vec[T]</code>) are generic, dynamically-sized heap-allocated arrays. When a vector is dropped, it automatically frees all of its elements to prevent memory leaks.
        </p>
        <p>
          Construct a new empty vector using:
        </p>
        <pre><code>{@html highlight(`pub fn vec_new[T]() -> Vec[T]`, "wasome")}</code></pre>
        <p>
          The <code>Vec[T]</code> struct exposes the following public methods:
        </p>
        <ul>
          <li><code>pub fn push(T val)</code>: Appends an element to the end of the vector.</li>
          <li><code>pub fn pop() -> T</code>: Removes and returns the last element from the vector.</li>
          <li><code>pub fn get(u32 index) -> T</code>: Retrieves a reference to the element at the specified index.</li>
          <li><code>pub fn set(u32 index, T val) -> T</code>: Replaces the element at the specified index, returning the old value.</li>
          <li><code>pub fn remove(u32 index) -> T</code>: Removes and returns the element at the specified index, shifting subsequent elements.</li>
          <li><code>pub fn len() -> u32</code>: Returns the number of elements currently stored in the vector.</li>
        </ul>
        <pre><code>{@html highlight(`import "std" as std

fn main() {
    std.Vec[s32] list <- std.vec_new[s32]()
    (list).push(10)
    (list).push(20)

    s32 element <- (list).get(0) // 10
    s32 length <- (list).len()   // 2
}`, "wasome")}</code></pre>
      </div>

      <div id="std-strings" class="doc-subsection">
        <h3>9.3 Strings (String)</h3>
        <p>
          The <code>String</code> struct is a reference-counted, heap-allocated character buffer. It can dynamically grow as new values are appended.
        </p>
        <p>
          Construct a new empty string using:
        </p>
        <pre><code>{@html highlight(`pub fn string_new() -> String`, "wasome")}</code></pre>
        <p>
          The <code>String</code> struct exposes the following public methods:
        </p>
        <ul>
          <li><code>pub fn push_char(char to_push)</code>: Appends a single Unicode character to the string.</li>
          <li><code>pub fn push_s32(s32 to_push)</code>: Appends a signed 32-bit integer formatted as a text string.</li>
          <li><code>pub fn push_s64(s64 to_push)</code>: Appends a signed 64-bit integer formatted as a text string.</li>
          <li><code>pub fn push_u64(u64 to_push)</code>: Appends an unsigned 64-bit integer formatted as a text string.</li>
          <li><code>pub fn push_f64(s64 to_push)</code>: Appends a float (cast as 64-bit integer) formatted as a text string.</li>
          <li><code>pub fn push_string(String to_push)</code>: Appends another <code>String</code> to this string.</li>
          <li><code>pub fn pop() -> char</code>: Removes and returns the last character from the string.</li>
          <li><code>pub fn len() -> u32</code>: Returns the length (number of characters) of the string.</li>
        </ul>
        <pre><code>{@html highlight(`import "std" as std

fn main() {
    std.String message <- std.string_new()
    (message).push_string(std.read_line())
    (message).push_char('!')

    std.print_string(message)
}`, "wasome")}</code></pre>
      </div>

      <div id="std-targets" class="doc-subsection">
        <h3>9.4 Targets & Customization</h3>
        <p>
          The Wasome compiler lists and resolves targets dynamically by scanning subdirectories inside the standard library root directory (default location is <code>&lt;binary_dir&gt;/../std/</code>, which can be overridden via the <code>--std &lt;path&gt;</code> flag).
        </p>
        <p>
          By default, Wasome ships with two pre-configured compilation targets:
        </p>
        <ul>
          <li><strong>runtime</strong>: The default target used for native CLI command executions. It links to a debug build of the standard library runtime.</li>
          <li><strong>web</strong>: Optimized for web browser environments. It compiles programs with size-min profiles and links to a release build of the standard library runtime.</li>
        </ul>
        <p>
          Each target folder inside the standard library root must follow a specific directory structure to be recognized as a valid target:
        </p>
        <pre><code>std/&lt;target-name&gt;/
├── wasome/
│   ├── waso.toml       # Standard library project manifest
│   └── src/
│       └── lib.waso    # Public standard library constructs and declarations
└── bin/                # Pre-compiled static archives (.a) or object files (.o) (or "lib/")
    └── libstd.a        # Compiled native implementation of the runtime target</code></pre>
        <p>
          You can list all currently recognized targets by running the target list command:
        </p>
        <pre><code>waso target list</code></pre>
        
        <h4>Creating a Custom Target</h4>
        <p>
          You can implement your own custom standard library target by adding a new directory inside the standard library path and building it:
        </p>
        <ul>
          <li>Create a target directory: <code>std/my-target/</code>.</li>
          <li>Place your public Wasome declarations under <code>std/my-target/wasome/src/lib.waso</code> and standard library project configuration in <code>std/my-target/wasome/waso.toml</code>.</li>
          <li>Write the native implementation in a compiled language capable of exporting to WebAssembly static archives (like Rust). Inside the Rust project's <code>Cargo.toml</code>, specify:
            <pre><code>[lib]
crate-type = ["staticlib"]</code></pre>
          </li>
          <li>Build the static archive using the WebAssembly target:
            <pre><code>cargo build --release --target wasm32-wasip1</code></pre>
          </li>
          <li>Copy the generated archive (e.g., <code>libstd.a</code>) into the target bin folder: <code>std/my-target/bin/libstd.a</code>.</li>
          <li>Build your application linking to the custom target:
            <pre><code>waso build --target my-target</code></pre>
          </li>
        </ul>
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
