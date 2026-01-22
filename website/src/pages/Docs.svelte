<script>
  import { highlight } from '../lib/highlighter';

  function scrollTo(id) {
    const el = document.getElementById(id);
    if (el) {
      // Adjust for header height
      const y = el.getBoundingClientRect().top + window.pageYOffset - 100;
      window.scrollTo({top: y, behavior: 'smooth'});
    }
  }
</script>

<section class="container page-content">
  <aside class="sidebar">
    <div class="sidebar-inner">
      <h4>Contents</h4>
      <nav>
        <a href="#datatypes" on:click|preventDefault={() => scrollTo('datatypes')}>Datatypes</a>
        <a href="#expressions" on:click|preventDefault={() => scrollTo('expressions')}>Expressions</a>
        <a href="#operators" on:click|preventDefault={() => scrollTo('operators')}>Operators</a>
        <a href="#functions" on:click|preventDefault={() => scrollTo('functions')}>Functions</a>
        <a href="#variables" on:click|preventDefault={() => scrollTo('variables')}>Variables</a>
        <a href="#control-flow" on:click|preventDefault={() => scrollTo('control-flow')}>Control Flow</a>
        <a href="#structs" on:click|preventDefault={() => scrollTo('structs')}>Structs</a>
        <a href="#enums" on:click|preventDefault={() => scrollTo('enums')}>Enums</a>
      </nav>
    </div>
  </aside>

  <article class="docs-content">
    <h1>Documentation</h1>
    <p class="intro">The complete reference for the Wasome programming language.</p>
    
    <div id="datatypes" class="doc-section">
      <h2>Datatypes</h2>
      <p>Wasome provides a canonical set of primitives optimized for WebAssembly interop.</p>
      
      <div class="table-wrapper">
        <table>
          <thead>
            <tr>
              <th>Type</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr><td><code>u8, u16, u32, u64</code></td><td>Unsigned integers</td></tr>
            <tr><td><code>s8, s16, s32, s64</code></td><td>Signed integers</td></tr>
            <tr><td><code>f32, f64</code></td><td>Floating-point numbers</td></tr>
            <tr><td><code>bool</code></td><td>Boolean (true / false)</td></tr>
            <tr><td><code>char</code></td><td>Unicode code point (UTF-8)</td></tr>
          </tbody>
        </table>
      </div>
    </div>

    <div id="expressions" class="doc-section">
      <h2>Expressions</h2>
      <p>Literals allow you to express values directly in your code.</p>
      <pre><code>{@html highlight(`// Integers
42
// Floats
3.14159
// Booleans
true`)}</code></pre>
    </div>

    <div id="operators" class="doc-section">
      <h2>Operators</h2>
      <p>Standard arithmetic, bitwise, and logical operators are supported.</p>
      <ul>
        <li><strong>Arithmetic:</strong> <code>+</code> <code>-</code> <code>*</code> <code>/</code> <code>%</code></li>
        <li><strong>Comparison:</strong> <code>&gt;</code> <code>&lt;</code> <code>&gt;=</code> <code>&lt;=</code> <code>==</code> <code>!=</code></li>
        <li><strong>Logical:</strong> <code>||</code> <code>&&</code> <code>!</code></li>
      </ul>
    </div>

    <div id="functions" class="doc-section">
      <h2>Functions</h2>
      <p>Functions are the building blocks of Wasome. They are declared with the <code>fn</code> keyword.</p>
      <pre><code>{@html highlight(`fn add(s32 a, s32 b) -> s32 {
    -> a + b
}`)}</code></pre>
    </div>

    <div id="variables" class="doc-section">
      <h2>Variables</h2>
      <p>Variables are mutable by default and use the arrow syntax <code>&lt;-</code> for assignment.</p>
      <pre><code>{@html highlight(`s32 counter <- 0
counter <- counter + 1`)}</code></pre>
    </div>

    <div id="control-flow" class="doc-section">
      <h2>Control Flow</h2>
      <p>Wasome simplifies loops with a single <code>loop</code> keyword.</p>
      
      <h3>If Statements</h3>
      <pre><code>{@html highlight(`if (is_valid) {
    process()
} else {
    abort()
}`)}</code></pre>

      <h3>Loops</h3>
      <pre><code>{@html highlight(`// While loop
loop (x < 10) { ... }

// For loop equivalent
loop (s32 i <- 0; i < 10; i <- i + 1) { ... }`)}</code></pre>
    </div>

    <div id="structs" class="doc-section">
      <h2>Structs</h2>
      <pre><code>{@html highlight(`struct User {
    s32 id
    bool active
}

User u <- new User { id <- 1, active <- true }`)}</code></pre>
    </div>

    <div id="enums" class="doc-section">
      <h2>Enums</h2>
      <pre><code>{@html highlight(`enum Status {
    Pending,
    Active,
    Error(s32)
}`)}</code></pre>
    </div>

  </article>
</section>

<style>
  .page-content {
    display: grid;
    grid-template-columns: 250px 1fr;
    gap: 4rem;
    padding-top: 4rem;
    padding-bottom: 4rem;
  }

  /* Sidebar */
  .sidebar {
    position: relative;
  }

  .sidebar-inner {
    position: sticky;
    top: 100px;
    border-right: 1px solid var(--border-light);
    padding-right: 2rem;
  }

  .sidebar h4 {
    margin-bottom: 1.5rem;
    color: var(--text-main);
    text-transform: uppercase;
    font-size: 0.85rem;
    letter-spacing: 0.1em;
  }

  .sidebar nav {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .sidebar nav a {
    color: var(--text-secondary);
    font-size: 0.95rem;
  }

  .sidebar nav a:hover {
    color: var(--primary);
    transform: translateX(4px);
  }

  /* Content */
  .docs-content {
    max-width: 800px;
  }

  h1 {
    font-size: 3rem;
    margin-bottom: 1rem;
  }

  .intro {
    font-size: 1.25rem;
    color: var(--text-secondary);
    margin-bottom: 4rem;
  }

  .doc-section {
    margin-bottom: 4rem;
    scroll-margin-top: 100px;
  }

  h2 {
    font-size: 2rem;
    margin-bottom: 1.5rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-light);
  }

  h3 {
    font-size: 1.25rem;
    margin-top: 2rem;
    margin-bottom: 1rem;
    color: var(--text-main);
  }

  p {
    color: var(--text-secondary);
    margin-bottom: 1.5rem;
  }

  ul {
    list-style: none;
    padding-left: 0;
  }

  li {
    margin-bottom: 0.5rem;
    color: var(--text-secondary);
  }

  code {
    background: rgba(255, 255, 255, 0.1);
    padding: 0.2em 0.4em;
    border-radius: 4px;
    color: var(--primary);
  }

  pre {
    background: #0A0A0A;
    border: 1px solid var(--border-light);
    border-radius: 8px;
    padding: 1.5rem;
    overflow-x: auto;
    margin-bottom: 1.5rem;
  }

  pre code {
    background: none;
    padding: 0;
    color: #e4e4e7;
    font-family: var(--font-mono);
  }

  .comment { color: #52525B; }

  /* Table */
  .table-wrapper {
    background: #0A0A0A;
    border-radius: 8px;
    border: 1px solid var(--border-light);
    overflow: hidden;
  }

  table {
    width: 100%;
    border-collapse: collapse;
  }

  th, td {
    padding: 1rem;
    text-align: left;
    border-bottom: 1px solid var(--border-light);
  }

  th {
    background: #111;
    color: #fff;
    font-weight: 600;
  }

  td {
    color: var(--text-secondary);
  }

  tr:last-child td {
    border-bottom: none;
  }

  @media (max-width: 900px) {
    .page-content {
      grid-template-columns: 1fr;
    }
    .sidebar {
      display: none;
    }
  }
</style>
