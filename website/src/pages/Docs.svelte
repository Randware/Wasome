<script>
  import { onMount, onDestroy } from "svelte";
  import { highlight } from "../lib/highlighter";

  let activeSection = $state("datatypes");
  let tocOpen = $state(false);
  let observer;

  const sections = [
    { id: "datatypes", label: "Datatypes" },
    { id: "expressions", label: "Expressions" },
    { id: "operators", label: "Operators" },
    { id: "functions", label: "Functions" },
    { id: "variables", label: "Variables" },
    { id: "control-flow", label: "Control Flow" },
    { id: "structs", label: "Structs" },
    { id: "enums", label: "Enums" },
  ];

  function scrollTo(e, id) {
    e.preventDefault();
    tocOpen = false;
    activeSection = id;
    const el = document.getElementById(id);
    if (el) {
      const y = el.getBoundingClientRect().top + window.pageYOffset - 100;
      window.scrollTo({ top: y, behavior: "smooth" });
    }
  }

  onMount(() => {
    observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            activeSection = entry.target.id;
          }
        });
      },
      { rootMargin: "-100px 0px -60% 0px", threshold: 0 },
    );

    sections.forEach((s) => {
      const el = document.getElementById(s.id);
      if (el) observer.observe(el);
    });
  });

  onDestroy(() => {
    if (observer) observer.disconnect();
  });
</script>

<section class="container page-content page-animate">
  <aside class="sidebar">
    <div class="sidebar-inner">
      <button class="toc-toggle" onclick={() => (tocOpen = !tocOpen)}>
        <h4>Contents</h4>
        <svg
          class="toc-chevron"
          class:open={tocOpen}
          width="16"
          height="16"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"><polyline points="6 9 12 15 18 9"></polyline></svg
        >
      </button>
      <nav class:collapsed={!tocOpen}>
        {#each sections as s}
          <a
            href="#{s.id}"
            class:active={activeSection === s.id}
            onclick={(e) => scrollTo(e, s.id)}
          >
            {s.label}
          </a>
        {/each}
      </nav>
    </div>
  </aside>

  <article class="docs-content">
    <h1>Documentation</h1>
    <p class="intro">
      The complete reference for the Wasome programming language.
    </p>

    <div id="datatypes" class="doc-section">
      <h2>Datatypes</h2>
      <p>
        Wasome provides a canonical set of primitives optimized for WebAssembly
        interop.
      </p>

      <div class="table-wrapper">
        <table>
          <thead>
            <tr>
              <th>Type</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr
              ><td><code>{@html highlight("u8, u16, u32, u64")}</code></td><td
                >Unsigned integers</td
              ></tr
            >
            <tr
              ><td><code>{@html highlight("s8, s16, s32, s64")}</code></td><td
                >Signed integers</td
              ></tr
            >
            <tr
              ><td><code>{@html highlight("f32, f64")}</code></td><td
                >Floating-point numbers</td
              ></tr
            >
            <tr
              ><td><code>{@html highlight("bool")}</code></td><td
                >Boolean (true / false)</td
              ></tr
            >
            <tr
              ><td><code>{@html highlight("char")}</code></td><td
                >Unicode code point (UTF-8)</td
              ></tr
            >
          </tbody>
        </table>
      </div>
    </div>

    <div id="expressions" class="doc-section">
      <h2>Expressions</h2>
      <p>Literals allow you to express values directly in your code.</p>
      <pre><code
          >{@html highlight(`// Integers
42
// Floats
3.14159
// Booleans
true`)}</code
        ></pre>
    </div>

    <div id="operators" class="doc-section">
      <h2>Operators</h2>
      <p>Standard arithmetic, bitwise, and logical operators are supported.</p>
      <ul>
        <li>
          <strong>Arithmetic:</strong> <code>+</code> <code>-</code>
          <code>*</code> <code>/</code> <code>%</code>
        </li>
        <li>
          <strong>Comparison:</strong> <code>&gt;</code> <code>&lt;</code>
          <code>&gt;=</code> <code>&lt;=</code> <code>==</code> <code>!=</code>
        </li>
        <li>
          <strong>Logical:</strong> <code>||</code> <code>&&</code>
          <code>!</code>
        </li>
      </ul>
    </div>

    <div id="functions" class="doc-section">
      <h2>Functions</h2>
      <p>
        Functions are the building blocks of Wasome. They are declared with the <code
          >fn</code
        > keyword.
      </p>
      <pre><code
          >{@html highlight(`fn add(s32 a, s32 b) -> s32 {
    -> a + b
}`)}</code
        ></pre>
    </div>

    <div id="variables" class="doc-section">
      <h2>Variables</h2>
      <p>
        Variables are mutable by default and use the arrow syntax <code
          >&lt;-</code
        > for assignment.
      </p>
      <pre><code
          >{@html highlight(`s32 counter <- 0
counter <- counter + 1`)}</code
        ></pre>
    </div>

    <div id="control-flow" class="doc-section">
      <h2>Control Flow</h2>
      <p>Wasome simplifies loops with a single <code>loop</code> keyword.</p>

      <h3>If Statements</h3>
      <pre><code
          >{@html highlight(`if (is_valid) {
    process()
} else {
    abort()
}`)}</code
        ></pre>

      <h3>Loops</h3>
      <pre><code
          >{@html highlight(`// While loop
loop (x < 10) { ... }

// For loop equivalent
loop (s32 i <- 0; i < 10; i <- i + 1) { ... }`)}</code
        ></pre>
    </div>

    <div id="structs" class="doc-section">
      <h2>Structs</h2>
      <pre><code
          >{@html highlight(`struct User {
    s32 id
    bool active
}

User u <- new User { id <- 1, active <- true }`)}</code
        ></pre>
    </div>

    <div id="enums" class="doc-section">
      <h2>Enums</h2>
      <pre><code
          >{@html highlight(`enum Status {
    Pending,
    Active,
    Error(s32)
}`)}</code
        ></pre>
    </div>
  </article>
</section>

<style>
  .page-content {
    display: grid;
    grid-template-columns: 250px 1fr;
    gap: 4rem;
    padding-top: 4rem;
    padding-bottom: 8rem;
  }

  .sidebar {
    position: relative;
  }

  .sidebar-inner {
    position: sticky;
    top: 100px;
    border-right: 1px solid var(--border-light);
    padding-right: 2rem;
  }

  .toc-toggle {
    display: none;
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
    gap: 0.25rem;
  }

  .sidebar nav a {
    color: var(--text-secondary);
    font-size: 0.95rem;
    padding: 0.4rem 0.75rem;
    border-left: 2px solid transparent;
    border-radius: 0 4px 4px 0;
    transition: all 0.2s;
  }

  .sidebar nav a:hover {
    color: var(--primary);
    background: rgba(250, 204, 21, 0.03);
  }

  .sidebar nav a.active {
    color: var(--primary);
    border-left-color: var(--primary);
    background: rgba(250, 204, 21, 0.05);
    font-weight: 500;
  }

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
    padding: 0.3em 0.6em;
    border-radius: 4px;
    color: var(--primary);
    display: inline-block;
    line-height: 1.4;
    white-space: nowrap;
  }

  pre {
    background: #0a0a0a;
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
    display: inline;
    white-space: pre;
  }

  .table-wrapper {
    background: #0a0a0a;
    border-radius: 8px;
    border: 1px solid var(--border-light);
    overflow-x: auto;
  }

  table {
    width: 100%;
    border-collapse: collapse;
  }

  th,
  td {
    text-align: left;
    padding: 1rem;
    border-bottom: 1px solid var(--border-light);
  }

  th {
    color: var(--text-main);
    font-weight: 600;
  }

  td {
    color: var(--text-secondary);
  }

  tr:last-child td {
    border-bottom: none;
  }

  .toc-chevron {
    transition: transform 0.2s;
  }

  .toc-chevron.open {
    transform: rotate(180deg);
  }

  @media (max-width: 900px) {
    .page-content {
      grid-template-columns: 1fr;
      display: flex;
      flex-direction: column;
      gap: 0;
    }

    .sidebar {
      position: static;
      border-bottom: 1px solid var(--border-light);
      margin-bottom: 2rem;
    }

    .sidebar-inner {
      position: static;
      border-right: none;
      padding-right: 0;
    }

    .sidebar h4 {
      margin-bottom: 0;
    }

    .toc-toggle {
      display: flex;
      align-items: center;
      justify-content: space-between;
      width: 100%;
      background: none;
      border: none;
      padding: 1rem 0;
      cursor: pointer;
      color: var(--text-main);
    }

    .sidebar nav {
      flex-direction: column;
      gap: 0.25rem;
      padding-bottom: 1rem;
    }

    .sidebar nav.collapsed {
      display: none;
    }

    .sidebar nav a {
      padding: 0.5rem 0.75rem;
      border-left: 2px solid transparent;
      border-radius: 0 4px 4px 0;
    }
  }
</style>
