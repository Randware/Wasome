<script>
  import { navigate } from '../lib/router';
  import Background3D from '../components/Background3D.svelte';
  import { highlight } from '../lib/highlighter';
  
  const snippet = `struct User {
    s32 id
    bool is_active
}

fn main() -> s32 {
    User u <- new User { id <- 42, is_active <- true }
    
    if (u.is_active) {
        -> u.id * 2
    }
    -> 0
}`;
</script>

<section class="hero-section">
  <Background3D />
  
  <div class="content container">
    <div class="badge">v0.1.0 Alpha is here</div>
    
    <h1>
      The <span class="gradient-text">WebAssembly</span><br>
      Language for Everyone
    </h1>
    
    <p class="subtitle">
      Wasome is a modern, high-performance language built directly for the web. 
      Type-safe, expressive, and blazing fast.
    </p>
    
    <div class="actions">
      <button class="btn primary" onclick={() => navigate('/docs')}>
        Get Started
      </button>
      <button class="btn secondary" onclick={() => navigate('/walkthrough')}>
        Take the Tour
      </button>
    </div>

    <div class="code-preview">
      <div class="window-controls">
        <span class="dot red"></span>
        <span class="dot yellow"></span>
        <span class="dot green"></span>
        <span class="window-title">main.waso</span>
      </div>
      <div class="code-inner">
        <pre>{@html highlight(snippet)}</pre>
      </div>
    </div>
  </div>
</section>

<section class="features container">
  <div class="feature-card">
    <h3>🚀 Native Performance</h3>
    <p>Compiles directly to optimized WebAssembly with zero overhead. Your code runs at near-native speed.</p>
  </div>
  <div class="feature-card">
    <h3>🛡️ Type Safe</h3>
    <p>Strong static typing catches errors at compile time, not runtime. No more undefined is not a function.</p>
  </div>
  <div class="feature-card">
    <h3>✨ Modern Syntax</h3>
    <p>Familiar syntax inspired by Rust and TypeScript, designed for clarity and productivity.</p>
  </div>
</section>

<section class="why-section container">
  <h2>Why Wasome?</h2>
  <div class="grid-2">
    <div>
      <h3>WebAssembly First</h3>
      <p>
        Most languages treat WebAssembly as a compilation target. Wasome treats it as the platform.
        Every feature maps cleanly to Wasm instructions, ensuring predictable performance and tiny binary sizes.
      </p>
    </div>
    <div>
      <h3>Zero Garbage Collection</h3>
      <p>
        Manage memory manually or use linear memory patterns. Wasome gives you control without the complexity of C++.
        Perfect for games, audio processing, and high-performance visualizations.
      </p>
    </div>
    <div>
      <h3>Tooling that Works</h3>
      <p>
        Built-in formatter, linter, and package manager. The Wasome ecosystem is designed to get you up and running in seconds.
      </p>
    </div>
    <div>
      <h3>Open Source</h3>
      <p>
        Community driven development. Join us on GitHub to shape the future of the language.
      </p>
    </div>
  </div>
</section>

<style>
  .hero-section {
    position: relative;
    padding: 8rem 0 4rem;
    display: flex;
    flex-direction: column;
    align-items: center;
    text-align: center;
    overflow: hidden;
    min-height: 90vh; /* ensure enough height for 3D bg */
  }

  .badge {
    background: rgba(250, 204, 21, 0.1);
    color: var(--primary);
    border: 1px solid rgba(250, 204, 21, 0.2);
    padding: 0.4rem 1rem;
    border-radius: 99px;
    font-size: 0.85rem;
    font-weight: 500;
    margin-bottom: 2rem;
    display: inline-block;
  }

  h1 {
    font-size: 5rem;
    font-weight: 800;
    line-height: 1.05;
    margin-bottom: 1.5rem;
    letter-spacing: -0.03em;
  }

  .gradient-text {
    background: linear-gradient(135deg, #fff 30%, #888 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  .subtitle {
    font-size: 1.25rem;
    color: var(--text-secondary);
    max-width: 600px;
    margin: 0 auto 3rem;
  }

  .actions {
    display: flex;
    gap: 1rem;
    justify-content: center;
    margin-bottom: 5rem;
  }

  .btn {
    padding: 1rem 2rem;
    border-radius: 8px;
    font-weight: 600;
    font-size: 1rem;
    transition: all 0.2s;
  }

  .btn.primary {
    background: var(--primary);
    color: #000;
    box-shadow: 0 0 20px rgba(250, 204, 21, 0.3);
  }

  .btn.primary:hover {
    background: var(--primary-dim);
    transform: translateY(-2px);
    box-shadow: 0 0 30px rgba(250, 204, 21, 0.4);
  }

  .btn.secondary {
    background: rgba(255, 255, 255, 0.05);
    color: #fff;
    border: 1px solid rgba(255, 255, 255, 0.1);
  }

  .btn.secondary:hover {
    background: rgba(255, 255, 255, 0.1);
    border-color: rgba(255, 255, 255, 0.2);
  }

  .code-preview {
    background: #1e1e1e; /* Solid dark background */
    border: 1px solid #333;
    border-radius: 12px;
    text-align: left;
    width: 100%;
    max-width: 600px;
    box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5); /* Better shadow */
    position: relative;
    overflow: hidden;
  }

  .window-controls {
    display: flex;
    gap: 8px;
    padding: 12px 16px;
    background: #252526; /* Slightly lighter header */
    border-bottom: 1px solid #333;
    align-items: center;
  }

  .dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
    display: inline-block;
  }

  .red { background-color: #FF5F56; border: 1px solid #E0443E; }
  .yellow { background-color: #FFBD2E; border: 1px solid #DEA123; }
  .green { background-color: #27C93F; border: 1px solid #1AAB29; }

  .window-title {
    margin-left: auto;
    font-size: 0.85rem;
    color: #888;
    font-family: var(--font-mono);
  }

  .code-inner {
    padding: 1.5rem;
    background: #1e1e1e;
  }

  pre {
    margin: 0;
    font-family: var(--font-mono);
    color: #d4d4d8;
    line-height: 1.6;
    font-size: 0.95rem;
    overflow-x: auto;
  }

  .features {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 2rem;
    margin-top: 4rem;
  }

  .feature-card {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    padding: 2rem;
    border-radius: 12px;
    transition: all 0.3s;
  }

  .feature-card:hover {
    border-color: var(--primary);
    transform: translateY(-5px);
  }

  .feature-card h3 {
    font-size: 1.5rem;
    margin-bottom: 1rem;
    color: #fff;
  }

  .feature-card p {
    color: var(--text-secondary);
  }

    .why-section {

      padding: 6rem 1.5rem;

      border-top: 1px solid var(--border-light);

    }

  

    .why-section h2 {

      font-size: 2.5rem;

      text-align: center;

      margin-bottom: 4rem;

    }

  

    .grid-2 {

      display: grid;

      grid-template-columns: 1fr 1fr;

      gap: 4rem;

    }

  

    .grid-2 h3 {

      color: var(--primary);

      margin-bottom: 1rem;

      font-size: 1.4rem;

    }

  

    .grid-2 p {

      color: var(--text-secondary);

      font-size: 1.1rem;

      line-height: 1.8;

    }

  

    @media (max-width: 768px) {

      h1 { font-size: 3rem; }

      .hero-section { padding-top: 4rem; }

      .grid-2 { grid-template-columns: 1fr; gap: 2rem; }

    }

  </style>

  