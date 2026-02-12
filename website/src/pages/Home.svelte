<script>
  import { navigate } from '../lib/router';
  import BackgroundStars from '../components/BackgroundStars.svelte';
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

  // Tilt Effect Variables
  let card;
  let glow;
  let rotateX = 0;
  let rotateY = 0;
  let glowX = 50;
  let glowY = 50;
  let glowOpacity = 0;

  function handleMouseMove(e) {
    if (!card) return;
    
    const rect = card.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    
    const centerX = rect.width / 2;
    const centerY = rect.height / 2;
    
    // Calculate rotation (max 10 deg)
    rotateY = ((x - centerX) / centerX) * 5;
    rotateX = -((y - centerY) / centerY) * 5;
    
    // Calculate glow position (opposite to mouse)
    glowX = (x / rect.width) * 100;
    glowY = (y / rect.height) * 100;
    glowOpacity = 1;
  }

  function handleMouseLeave() {
    rotateX = 0;
    rotateY = 0;
    glowOpacity = 0;
  }
</script>

<section class="hero-section">
  <BackgroundStars />
  
  <div class="content container">
    <div class="hero-grid">
      <div class="hero-text">
        <div class="badge">v0.1.0 Alpha is here</div>
        
        <h1>
          <span class="desktop-text">
            The <span class="gradient-text">WebAssembly</span><br>
            Language for Everyone
          </span>
          <span class="mobile-text">
            <span class="gradient-text">WebAssembly</span><br>
            for Everyone
          </span>
        </h1>
        
        <p class="subtitle">
          <span class="desktop-text">
            Wasome is a modern, high-performance language built directly for the web. 
            Type-safe, expressive, and blazing fast.
          </span>
          <span class="mobile-text">
            A modern, type-safe language for the web.<br>
            Expressive and blazing fast.
          </span>
        </p>
        
        <div class="actions">
          <button class="btn primary" onclick={() => navigate('/install')}>
            Get Started
          </button>
          <button class="btn secondary" onclick={() => navigate('/walkthrough')}>
            Take the Tour
          </button>
        </div>
      </div>

      <div class="hero-code">
        <!-- svelte-ignore a11y-no-static-element-interactions -->
        <div 
          class="code-card-wrapper"
          bind:this={card}
          onmousemove={handleMouseMove}
          onmouseleave={handleMouseLeave}
          style="transform: perspective(1000px) rotateX({rotateX}deg) rotateY({rotateY}deg);"
        >
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
            
            <!-- Dynamic Glare/Glow Overlay -->
            <div 
              class="glare"
              style="
                background: radial-gradient(circle at {glowX}% {glowY}%, rgba(255,255,255,0.1) 0%, transparent 60%);
                opacity: {glowOpacity};
              "
            ></div>
          </div>
          
          <!-- Back Glow -->
          <div class="glow-effect"></div>
        </div>
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
    padding: 8rem 0 12rem;
    min-height: 100vh;
    display: flex;
    align-items: center;
    overflow: hidden;
  }

  .content {
    width: 100%;
    position: relative;
    z-index: 1;
  }

  .hero-grid {
    display: grid;
    grid-template-columns: 1.1fr 0.9fr;
    gap: 4rem;
    align-items: center;
  }

  .hero-text {
    text-align: left;
  }

  .badge {
    background: rgba(250, 204, 21, 0.1);
    color: var(--primary);
    border: 1px solid rgba(250, 204, 21, 0.2);
    padding: 0.4rem 1rem;
    border-radius: 99px;
    font-size: 0.85rem;
    font-weight: 500;
    margin-bottom: 1.5rem;
    display: inline-block;
  }

  h1 {
    font-size: 5rem;
    font-weight: 800;
    line-height: 1.05;
    margin-bottom: 1.5rem;
    letter-spacing: -0.04em;
    color: #fff;
    text-shadow: 0 4px 20px rgba(0,0,0,0.5);
  }

  .gradient-text {
    color: #fff;
    display: inline-block;
  }

  .subtitle {
    font-size: 1.25rem;
    color: var(--text-secondary);
    margin-bottom: 2.5rem;
    line-height: 1.6;
    max-width: 90%;
  }

  .actions {
    display: flex;
    gap: 1rem;
  }

  .btn {
    padding: 1rem 2rem;
    border-radius: 8px;
    font-weight: 600;
    font-size: 1rem;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .btn.primary {
    background: var(--primary);
    color: #000;
    border: none;
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

  /* Code Section Styling */
  .hero-code {
    display: flex;
    justify-content: center;
    perspective: 1000px; /* Essential for 3D effect context */
  }

  .code-card-wrapper {
    position: relative;
    width: 100%;
    max-width: 550px;
    /* Transformation is now handled by inline JS */
    transition: transform 0.1s cubic-bezier(0.03, 0.98, 0.52, 0.99); /* Fast but smooth */
    transform-style: preserve-3d;
  }

  .code-preview {
    background: #1e1e1e;
    border: 1px solid #333;
    border-radius: 12px;
    text-align: left;
    position: relative;
    z-index: 2;
    box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5);
    overflow: hidden;
  }
  
  .glare {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    pointer-events: none;
    mix-blend-mode: overlay;
    transition: opacity 0.3s ease;
  }

  .glow-effect {
    position: absolute;
    top: -20px;
    right: -20px;
    bottom: -20px;
    left: -20px;
    background: radial-gradient(circle, rgba(250, 204, 21, 0.15) 0%, rgba(0,0,0,0) 70%);
    z-index: 1;
    border-radius: 20px;
    pointer-events: none;
    transform: translateZ(-10px); /* Push glow behind */
  }

  .window-controls {
    display: flex;
    gap: 8px;
    padding: 12px 16px;
    background: #252526;
    border-bottom: 1px solid #333;
    align-items: center;
  }

  .dot { width: 12px; height: 12px; border-radius: 50%; }
  .red { background-color: #FF5F56; border: 1px solid #E0443E; }
  .yellow { background-color: #FFBD2E; border: 1px solid #DEA123; }
  .green { background-color: #27C93F; border: 1px solid #1AAB29; }

  .window-title { margin-left: auto; font-size: 0.85rem; color: #888; font-family: var(--font-mono); }

  .code-inner { padding: 1.5rem; background: #1e1e1e; }

  pre {
    margin: 0;
    font-family: var(--font-mono);
    color: #d4d4d8;
    line-height: 1.6;
    font-size: 0.9rem;
    overflow-x: auto;
  }

  .features {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 2rem;
    padding: 8rem 0; /* Changed from margin-top: 4rem and margin-bottom: 8rem */
    border-top: 1px solid rgba(255, 255, 255, 0.03); /* Extremely subtle line */
    position: relative;
    z-index: 2;
    background: var(--bg-dark); /* Ensure it's opaque over the hero background */
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

  .feature-card h3 { font-size: 1.5rem; margin-bottom: 1rem; color: #fff; }
  .feature-card p { color: var(--text-secondary); }

  .why-section { padding: 6rem 1.5rem 10rem; border-top: 1px solid var(--border-light); }
  .why-section h2 { font-size: 2.5rem; text-align: center; margin-bottom: 4rem; }
  .grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 4rem; }
  .grid-2 h3 { color: var(--primary); margin-bottom: 1rem; font-size: 1.4rem; }
  .grid-2 p { color: var(--text-secondary); font-size: 1.1rem; line-height: 1.8; }

  .mobile-text {
    display: none;
  }

  /* Intermediate Breakpoint: Stack content but keep code visible */
  @media (max-width: 1200px) {
    .hero-grid {
      grid-template-columns: 1fr;
      text-align: center;
      gap: 3rem;
    }

    .hero-text {
      display: flex;
      flex-direction: column;
      align-items: center;
      text-align: center;
    }
    
    .code-card-wrapper {
      transform: none; /* Disable tilt on tablet/smaller to prevent overflow issues */
      margin: 0 auto;
      max-width: 100%; /* Ensure it doesn't overflow */
      width: 550px; /* Target width */
    }
    
    .hero-code {
      width: 100%;
      display: flex;
      justify-content: center;
      padding: 0 1rem;
    }
  }

  /* Mobile Breakpoint: Hide code, optimized text */
  @media (max-width: 900px) {
    .features {
      padding-bottom: 2rem;
      margin-bottom: 4rem;
    }

    .why-section {
      padding-top: 4rem !important;
      padding-bottom: 6rem !important;
      border-top: 1px solid var(--border-light);
    }

    .hero-code {
      display: none;
    }

    .desktop-text {
      display: none;
    }
    
    .mobile-text {
      display: inline;
    }

    h1 { font-size: 3.5rem; }
    
    .grid-2 { grid-template-columns: 1fr; gap: 2rem; }
  }

  /* Small Mobile Optimizations */
  @media (max-width: 600px) {
    h1 {
      font-size: 2.8rem;
    }
    
    .hero-section {
      padding: 6rem 0 8rem; /* Reduced padding */
      min-height: auto; /* Remove 100vh constraint */
    }

    .container {
      padding: 0 1rem; /* Smaller horizontal padding */
    }

    .subtitle {
      font-size: 1.1rem;
    }
    
    .actions {
      flex-direction: column;
      width: 100%;
      max-width: 300px;
    }
    
    .btn {
      width: 100%;
      text-align: center;
    }
  }
</style>