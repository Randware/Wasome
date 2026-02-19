<script>
  import { onMount } from "svelte";
  import { navigate } from "../lib/router";
  import BackgroundStars from "../components/BackgroundStars.svelte";
  import { highlight } from "../lib/highlighter";
  import {
    Zap,
    ShieldCheck,
    Code,
    Globe,
    Database,
    Wrench,
    Github,
  } from "lucide-svelte";

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

  let card;
  let glow;
  let rotateX = 0;
  let rotateY = 0;
  let glowX = 50;
  let glowY = 50;
  let glowOpacity = 0;

  let animatedSections = new Set();

  function handleMouseMove(e) {
    if (!card) return;

    const rect = card.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;

    const centerX = rect.width / 2;
    const centerY = rect.height / 2;

    rotateY = ((x - centerX) / centerX) * 5;
    rotateX = -((y - centerY) / centerY) * 5;

    glowX = (x / rect.width) * 100;
    glowY = (y / rect.height) * 100;
    glowOpacity = 1;
  }

  function handleMouseLeave() {
    rotateX = 0;
    rotateY = 0;
    glowOpacity = 0;
  }

  onMount(() => {
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            entry.target.classList.add("visible");
            observer.unobserve(entry.target);
          }
        });
      },
      { threshold: 0.1 },
    );

    document.querySelectorAll(".animate-on-scroll").forEach((el) => {
      observer.observe(el);
    });

    return () => observer.disconnect();
  });
</script>

<section class="hero-section">
  <BackgroundStars />

  <div class="content container">
    <div class="hero-grid">
      <div class="hero-text">
        <div class="badge">✨ Alpha coming soon</div>

        <h1>
          <span class="desktop-text">
            The <span class="gradient-text">WebAssembly</span><br />
            Language for Everyone
          </span>
          <span class="mobile-text">
            <span class="gradient-text">WebAssembly</span><br />
            for Everyone
          </span>
        </h1>

        <p class="subtitle">
          <span class="desktop-text">
            Wasome is a modern, high-performance language built directly for the
            web. Type-safe, expressive, and blazing fast.
          </span>
          <span class="mobile-text">
            A modern, type-safe language for the web.<br />
            Expressive and blazing fast.
          </span>
        </p>

        <div class="actions">
          <button class="btn primary" onclick={() => navigate("/install")}>
            Get Started
          </button>
          <button
            class="btn secondary"
            onclick={() => navigate("/walkthrough")}
          >
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

            <div
              class="glare"
              style="
                background: radial-gradient(circle at {glowX}% {glowY}%, rgba(255,255,255,0.1) 0%, transparent 60%);
                opacity: {glowOpacity};
              "
            ></div>
          </div>

          <div class="glow-effect"></div>
        </div>
      </div>
    </div>
  </div>
</section>

<section class="features container">
  <h2 class="section-title animate-on-scroll">What makes Wasome awesome</h2>
  <div class="features-grid">
    <div class="feature-card animate-on-scroll">
      <h3><Zap size={24} color="var(--primary)" /> Native Performance</h3>
      <p>
        Compiles directly to optimized WebAssembly with zero overhead. Your code
        runs at near-native speed.
      </p>
    </div>
    <div class="feature-card animate-on-scroll" style="animation-delay: 0.1s">
      <h3><ShieldCheck size={24} color="var(--primary)" /> Type Safe</h3>
      <p>
        Strong static typing catches errors at compile time, not runtime. No
        more undefined is not a function.
      </p>
    </div>
    <div class="feature-card animate-on-scroll" style="animation-delay: 0.2s">
      <h3><Code size={24} color="var(--primary)" /> Modern Syntax</h3>
      <p>
        Familiar syntax inspired by Rust and Go, designed for clarity and
        productivity.
      </p>
    </div>
  </div>
</section>

<section class="why-section container">
  <h2 class="animate-on-scroll">Why Wasome?</h2>
  <div class="grid-2">
    <div class="animate-on-scroll">
      <h3><Globe size={24} color="var(--primary)" /> WebAssembly First</h3>
      <p>
        Most languages treat WebAssembly as a compilation target. Wasome treats
        it as the platform. Every feature maps cleanly to Wasm instructions,
        ensuring predictable performance and tiny binary sizes.
      </p>
    </div>
    <div class="animate-on-scroll" style="animation-delay: 0.1s">
      <h3>
        <Database size={24} color="var(--primary)" /> Reference Counted Memory
      </h3>
      <p>
        Memory is reference counted. Objects get freed when nothing points to
        them anymore. No GC pauses, no tracing, no surprises. Cleanup is
        deterministic, so you always know when resources are released.
      </p>
    </div>
    <div class="animate-on-scroll" style="animation-delay: 0.15s">
      <h3><Wrench size={24} color="var(--primary)" /> Tooling that Works</h3>
      <p>
        Built-in formatter, syntax checker, and package manager. The Wasome
        ecosystem is designed to get you up and running in seconds.
      </p>
    </div>
    <div class="animate-on-scroll" style="animation-delay: 0.2s">
      <h3><Github size={24} color="var(--primary)" /> Open Source</h3>
      <p>
        Community driven development. Join us on GitHub to shape the future of
        the language.
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
    text-shadow: 0 4px 20px rgba(0, 0, 0, 0.5);
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
    background: rgba(255, 255, 255, 0.1);
    color: #fff;
    border: 1px solid rgba(255, 255, 255, 0.15);
  }

  .btn.secondary:hover {
    background: rgba(255, 255, 255, 0.15);
    border-color: rgba(255, 255, 255, 0.25);
  }

  .hero-code {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    perspective: 1000px;
  }

  .code-card-wrapper {
    position: relative;
    width: 100%;
    max-width: 550px;
    transition: transform 0.1s cubic-bezier(0.03, 0.98, 0.52, 0.99);
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
    background: radial-gradient(
      circle,
      rgba(250, 204, 21, 0.15) 0%,
      rgba(0, 0, 0, 0) 70%
    );
    z-index: 1;
    border-radius: 20px;
    pointer-events: none;
    transform: translateZ(-10px);
  }

  .window-controls {
    display: flex;
    gap: 8px;
    padding: 12px 16px;
    background: #252526;
    border-bottom: 1px solid #333;
    align-items: center;
  }

  .dot {
    width: 12px;
    height: 12px;
    border-radius: 50%;
  }
  .red {
    background-color: #ff5f56;
    border: 1px solid #e0443e;
  }
  .yellow {
    background-color: #ffbd2e;
    border: 1px solid #dea123;
  }
  .green {
    background-color: #27c93f;
    border: 1px solid #1aab29;
  }

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
    font-size: 0.9rem;
    overflow-x: auto;
  }

  /* ── Features ── */
  .features {
    padding: 8rem 0;
    border-top: 1px solid rgba(255, 255, 255, 0.03);
    position: relative;
    z-index: 2;
    background: var(--bg-dark);
    text-align: center;
  }

  .section-title {
    font-size: 2.5rem;
    margin-bottom: 4rem;
    color: var(--text-main);
  }

  .features-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 2rem;
  }

  .feature-card {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    padding: 2rem;
    border-radius: 12px;
    transition: all 0.3s;
    text-align: left;
  }

  .feature-card:hover {
    border-color: var(--primary);
    transform: translateY(-5px);
  }

  .feature-card h3 {
    font-size: 1.5rem;
    margin-bottom: 1rem;
    color: #fff;
    display: flex;
    align-items: center;
    gap: 0.6rem;
  }
  .feature-card p {
    color: var(--text-secondary);
  }

  /* ── Why section ── */
  .why-section {
    padding: 6rem 1.5rem 10rem;
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
    display: flex;
    align-items: center;
    gap: 0.6rem;
  }
  .grid-2 p {
    color: var(--text-secondary);
    font-size: 1.1rem;
    line-height: 1.8;
  }

  :global(.animate-on-scroll) {
    opacity: 0;
    transform: translateY(20px);
    transition:
      opacity 0.6s ease,
      transform 0.6s ease;
  }

  :global(.animate-on-scroll.visible) {
    opacity: 1;
    transform: translateY(0);
  }

  .mobile-text {
    display: none;
  }

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
      transform: none;
      margin: 0 auto;
      max-width: 100%;
      width: 550px;
    }

    .hero-code {
      width: 100%;
      display: flex;
      justify-content: center;
      padding: 0 1rem;
    }
  }

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

    h1 {
      font-size: 3.5rem;
    }

    .grid-2 {
      grid-template-columns: 1fr;
      gap: 2rem;
    }
  }

  @media (max-width: 600px) {
    h1 {
      font-size: 2.8rem;
    }

    .hero-section {
      padding: 6rem 0 8rem;
      min-height: auto;
    }

    .container {
      padding: 0 1rem;
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
