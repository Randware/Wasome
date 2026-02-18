<script>
  import { navigate } from "../lib/router";
  import { slide } from "svelte/transition";
  import { Download } from "lucide-svelte";

  let { activeRoute = "/" } = $props();
  let isMenuOpen = $state(false);

  function go(e, path) {
    e.preventDefault();
    isMenuOpen = false;
    navigate(path);
  }

  function toggleMenu() {
    isMenuOpen = !isMenuOpen;
  }
</script>

<header class:menu-open={isMenuOpen}>
  <div class="container">
    <a href="/" class="logo" onclick={(e) => go(e, "/")}>
      <img src="/logo.png" alt="Wasome Logo" />
      <span>Wasome</span>
    </a>

    <nav class="desktop-nav">
      <a
        href="/docs"
        class:active={activeRoute === "/docs"}
        onclick={(e) => go(e, "/docs")}
      >
        Docs
      </a>
      <a
        href="/walkthrough"
        class:active={activeRoute === "/walkthrough"}
        onclick={(e) => go(e, "/walkthrough")}
      >
        Tour
      </a>
      <a
        href="/examples"
        class:active={activeRoute === "/examples"}
        onclick={(e) => go(e, "/examples")}
      >
        Examples
      </a>
      <a
        href="/playground"
        class:active={activeRoute === "/playground"}
        onclick={(e) => go(e, "/playground")}
      >
        Playground
      </a>
    </nav>

    <div class="cta desktop-cta">
      <a href="/install" class="install-btn" onclick={(e) => go(e, "/install")}>
        <Download size={14} /> Install
      </a>
      <a
        href="https://github.com/Randware/Wasome"
        target="_blank"
        class="github-btn"
      >
        <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"
          ><path
            d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"
          /></svg
        >
        GitHub
      </a>
    </div>

    <button class="burger-btn" onclick={toggleMenu} aria-label="Toggle menu">
      <div class="bar" class:open={isMenuOpen}></div>
      <div class="bar" class:open={isMenuOpen}></div>
      <div class="bar" class:open={isMenuOpen}></div>
    </button>
  </div>

  {#if isMenuOpen}
    <div class="mobile-menu" transition:slide={{ duration: 250 }}>
      <nav>
        <a
          href="/docs"
          class:active={activeRoute === "/docs"}
          onclick={(e) => go(e, "/docs")}>Docs</a
        >
        <a
          href="/walkthrough"
          class:active={activeRoute === "/walkthrough"}
          onclick={(e) => go(e, "/walkthrough")}>Tour</a
        >
        <a
          href="/examples"
          class:active={activeRoute === "/examples"}
          onclick={(e) => go(e, "/examples")}>Examples</a
        >
        <a
          href="/playground"
          class:active={activeRoute === "/playground"}
          onclick={(e) => go(e, "/playground")}>Playground</a
        >
        <div class="mobile-cta">
          <a
            href="/install"
            class="install-btn"
            onclick={(e) => go(e, "/install")}><Download size={14} /> Install</a
          >
          <a
            href="https://github.com/Randware/Wasome"
            target="_blank"
            class="github-btn"
          >
            <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"
              ><path
                d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"
              /></svg
            >
            GitHub
          </a>
        </div>
      </nav>
    </div>
  {/if}
</header>

<style>
  header {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    min-height: var(--header-height);
    background: rgba(5, 5, 5, 0.85);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border-bottom: 1px solid var(--border-light);
    z-index: 1000;
    display: flex;
    flex-direction: column;
  }

  .container {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    max-width: var(--container-width);
    margin: 0 auto;
    padding: 0 1.5rem;
    min-height: var(--header-height);
  }

  .logo {
    display: flex;
    align-items: center;
    gap: 1rem;
    font-weight: 700;
    font-size: 1.5rem;
    color: var(--text-main);
    letter-spacing: -0.02em;
    z-index: 1001;
  }

  .logo img {
    height: 56px;
    width: auto;
  }

  .desktop-nav {
    display: flex;
    gap: 2rem;
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
  }

  .desktop-nav a {
    color: var(--text-secondary);
    font-size: 0.95rem;
    font-weight: 500;
    padding: 0.5rem 0;
    position: relative;
  }

  .desktop-nav a:hover {
    color: var(--text-main);
  }

  .desktop-nav a.active {
    color: var(--text-main);
  }

  .desktop-nav a.active::after {
    content: "";
    position: absolute;
    bottom: -2px;
    left: 0;
    width: 100%;
    height: 2px;
    background: var(--primary);
    box-shadow: 0 0 10px var(--primary);
  }

  .desktop-cta {
    display: flex;
    gap: 1rem;
    align-items: center;
  }

  .install-btn {
    background: var(--primary);
    color: #000;
    padding: 0.5rem 1.2rem;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 600;
    text-decoration: none;
    transition: all 0.2s;
    display: inline-flex;
    align-items: center;
    gap: 0.35rem;
  }

  .install-btn:hover {
    background-color: var(--primary-hover) !important;
    color: #000000 !important;
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(250, 204, 21, 0.4);
  }

  .github-btn {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    color: var(--text-main);
    padding: 0.5rem 1rem;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 500;
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
  }

  .github-btn:hover {
    background: var(--bg-card-hover);
    border-color: var(--border-active);
  }

  .burger-btn {
    display: none;
    flex-direction: column;
    justify-content: center;
    gap: 5px;
    width: 2rem;
    height: 2rem;
    background: transparent;
    padding: 0;
    z-index: 1001;
  }

  .bar {
    width: 100%;
    height: 2px;
    background: var(--text-main);
    border-radius: 2px;
    transition: all 0.3s ease;
    transform-origin: center;
  }

  .bar.open:nth-child(1) {
    transform: translateY(7px) rotate(45deg);
  }
  .bar.open:nth-child(2) {
    opacity: 0;
    transform: scaleX(0);
  }
  .bar.open:nth-child(3) {
    transform: translateY(-7px) rotate(-45deg);
  }

  .mobile-menu {
    width: 100%;
    padding: 0.5rem 0 2rem;
    overflow: hidden;
    border-top: 1px solid var(--border-light);
  }

  .mobile-menu nav {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.25rem;
  }

  .mobile-menu nav > a {
    font-size: 1rem;
    color: var(--text-secondary);
    font-weight: 500;
    padding: 0.7rem 1.5rem;
    border-radius: 6px;
    transition: all 0.2s;
    width: 200px;
    text-align: center;
  }

  .mobile-menu nav > a:hover,
  .mobile-menu nav > a.active {
    color: var(--text-main);
    background: rgba(255, 255, 255, 0.05);
  }

  .mobile-cta {
    display: flex;
    gap: 1rem;
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border-light);
  }

  @media (max-width: 900px) {
    .desktop-nav,
    .desktop-cta {
      display: none;
    }

    .burger-btn {
      display: flex;
    }
  }
</style>
