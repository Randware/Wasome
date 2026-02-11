<script>
  import { navigate } from '../lib/router';
  import { slide } from 'svelte/transition';
  
  let { activeRoute = '/' } = $props();
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

<header>
  <div class="container">
    <a href="/" class="logo" onclick={(e) => go(e, '/')}>
      <img src="/logo.png" alt="Wasome Logo" />
      <span>Wasome</span>
    </a>

    <!-- Desktop Nav -->
    <nav class="desktop-nav">
      <a href="/docs" 
         class:active={activeRoute === '/docs'} 
         onclick={(e) => go(e, '/docs')}>
        Docs
      </a>
      <a href="/walkthrough" 
         class:active={activeRoute === '/walkthrough'} 
         onclick={(e) => go(e, '/walkthrough')}>
        Tour
      </a>
      <a href="/examples" 
         class:active={activeRoute === '/examples'} 
         onclick={(e) => go(e, '/examples')}>
        Examples
      </a>
      <a href="/playground" 
         class:active={activeRoute === '/playground'} 
         onclick={(e) => go(e, '/playground')}>
        Playground
      </a>
    </nav>
    
    <div class="cta desktop-cta">
      <a href="/install" class="install-btn" onclick={(e) => go(e, '/install')}>
        Install
      </a>
      <a href="https://github.com/Randware/Wasome" target="_blank" class="github-btn">
        GitHub
      </a>
    </div>

    <!-- Mobile Burger Button -->
    <button class="burger-btn" onclick={toggleMenu} aria-label="Toggle menu">
      <div class="bar" class:open={isMenuOpen}></div>
      <div class="bar" class:open={isMenuOpen}></div>
      <div class="bar" class:open={isMenuOpen}></div>
    </button>
  </div>

  <!-- Mobile Menu Dropdown -->
  {#if isMenuOpen}
    <div class="mobile-menu" transition:slide={{ duration: 300 }}>
      <nav>
        <a href="/docs" onclick={(e) => go(e, '/docs')}>Docs</a>
        <a href="/walkthrough" onclick={(e) => go(e, '/walkthrough')}>Tour</a>
        <a href="/examples" onclick={(e) => go(e, '/examples')}>Examples</a>
        <a href="/playground" onclick={(e) => go(e, '/playground')}>Playground</a>
        <div class="mobile-cta">
            <a href="/install" class="install-btn" onclick={(e) => go(e, '/install')}>Install</a>
            <a href="https://github.com/Randware/Wasome" target="_blank" class="github-btn">GitHub</a>
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
    height: var(--header-height);
    background: rgba(5, 5, 5, 0.85);
    backdrop-filter: blur(12px);
    border-bottom: 1px solid var(--border-light);
    z-index: 1000;
    display: flex;
    align-items: center;
  }

  .container {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    max-width: var(--container-width);
    margin: 0 auto;
    padding: 0 1.5rem;
  }

  .logo {
    display: flex;
    align-items: center;
    gap: 1rem;
    font-weight: 700;
    font-size: 1.5rem;
    color: var(--text-main);
    letter-spacing: -0.02em;
    z-index: 1001; /* Above mobile menu */
  }

  .logo img {
    height: 48px;
    width: auto;
  }

  /* Desktop Styles */
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
    content: '';
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
  }

  .github-btn:hover {
    background: var(--bg-card-hover);
    border-color: var(--border-active);
  }

  /* Burger Button */
  .burger-btn {
    display: none;
    flex-direction: column;
    justify-content: space-around;
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
    border-radius: 10px;
    transition: all 0.3s linear;
    position: relative;
    transform-origin: 1px;
  }

  /* Mobile Menu Styles */
  .mobile-menu {
    position: absolute;
    top: var(--header-height);
    left: 0;
    width: 100%;
    background: var(--bg-dark);
    border-bottom: 1px solid var(--border-light);
    padding: 1rem 0 2rem;
    box-shadow: 0 20px 40px rgba(0,0,0,0.5);
    overflow: hidden;
  }

  .mobile-menu nav {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 1.5rem;
  }

  .mobile-menu a {
    font-size: 1.2rem;
    color: var(--text-main);
    font-weight: 500;
  }
  
  .mobile-cta {
    display: flex;
    gap: 1rem;
    margin-top: 1rem;
  }

  /* Responsive Breakpoints */
  @media (max-width: 900px) {
    .desktop-nav, .desktop-cta {
      display: none;
    }

    .burger-btn {
      display: flex;
    }

    /* Burger Animation */
    .bar.open:nth-child(1) {
      transform: rotate(45deg);
    }
    .bar.open:nth-child(2) {
      opacity: 0;
      transform: translateX(20px);
    }
    .bar.open:nth-child(3) {
      transform: rotate(-45deg);
    }
  }
</style>