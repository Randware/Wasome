<script>
  import { route, navigate } from '../lib/router';
  
  export let activeRoute = '/';

  function go(path) {
    navigate(path);
  }
</script>

<header>
  <div class="container">
    <a href="#/" class="logo" on:click|preventDefault={() => go('/')}>
      <img src="/logo.png" alt="Wasome Logo" />
      <span>Wasome</span>
    </a>

    <nav>
      <a href="#/docs" 
         class:active={activeRoute === '/docs'} 
         on:click|preventDefault={() => go('/docs')}>
        Docs
      </a>
      <a href="#/walkthrough" 
         class:active={activeRoute === '/walkthrough'} 
         on:click|preventDefault={() => go('/walkthrough')}>
        Tour
      </a>
      <a href="#/examples" 
         class:active={activeRoute === '/examples'} 
         on:click|preventDefault={() => go('/examples')}>
        Examples
      </a>
      <a href="#/playground" 
         class:active={activeRoute === '/playground'} 
         on:click|preventDefault={() => go('/playground')}>
        Playground
      </a>
    </nav>
    
    <div class="cta">
      <a href="#/install" class="install-btn" on:click|preventDefault={() => go('/install')}>
        Install
      </a>
      <a href="https://github.com/Randware/Wasome" target="_blank" class="github-btn">
        GitHub
      </a>
    </div>
  </div>
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
  }

  .logo img {
    height: 48px;
    width: auto;
  }

  nav {
    display: flex;
    gap: 2rem;
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
  }

  nav a {
    color: var(--text-secondary);
    font-size: 0.95rem;
    font-weight: 500;
    padding: 0.5rem 0;
    position: relative;
  }

  nav a:hover {
    color: var(--text-main);
  }

  nav a.active {
    color: var(--text-main);
  }

  nav a.active::after {
    content: '';
    position: absolute;
    bottom: -2px;
    left: 0;
    width: 100%;
    height: 2px;
    background: var(--primary);
    box-shadow: 0 0 10px var(--primary);
  }

  .cta {
    display: flex;
    gap: 1rem;
    align-items: center;
  }

  .cta .install-btn {
    background: var(--primary);
    color: #000;
    padding: 0.5rem 1.2rem;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 600;
    text-decoration: none;
    transition: all 0.2s;
  }

  .cta .install-btn:hover {
    background-color: var(--primary-hover) !important;
    color: #000000 !important;
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(250, 204, 21, 0.4);
  }

  .cta .github-btn {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    color: var(--text-main);
    padding: 0.5rem 1rem;
    border-radius: 6px;
    font-size: 0.9rem;
    font-weight: 500;
  }

  .cta .github-btn:hover {
    background: var(--bg-card-hover);
    border-color: var(--border-active);
  }

  @media (max-width: 768px) {
    nav {
      display: none; /* Mobile menu could be implemented here */
    }
  }
</style>