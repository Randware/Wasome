<script>
  import { onMount } from 'svelte';
  import { route } from './lib/router';
  import Header from './components/Header.svelte';
  import Home from './pages/Home.svelte';
  import Docs from './pages/Docs.svelte';
  import Examples from './pages/Examples.svelte';
  import Playground from './pages/Playground.svelte';
  import Walkthrough from './pages/Walkthrough.svelte';
  import Install from './pages/Install.svelte';

  let Component;

  // Simple router logic
  $: {
    switch ($route) {
      case '/':
      case '':
        Component = Home;
        break;
      case '/docs':
        Component = Docs;
        break;
      case '/walkthrough':
        Component = Walkthrough;
        break;
      case '/examples':
        Component = Examples;
        break;
      case '/playground':
        Component = Playground;
        break;
      case '/install':
        Component = Install;
        break;
      default:
        Component = Home; // Fallback to Home or 404
    }
    // Scroll to top on route change
    if (typeof window !== 'undefined') {
      window.scrollTo(0, 0);
    }
  }

  onMount(() => {
    const cleanup = route.init();
    return cleanup;
  });
</script>

<Header activeRoute={$route} />

<main>
  {#if Component}
    <svelte:component this={Component} />
  {/if}
</main>

<footer>
  <div class="container">
    <div class="footer-content">
      <div class="logo">
        <img src="/logo.png" alt="Wasome" />
        <span>Wasome</span>
      </div>
      <p class="copyright">
        &copy; 2025 The Wasome Project. <br>
        <span class="dim">Designed for the future of WebAssembly.</span>
      </p>
      <div class="links">
        <a href="https://github.com/Randware/Wasome" target="_blank">GitHub</a>
        <a href="https://twitter.com" target="_blank">Twitter</a>
      </div>
    </div>
  </div>
</footer>

<style>
  main {
    min-height: calc(100vh - var(--header-height) - 200px);
    padding-top: var(--header-height); /* Offset for fixed header */
    animation: fade-in 0.3s ease-out;
  }

  @keyframes fade-in {
    from { opacity: 0; transform: translateY(10px); }
    to { opacity: 1; transform: translateY(0); }
  }

  footer {
    border-top: 1px solid var(--border-light);
    padding: 4rem 0;
    margin-top: 6rem;
    background: linear-gradient(to bottom, var(--bg-dark), #080808);
  }

  .footer-content {
    display: flex;
    flex-direction: column;
    align-items: center;
    text-align: center;
    gap: 1.5rem;
  }

  .logo {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-weight: 700;
    color: var(--text-main);
  }

  .logo img {
    height: 24px;
    opacity: 0.8;
  }

  .copyright {
    color: var(--text-secondary);
    font-size: 0.9rem;
  }

  .dim {
    color: var(--text-muted);
    font-size: 0.85rem;
  }

  .links {
    display: flex;
    gap: 1.5rem;
  }

  .links a {
    color: var(--text-secondary);
    font-size: 0.9rem;
  }

  .links a:hover {
    color: var(--primary);
  }
</style>
