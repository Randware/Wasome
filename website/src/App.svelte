<script>
  import { onMount } from "svelte";
  import { route } from "./lib/router";
  import Header from "./components/Header.svelte";
  import Home from "./pages/Home.svelte";
  import Docs from "./pages/Docs.svelte";
  import Examples from "./pages/Examples.svelte";
  import Playground from "./pages/Playground.svelte";
  import Walkthrough from "./pages/Walkthrough.svelte";
  import Install from "./pages/Install.svelte";

  let Component = $derived.by(() => {
    switch ($route) {
      case "/":
      case "":
        return Home;
      case "/docs":
        return Docs;
      case "/walkthrough":
        return Walkthrough;
      case "/examples":
        return Examples;
      case "/playground":
        return Playground;
      case "/install":
        return Install;
      default:
        return Home;
    }
  });

  $effect(() => {
    $route;
    if (typeof window !== "undefined") {
      window.scrollTo(0, 0);
    }
  });

  onMount(() => {
    const cleanup = route.init();
    return cleanup;
  });
</script>

<Header activeRoute={$route} />

<main>
  {#if Component}
    <Component />
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
        &copy; 2026 The Wasome Project. <br />
        <span class="dim">Designed for the future of WebAssembly.</span>
      </p>
      <div class="links">
        <a href="https://github.com/Randware/Wasome" target="_blank">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"
            ><path
              d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"
            /></svg
          >
          GitHub
        </a>
        <a href="https://www.youtube.com/@randware" target="_blank">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor"
            ><path
              d="M23.498 6.186a3.016 3.016 0 0 0-2.122-2.136C19.505 3.545 12 3.545 12 3.545s-7.505 0-9.377.505A3.017 3.017 0 0 0 .502 6.186C0 8.07 0 12 0 12s0 3.93.502 5.814a3.016 3.016 0 0 0 2.122 2.136c1.871.505 9.376.505 9.376.505s7.505 0 9.377-.505a3.015 3.015 0 0 0 2.122-2.136C24 15.93 24 12 24 12s0-3.93-.502-5.814zM9.545 15.568V8.432L15.818 12l-6.273 3.568z"
            /></svg
          >
          YouTube
        </a>
      </div>
    </div>
  </div>
</footer>

<style>
  main {
    min-height: calc(100vh - var(--header-height) - 200px);
    padding-top: var(--header-height);
    animation: fade-in 0.3s ease-out;
  }

  @keyframes fade-in {
    from {
      opacity: 0;
      transform: translateY(10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  footer {
    border-top: 1px solid var(--border-light);
    padding: 4rem 0;
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
    display: inline-flex;
    align-items: center;
    gap: 0.4rem;
  }

  .links a:hover {
    color: var(--primary);
  }
</style>
