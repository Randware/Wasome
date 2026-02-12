<script>
  import { onMount } from 'svelte';
  import { highlight } from '../lib/highlighter';
  import { fallbackExamples } from '../data/examples';

  let activeTab = $state('enum');
  let examples = $state(fallbackExamples);
  let isLoading = $state(true);

  const CACHE_KEY = 'wasome_examples_cache';
  const CACHE_DURATION = 24 * 60 * 60 * 1000; // 24 hours

  onMount(async () => {
    // 1. Check Cache
    const cached = localStorage.getItem(CACHE_KEY);
    if (cached) {
      try {
        const { timestamp, data } = JSON.parse(cached);
        if (Date.now() - timestamp < CACHE_DURATION) {
          examples = { ...fallbackExamples, ...data };
          isLoading = false;
          return;
        }
      } catch (e) {
        console.error('Cache parse error', e);
      }
    }

    // 2. Fetch from GitHub
    try {
      const response = await fetch('https://api.github.com/repos/Randware/Wasome/contents/docs/examples/single_file');
      
      if (!response.ok) {
        throw new Error('GitHub API Error');
      }

      const files = await response.json();
      const newExamples = {};

      // Fetch content for each file (limit concurrent requests if needed, but for ~12 files it's okay)
      // We use raw.githubusercontent.com to avoid hitting API rate limits for content
      const promises = files
        .filter(f => f.name.endsWith('.waso'))
        .map(async (file) => {
          const contentRes = await fetch(file.download_url); // Uses raw.githubusercontent.com
          if (contentRes.ok) {
            const text = await contentRes.text();
            const name = file.name.replace('.waso', '');
            newExamples[name] = text;
          }
        });

      await Promise.all(promises);

      // Update state and cache
      examples = { ...fallbackExamples, ...newExamples };
      localStorage.setItem(CACHE_KEY, JSON.stringify({
        timestamp: Date.now(),
        data: newExamples
      }));

    } catch (err) {
      console.warn('Failed to fetch examples from GitHub, using fallback.', err);
      // Silent failure: user stays on fallback examples
    } finally {
      isLoading = false;
    }
  });
</script>

<section class="container page-section">
  <div class="header">
    <h1>Examples</h1>
    <p>
      Learn by doing. Explore these code snippets to understand Wasome syntax.
    </p>
  </div>

  <div class="ide-container">
    <!-- Sidebar / File Explorer -->
    <aside class="sidebar">
      <div class="sidebar-header">
        <span class="icon">📂</span> FILES
      </div>
      <div class="file-list">
        {#each Object.keys(examples).sort() as key}
          <button 
            class="file-item" 
            class:active={activeTab === key}
            onclick={() => activeTab = key}
          >
            <span class="file-icon">📄</span>
            <span class="file-name">{key}.waso</span>
          </button>
        {/each}
      </div>
    </aside>

    <!-- Code Editor Area -->
    <div class="editor-area">
      <div class="editor-tabs-mobile">
        {#each Object.keys(examples).sort() as key}
          <button 
            class="mobile-tab" 
            class:active={activeTab === key}
            onclick={() => activeTab = key}
          >
            {key}.waso
          </button>
        {/each}
      </div>

      <div class="window-bar">
        <div class="tab-active">
          <span class="file-icon">📄</span>
          {activeTab}.waso
        </div>
        <div class="window-controls">
           <!-- Just decoration -->
        </div>
      </div>
      
      <div class="code-content">
        <pre>{@html highlight(examples[activeTab])}</pre>
      </div>
    </div>
  </div>
</section>

<style>
  .page-section {
    padding-top: 4rem;
    padding-bottom: 8rem;
  }

  .header {
    text-align: center;
    margin-bottom: 3rem;
  }

  h1 {
    font-size: 3rem;
    margin-bottom: 1rem;
    background: linear-gradient(to right, #fff, #aaa);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  p {
    color: var(--text-secondary);
    font-size: 1.2rem;
  }

  /* IDE Layout */
  .ide-container {
    display: grid;
    grid-template-columns: 250px 1fr;
    max-width: 1100px;
    margin: 0 auto;
    background: #0A0A0A;
    border: 1px solid var(--border-light);
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5);
    height: 600px; /* Fixed height for scrollable areas */
  }

  /* Sidebar */
  .sidebar {
    background: #111;
    border-right: 1px solid var(--border-light);
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .sidebar-header {
    padding: 1rem;
    font-size: 0.75rem;
    font-weight: 700;
    letter-spacing: 0.1em;
    color: var(--text-muted);
    border-bottom: 1px solid var(--border-light);
  }

  .file-list {
    flex: 1;
    overflow-y: auto;
    padding: 0.5rem 0;
  }

  .file-item {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    width: 100%;
    padding: 0.6rem 1rem;
    background: transparent;
    border: none;
    color: var(--text-secondary);
    font-family: var(--font-mono);
    font-size: 0.9rem;
    text-align: left;
    cursor: pointer;
    border-left: 2px solid transparent;
    transition: all 0.1s;
  }

  .file-item:hover {
    background: rgba(255, 255, 255, 0.03);
    color: var(--text-main);
  }

  .file-item.active {
    background: rgba(250, 204, 21, 0.05);
    color: var(--primary);
    border-left-color: var(--primary);
  }

  .file-icon { opacity: 0.7; }

  /* Editor Area */
  .editor-area {
    display: flex;
    flex-direction: column;
    background: #0A0A0A;
    min-width: 0; /* Prevent overflow flex child */
  }

  .window-bar {
    background: #151515;
    height: 40px;
    display: flex;
    align-items: center;
    border-bottom: 1px solid var(--border-light);
  }

  .tab-active {
    background: #0A0A0A;
    height: 100%;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0 1.5rem;
    border-right: 1px solid var(--border-light);
    border-top: 2px solid var(--primary);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    color: var(--text-main);
  }

  .code-content {
    flex: 1;
    overflow: auto;
    padding: 1.5rem;
  }

  pre {
    margin: 0;
    font-family: var(--font-mono);
    color: #e4e4e7;
    font-size: 0.95rem;
    line-height: 1.6;
  }

  /* Mobile Tabs (Hidden on Desktop) */
  .editor-tabs-mobile {
    display: none;
    overflow-x: auto;
    background: #111;
    border-bottom: 1px solid var(--border-light);
    white-space: nowrap;
    padding: 0 0.5rem;
  }

  .mobile-tab {
    background: transparent;
    border: none;
    color: var(--text-secondary);
    padding: 0.8rem 1rem;
    font-family: var(--font-mono);
    font-size: 0.85rem;
    cursor: pointer;
    border-bottom: 2px solid transparent;
  }

  .mobile-tab.active {
    color: var(--primary);
    border-bottom-color: var(--primary);
  }

  /* Responsive Design */
  @media (max-width: 800px) {
    .ide-container {
      grid-template-columns: 1fr;
      height: auto;
      min-height: 600px;
      display: flex;
      flex-direction: column;
    }

    .sidebar {
      display: none; /* Hide sidebar on mobile */
    }

    .editor-tabs-mobile {
      display: flex; /* Show horizontal tabs on mobile */
    }

    .window-bar {
      display: none; /* Hide desktop window bar on mobile to save space */
    }

    .code-content {
      height: 500px; /* Fixed height for code area on mobile */
    }
  }
</style>