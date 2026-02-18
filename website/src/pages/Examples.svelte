<script>
  import { onMount } from "svelte";
  import { highlight, detectLang } from "../lib/highlighter";
  import { fallbackExamples, fallbackTree } from "../data/examples";
  import { Folder, FolderOpen } from "lucide-svelte";

  const API_BASE = "https://api.github.com/repos/Randware/Wasome/contents/";
  const RAW_BASE = "https://raw.githubusercontent.com/Randware/Wasome/main/";
  const DIR_CACHE_KEY = "wasome_dir_cache";
  const FILE_CACHE_KEY = "wasome_file_cache";
  const CACHE_DURATION = 24 * 60 * 60 * 1000;

  let tree = $state(structuredClone(fallbackTree));
  let openTabs = $state([]);
  let activeTabPath = $state(null);
  let fileCache = $state({});
  let dirCache = {};
  let rateLimited = $state(false);
  let loadingPaths = $state(new Set());

  function getFileIcon(name) {
    const ext = name.substring(name.lastIndexOf("."));
    switch (ext) {
      case ".waso":
        return "wasome";
      case ".toml":
        return "toml";
      case ".json":
        return "json";
      case ".md":
        return "markdown";
      default:
        return "generic";
    }
  }

  function sortItems(items) {
    return [...items].sort((a, b) => {
      if (a.type === "dir" && b.type !== "dir") return -1;
      if (a.type !== "dir" && b.type === "dir") return 1;
      return a.name.localeCompare(b.name);
    });
  }

  async function fetchDir(path) {
    if (dirCache[path]) return dirCache[path];

    const cached = localStorage.getItem(DIR_CACHE_KEY);
    if (cached) {
      try {
        const store = JSON.parse(cached);
        if (store[path] && Date.now() - store[path].ts < CACHE_DURATION) {
          dirCache[path] = store[path].data;
          return store[path].data;
        }
      } catch {}
    }

    try {
      const res = await fetch(API_BASE + path);
      if (!res.ok) throw new Error("API error");
      const items = await res.json();
      const mapped = items.map((i) => ({
        name: i.name,
        path: i.path,
        type: i.type === "dir" ? "dir" : "file",
        download_url: i.download_url,
        children: i.type === "dir" ? null : undefined,
      }));

      dirCache[path] = mapped;

      try {
        const existing = JSON.parse(
          localStorage.getItem(DIR_CACHE_KEY) || "{}",
        );
        existing[path] = { ts: Date.now(), data: mapped };
        localStorage.setItem(DIR_CACHE_KEY, JSON.stringify(existing));
      } catch {}

      return mapped;
    } catch (err) {
      console.warn("Failed to fetch dir:", path, err);
      return null;
    }
  }

  async function fetchFile(path, downloadUrl) {
    if (fileCache[path]) return fileCache[path];

    const cached = localStorage.getItem(FILE_CACHE_KEY);
    if (cached) {
      try {
        const store = JSON.parse(cached);
        if (store[path] && Date.now() - store[path].ts < CACHE_DURATION) {
          fileCache[path] = store[path].data;
          return store[path].data;
        }
      } catch {}
    }

    try {
      const url = downloadUrl || RAW_BASE + path;
      const res = await fetch(url);
      if (!res.ok) throw new Error("Fetch error");
      const text = await res.text();
      fileCache[path] = text;

      try {
        const existing = JSON.parse(
          localStorage.getItem(FILE_CACHE_KEY) || "{}",
        );
        existing[path] = { ts: Date.now(), data: text };
        localStorage.setItem(FILE_CACHE_KEY, JSON.stringify(existing));
      } catch {}

      return text;
    } catch (err) {
      console.warn("Failed to fetch file:", path, err);
      return "// Failed to load file content";
    }
  }

  function findNode(nodes, path) {
    for (const node of nodes) {
      if (node.path === path) return node;
      if (node.children) {
        const found = findNode(node.children, path);
        if (found) return found;
      }
    }
    return null;
  }

  async function toggleDir(node) {
    if (node.expanded) {
      node.expanded = false;
      tree = [...tree];
      return;
    }

    if (!node.children) {
      loadingPaths = new Set([...loadingPaths, node.path]);
      const items = await fetchDir(node.path);
      loadingPaths.delete(node.path);
      loadingPaths = new Set(loadingPaths);

      if (items) {
        node.children = sortItems(items);
      } else {
        node.children = [];
      }
    }

    node.expanded = true;
    tree = [...tree];
  }

  async function openFile(node, event) {
    const name = node.name;
    const path = node.path;
    const lang = detectLang(name);

    const existingIdx = openTabs.findIndex((t) => t.path === path);
    if (existingIdx !== -1) {
      activeTabPath = path;
      return;
    }

    let content = fileCache[path];

    const fallbackKey = name.replace(".waso", "");
    if (!content && fallbackExamples[fallbackKey]) {
      content = fallbackExamples[fallbackKey];
      fileCache[path] = content;
    }

    if (!content) {
      loadingPaths = new Set([...loadingPaths, path]);
      content = await fetchFile(path, node.download_url);
      loadingPaths.delete(path);
      loadingPaths = new Set(loadingPaths);
    }

    const tab = { name, path, content, lang };

    if (event && event.shiftKey) {
      openTabs = [...openTabs, tab];
    } else {
      if (openTabs.length === 0) {
        openTabs = [tab];
      } else {
        const activeIdx = openTabs.findIndex((t) => t.path === activeTabPath);
        if (activeIdx !== -1) {
          openTabs[activeIdx] = tab;
          openTabs = [...openTabs];
        } else {
          openTabs = [...openTabs, tab];
        }
      }
    }

    activeTabPath = path;
  }

  function closeTab(path, event) {
    event.stopPropagation();
    const idx = openTabs.findIndex((t) => t.path === path);
    if (idx === -1) return;

    openTabs = openTabs.filter((t) => t.path !== path);

    if (activeTabPath === path) {
      if (openTabs.length > 0) {
        const newIdx = Math.min(idx, openTabs.length - 1);
        activeTabPath = openTabs[newIdx].path;
      } else {
        activeTabPath = null;
      }
    }
  }

  function switchTab(path) {
    activeTabPath = path;
  }

  let activeContent = $derived(
    openTabs.find((t) => t.path === activeTabPath)?.content || null,
  );

  let activeLang = $derived(
    openTabs.find((t) => t.path === activeTabPath)?.lang || "wasome",
  );

  onMount(async () => {
    // Check GitHub API rate limit upfront
    try {
      const res = await fetch("https://api.github.com/rate_limit");
      if (res.ok) {
        const data = await res.json();
        rateLimited = data.resources.core.remaining === 0;
      }
    } catch {}

    const singleFile = tree.find((n) => n.name === "single_file");
    if (singleFile) {
      singleFile.expanded = true;
      const firstFile = singleFile.children?.[0];
      if (firstFile) {
        openFile(firstFile, null);
      }
      tree = [...tree];
    }
  });
</script>

<section class="container page-section page-animate">
  <div class="header">
    <h1>Examples</h1>
    <p>
      Explore real Wasome code from the <a
        href="https://github.com/Randware/Wasome/tree/main/docs/examples"
        target="_blank">GitHub repository</a
      >.
    </p>
    {#if rateLimited}
      <p class="rate-limit-note">
        Some folders may be unavailable — GitHub API rate limit reached.
      </p>
    {/if}
  </div>

  <div class="ide-container">
    <aside class="sidebar">
      <div class="sidebar-header">
        <svg
          width="14"
          height="14"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          ><path
            d="M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z"
          /></svg
        >
        EXPLORER
      </div>
      <div class="file-tree">
        {#each tree as node}
          {#snippet treeNode(item, depth)}
            {#if item.type === "dir"}
              <button
                class="tree-item dir"
                class:expanded={item.expanded}
                style="padding-left: {12 + depth * 16}px"
                onclick={() => toggleDir(item)}
              >
                <svg
                  class="chevron"
                  width="12"
                  height="12"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="currentColor"
                  stroke-width="2"><polyline points="9 18 15 12 9 6" /></svg
                >
                {#if item.expanded}
                  <FolderOpen
                    size={14}
                    color="var(--primary)"
                    class="folder-icon"
                  />
                {:else}
                  <Folder
                    size={14}
                    color="var(--primary)"
                    class="folder-icon"
                  />
                {/if}
                <span class="item-name">{item.name}</span>
                {#if loadingPaths.has(item.path)}
                  <span class="loading-dot"></span>
                {/if}
              </button>
              {#if item.expanded && item.children}
                {#if item.children.length === 0}
                  <div
                    class="empty-folder-hint"
                    style="padding-left: {12 + (depth + 1) * 16}px"
                  >
                    Empty folder
                  </div>
                {:else}
                  {#each sortItems(item.children) as child}
                    {@render treeNode(child, depth + 1)}
                  {/each}
                {/if}
              {/if}
            {:else}
              <button
                class="tree-item file"
                class:active={activeTabPath === item.path}
                style="padding-left: {12 + depth * 16}px"
                onclick={(e) => openFile(item, e)}
                title="Click to open, Shift+Click for new tab"
              >
                {#if getFileIcon(item.name) === "wasome"}
                  <img
                    src="/logo.png"
                    alt=""
                    class="file-type-icon wasome-icon"
                  />
                {:else if getFileIcon(item.name) === "toml"}
                  <svg
                    class="file-type-icon"
                    width="14"
                    height="14"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="#9CA3AF"
                    stroke-width="2"
                    ><path
                      d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                    /><polyline points="14 2 14 8 20 8" /><line
                      x1="8"
                      y1="13"
                      x2="16"
                      y2="13"
                    /><line x1="8" y1="17" x2="12" y2="17" /></svg
                  >
                {:else if getFileIcon(item.name) === "json"}
                  <svg
                    class="file-type-icon"
                    width="14"
                    height="14"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="#60A5FA"
                    stroke-width="2"
                    ><path
                      d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                    /><polyline points="14 2 14 8 20 8" /></svg
                  >
                {:else}
                  <svg
                    class="file-type-icon"
                    width="14"
                    height="14"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="#6B7280"
                    stroke-width="2"
                    ><path
                      d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                    /><polyline points="14 2 14 8 20 8" /></svg
                  >
                {/if}
                <span class="item-name">{item.name}</span>
              </button>
            {/if}
          {/snippet}
          {@render treeNode(node, 0)}
        {/each}
      </div>
    </aside>

    <div class="editor-area">
      {#if openTabs.length > 0}
        <div class="tab-bar">
          {#each openTabs as tab}
            <div
              class="tab"
              class:active={activeTabPath === tab.path}
              onclick={() => switchTab(tab.path)}
              onkeydown={(e) => {
                if (e.key === "Enter" || e.key === " ") switchTab(tab.path);
              }}
              role="tab"
              tabindex="0"
            >
              {#if getFileIcon(tab.name) === "wasome"}
                <img src="/logo.png" alt="" class="tab-icon wasome-icon" />
              {:else if getFileIcon(tab.name) === "toml"}
                <svg
                  class="tab-icon"
                  width="12"
                  height="12"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="#9CA3AF"
                  stroke-width="2"
                  ><path
                    d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                  /><polyline points="14 2 14 8 20 8" /></svg
                >
              {:else}
                <svg
                  class="tab-icon"
                  width="12"
                  height="12"
                  viewBox="0 0 24 24"
                  fill="none"
                  stroke="#6B7280"
                  stroke-width="2"
                  ><path
                    d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                  /><polyline points="14 2 14 8 20 8" /></svg
                >
              {/if}
              <span class="tab-name">{tab.name}</span>
              <button
                class="tab-close"
                onclick={(e) => closeTab(tab.path, e)}
                aria-label="Close tab">&times;</button
              >
            </div>
          {/each}
        </div>

        <div class="code-content">
          <pre>{#if activeContent !== null}<code
                >{@html highlight(activeContent, activeLang)}</code
              >{/if}</pre>
        </div>
      {:else}
        <div class="welcome">
          <div class="welcome-inner">
            <img src="/logo.png" alt="Wasome" class="welcome-logo" />
            <h3>Wasome Examples</h3>
            <p>Select a file from the explorer to view its code.</p>
            <p class="hint">Shift+Click to open in a new tab.</p>
          </div>
        </div>
      {/if}
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

  .header a {
    color: var(--primary);
    text-decoration: underline;
    text-underline-offset: 2px;
  }

  h1 {
    font-size: 3rem;
    margin-bottom: 1rem;
    background: linear-gradient(to right, #fff, #aaa);
    -webkit-background-clip: text;
    background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  .header p {
    color: var(--text-secondary);
    font-size: 1.2rem;
  }

  .rate-limit-note {
    font-size: 0.85rem !important;
    color: rgba(250, 204, 21, 0.7) !important;
    margin-top: 0.5rem;
  }

  .ide-container {
    display: grid;
    grid-template-columns: 260px 1fr;
    max-width: 1100px;
    margin: 0 auto;
    background: #0a0a0a;
    border: 1px solid var(--border-light);
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5);
    height: 620px;
  }

  .sidebar {
    background: #111;
    border-right: 1px solid var(--border-light);
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .sidebar-header {
    padding: 0.75rem 1rem;
    font-size: 0.7rem;
    font-weight: 700;
    letter-spacing: 0.12em;
    color: var(--text-muted);
    border-bottom: 1px solid var(--border-light);
    display: flex;
    align-items: center;
    gap: 0.5rem;
    flex-shrink: 0;
  }

  .file-tree {
    flex: 1;
    overflow-y: auto;
    overflow-x: hidden;
    padding: 4px 0;
  }

  .empty-folder-hint {
    padding: 4px 8px;
    font-size: 0.7rem;
    font-style: italic;
    color: var(--text-secondary);
    opacity: 0.7;
  }

  .tree-item {
    display: flex;
    align-items: center;
    gap: 6px;
    width: 100%;
    padding: 4px 12px;
    background: transparent;
    border: none;
    color: var(--text-secondary);
    font-family: var(--font-mono);
    font-size: 0.82rem;
    text-align: left;
    cursor: pointer;
    transition: background 0.1s;
    white-space: nowrap;
    min-height: 26px;
  }

  .tree-item:hover {
    background: rgba(255, 255, 255, 0.04);
  }

  .tree-item.file.active {
    background: rgba(250, 204, 21, 0.08);
    color: var(--text-main);
  }

  .tree-item .item-name {
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .chevron {
    flex-shrink: 0;
    transition: transform 0.15s;
    opacity: 0.5;
  }

  .dir.expanded > .chevron {
    transform: rotate(90deg);
  }

  .folder-icon {
    width: 16px;
    height: 16px;
    flex-shrink: 0;
  }

  .file-type-icon {
    width: 18px;
    height: 18px;
    flex-shrink: 0;
    object-fit: contain;
  }

  .wasome-icon {
    width: 22px;
    height: 22px;
  }

  .loading-dot {
    width: 6px;
    height: 6px;
    border-radius: 50%;
    background: var(--primary);
    animation: pulse 0.8s ease-in-out infinite;
    margin-left: auto;
    flex-shrink: 0;
  }

  @keyframes pulse {
    0%,
    100% {
      opacity: 0.3;
    }
    50% {
      opacity: 1;
    }
  }

  .editor-area {
    display: flex;
    flex-direction: column;
    background: #0a0a0a;
    min-width: 0;
    overflow: hidden;
  }

  .tab-bar {
    display: flex;
    background: #151515;
    border-bottom: 1px solid var(--border-light);
    overflow-x: auto;
    overflow-y: hidden;
    flex-shrink: 0;
    scrollbar-width: none;
  }

  .tab-bar::-webkit-scrollbar {
    display: none;
  }

  .tab {
    display: flex;
    align-items: center;
    gap: 6px;
    background: transparent;
    border: none;
    border-bottom: 2px solid transparent;
    color: var(--text-muted);
    font-family: var(--font-mono);
    font-size: 0.8rem;
    padding: 8px 12px;
    cursor: pointer;
    white-space: nowrap;
    transition: all 0.15s;
    min-width: 0;
  }

  .tab:hover {
    background: rgba(255, 255, 255, 0.03);
    color: var(--text-secondary);
  }

  .tab.active {
    background: #0a0a0a;
    color: var(--text-main);
    border-bottom-color: var(--primary);
  }

  .tab-icon {
    width: 15px;
    height: 15px;
    flex-shrink: 0;
    object-fit: contain;
  }

  .tab-name {
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .tab-close {
    background: none;
    border: none;
    color: var(--text-muted);
    font-size: 1rem;
    line-height: 1;
    padding: 0 2px;
    cursor: pointer;
    border-radius: 3px;
    opacity: 0;
    transition: all 0.1s;
    flex-shrink: 0;
    margin-left: 4px;
  }

  .tab:hover .tab-close,
  .tab.active .tab-close {
    opacity: 0.6;
  }

  .tab-close:hover {
    opacity: 1 !important;
    background: rgba(255, 255, 255, 0.1);
    color: var(--text-main);
  }

  .code-content {
    flex: 1;
    overflow: auto;
    padding: 1.5rem;
    scrollbar-width: thin;
    scrollbar-color: rgba(255, 255, 255, 0.15) transparent;
  }

  .code-content::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }

  .code-content::-webkit-scrollbar-track {
    background: transparent;
  }

  .code-content::-webkit-scrollbar-thumb {
    background: rgba(255, 255, 255, 0.15);
    border-radius: 4px;
  }

  .code-content::-webkit-scrollbar-thumb:hover {
    background: rgba(255, 255, 255, 0.25);
  }

  .code-content::-webkit-scrollbar-corner {
    background: transparent;
  }

  .code-content pre {
    margin: 0;
    counter-reset: line;
  }

  .code-content code {
    font-family: var(--font-mono);
    color: #e4e4e7;
    font-size: 0.9rem;
    line-height: 1.7;
    display: block;
    background: none;
    padding: 0;
    white-space: pre;
  }

  .welcome {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .welcome-inner {
    text-align: center;
    color: var(--text-muted);
  }

  .welcome-logo {
    width: 48px;
    height: 48px;
    opacity: 0.3;
    margin-bottom: 1.5rem;
  }

  .welcome h3 {
    color: var(--text-secondary);
    font-size: 1.3rem;
    margin-bottom: 0.5rem;
  }

  .welcome p {
    font-size: 0.95rem;
    margin-bottom: 0.25rem;
  }

  .hint {
    font-size: 0.8rem !important;
    opacity: 0.5;
    margin-top: 0.75rem !important;
  }

  @media (max-width: 800px) {
    .ide-container {
      grid-template-columns: 1fr;
      height: auto;
      min-height: 500px;
      display: flex;
      flex-direction: column;
    }

    .sidebar {
      max-height: 200px;
      border-right: none;
      border-bottom: 1px solid var(--border-light);
    }

    .editor-area {
      min-height: 400px;
    }

    .code-content {
      height: 400px;
    }
  }
</style>
