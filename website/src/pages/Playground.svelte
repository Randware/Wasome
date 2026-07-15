<script>
  import CodeEditor from "../components/CodeEditor.svelte";
  import { detectLang } from "../lib/highlighter";
  import {
    Play,
    RotateCcw,
    FilePlus,
    FolderPlus,
    X,
    Folder,
    FolderOpen,
  } from "lucide-svelte";
  import { onMount } from "svelte";
  import { Turnstile } from "svelte-turnstile";
  import { compileViaWebSocket } from "../lib/ws-compile.js";
  import { createWasiShim, WasiExit } from "../lib/wasi-shim.js";
  function stripAnsi(str) {
    return str.replace(/\x1b\[[0-9;]*[a-zA-Z]/g, '');
  }

  const DEFAULT_MAIN = `fn main() {
    // Write your Wasome code here
    s32 x <- 10
    s32 y <- 20
    s32 z <- x + y
}`;

  const DEFAULT_TOML = `[project]
name = "test"
version = "0.1.0"

[bin]
bin = "test"
entry = "src/main.waso"`;

  function createDefaultTree() {
    return [
      {
        name: "src",
        path: "src",
        type: "dir",
        expanded: true,
        children: [{ name: "main.waso", path: "src/main.waso", type: "file" }],
      },
      { name: "waso.toml", path: "waso.toml", type: "file" },
    ];
  }

  function createDefaultFiles() {
    return {
      "src/main.waso": DEFAULT_MAIN,
      "waso.toml": DEFAULT_TOML,
    };
  }

  const STORAGE_KEY = "wasome-playground";

  function loadSavedState() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (raw) {
        const saved = JSON.parse(raw);
        return {
          tree: saved.tree || createDefaultTree(),
          files: saved.files || createDefaultFiles(),
          openTabs: saved.openTabs || [],
          activeTabPath: saved.activeTabPath || null,
        };
      }
    } catch {}
    return null;
  }

  function saveState() {
    try {
      localStorage.setItem(
        STORAGE_KEY,
        JSON.stringify({
          tree: $state.snapshot(tree),
          files: $state.snapshot(files),
          openTabs: $state.snapshot(openTabs),
          activeTabPath,
        }),
      );
    } catch {}
  }

  const saved = loadSavedState();
  let tree = $state(saved ? saved.tree : createDefaultTree());
  let files = $state(saved ? saved.files : createDefaultFiles());
  let openTabs = $state(saved ? saved.openTabs : []);
  let activeTabPath = $state(saved ? saved.activeTabPath : null);
  let output = $state("// Output will appear here...");
  let hasValidSession = $state(false);
  let turnstileToken = $state(null);
  let turnstileRef = $state(null);
  let isRunning = $state(false);
  let isVerifying = $state(false);
  let compilationEnabled = $state(true);
  let maintenanceMode = $state(false);
  let statusLoaded = $state(false);

  $effect(() => {
    // Access all reactive state to track changes
    void JSON.stringify(tree);
    void JSON.stringify(files);
    void JSON.stringify(openTabs);
    void activeTabPath;
    saveState();
  });

  // Modal state
  let modal = $state({
    open: false,
    type: "prompt", // "prompt" | "confirm"
    title: "",
    message: "",
    placeholder: "",
    value: "",
    onConfirm: null,
    error: "",
    confirmLabel: "Create",
  });

  let modalInputEl = $state(null);

  function showPrompt(title, placeholder, onConfirm) {
    modal = {
      open: true,
      type: "prompt",
      title,
      message: "",
      placeholder,
      value: "",
      onConfirm,
      error: "",
      confirmLabel: "Create",
    };
    setTimeout(() => modalInputEl?.focus(), 50);
  }

  function showConfirm(title, message, onConfirm, confirmLabel = "Delete") {
    modal = {
      open: true,
      type: "confirm",
      title,
      message,
      placeholder: "",
      value: "",
      onConfirm,
      error: "",
      confirmLabel,
    };
  }

  function modalConfirm() {
    if (modal.type === "prompt") {
      const val = modal.value.trim();
      if (!val) {
        modal.error = "Name cannot be empty.";
        return;
      }
      modal.onConfirm?.(val);
    } else {
      modal.onConfirm?.();
    }
    modal = { ...modal, open: false };
  }

  function modalCancel() {
    modal = { ...modal, open: false };
  }

  function modalKeydown(e) {
    if (e.key === "Enter") modalConfirm();
    if (e.key === "Escape") modalCancel();
  }

  // Derived
  let activeContent = $derived(
    activeTabPath && files[activeTabPath] !== undefined
      ? files[activeTabPath]
      : null,
  );
  let activeLang = $derived(
    activeTabPath ? detectLang(activeTabPath.split("/").pop()) : "wasome",
  );

  // Helpers
  function sortItems(items) {
    return [...items].sort((a, b) => {
      if (a.type === "dir" && b.type !== "dir") return -1;
      if (a.type !== "dir" && b.type === "dir") return 1;
      return a.name.localeCompare(b.name);
    });
  }

  function getFileIcon(name) {
    const ext = name.substring(name.lastIndexOf("."));
    switch (ext) {
      case ".waso":
        return "wasome";
      case ".toml":
        return "toml";
      case ".json":
        return "json";
      default:
        return "generic";
    }
  }

  function pathExists(path) {
    if (files[path] !== undefined) return true;
    function check(nodes) {
      for (const n of nodes) {
        if (n.path === path) return true;
        if (n.children && check(n.children)) return true;
      }
      return false;
    }
    return check(tree);
  }

  // Tree actions
  function toggleDir(node) {
    node.expanded = !node.expanded;
    tree = [...tree];
  }

  function openFile(node) {
    const path = node.path;
    if (files[path] === undefined) files[path] = "";
    if (!openTabs.find((t) => t.path === path)) {
      openTabs = [...openTabs, { name: node.name, path }];
    }
    activeTabPath = path;
  }

  function closeTab(path, event) {
    event.stopPropagation();
    const idx = openTabs.findIndex((t) => t.path === path);
    openTabs = openTabs.filter((t) => t.path !== path);
    if (activeTabPath === path) {
      activeTabPath =
        openTabs.length > 0
          ? openTabs[Math.min(idx, openTabs.length - 1)].path
          : null;
    }
  }

  function switchTab(path) {
    activeTabPath = path;
  }

  // Create
  function getTotalFiles(nodes) {
    let count = 0;
    for (const n of nodes) {
      if (n.type === "file") count++;
      if (n.children) count += getTotalFiles(n.children);
    }
    return count;
  }

  function getDepth(path) {
    if (!path) return 0;
    return path.split("/").length;
  }

  function createFileAt(parentPath, parentList) {
    if (getTotalFiles(tree) >= 30) {
      modal.error = "Max file limit (30) reached.";
      modal.open = true;
      return;
    }
    showPrompt("New File", "e.g. utils.waso", (name) => {
      const filePath = parentPath ? parentPath + "/" + name : name;
      if (pathExists(filePath)) {
        // re-show with error
        modal.error = "A file with that name already exists.";
        modal.open = true;
        return;
      }
      parentList.push({ name, path: filePath, type: "file" });
      files[filePath] = "";
      tree = [...tree];
      openFile({ name, path: filePath });
    });
  }

  function createFolderAt(parentPath, parentList) {
    if (getDepth(parentPath) >= 5) {
      modal.error = "Max folder depth (5) reached.";
      modal.open = true;
      return;
    }
    showPrompt("New Folder", "e.g. lib", (name) => {
      if (parentList.find((n) => n.name === name && n.type === "dir")) {
        modal.error = "A folder with that name already exists.";
        modal.open = true;
        return;
      }
      parentList.push({
        name,
        path: parentPath ? parentPath + "/" + name : name,
        type: "dir",
        expanded: true,
        children: [],
      });
      tree = [...tree];
    });
  }

  function confirmDelete(node, parentList) {
    const label =
      node.type === "dir"
        ? `folder "${node.name}" and all its contents`
        : `file "${node.name}"`;
    showConfirm("Delete", `Are you sure you want to delete ${label}?`, () => {
      doDelete(node, parentList);
    });
  }

  function doDelete(node, parentList) {
    function collectPaths(n) {
      if (n.type === "file") return [n.path];
      if (n.children) return n.children.flatMap((c) => collectPaths(c));
      return [];
    }
    const pathsToRemove = collectPaths(node);
    const idx = parentList.indexOf(node);
    if (idx !== -1) parentList.splice(idx, 1);
    for (const p of pathsToRemove) {
      delete files[p];
      openTabs = openTabs.filter((t) => t.path !== p);
      if (activeTabPath === p) {
        activeTabPath = openTabs.length > 0 ? openTabs[0].path : null;
      }
    }
    tree = [...tree];
    files = { ...files };
  }

  // Toolbar
  function resetProject() {
    showConfirm(
      "Reset Project",
      "Reset all files to defaults? Any changes will be lost.",
      () => {
        tree = createDefaultTree();
        files = createDefaultFiles();
        openTabs = [];
        activeTabPath = null;
        output = "// Output will appear here...";
        try {
          localStorage.removeItem(STORAGE_KEY);
        } catch {}
      },
      "Reset",
    );
  }

  async function runCode() {
    if (maintenanceMode) {
      output = "Playground is under maintenance. Please try again later.";
      return;
    }
    if (!compilationEnabled) {
      output = "Compilation is currently disabled.";
      return;
    }
    if (!activeContent && activeContent !== "") {
      output = "No file is open. Select a file to run.";
      return;
    }
    isRunning = true;
    output = "";

    if (!hasValidSession) {
      const rawSiteKey = import.meta.env.PUBLIC_TURNSTILE_SITE_KEY;
      const isDummy = !rawSiteKey || rawSiteKey === "1x00000000000000000000AA" || rawSiteKey === "undefined" || rawSiteKey === "null" || rawSiteKey === "";
      if (isDummy) {
        await doCompile("dummy-bypass");
        return;
      }
      if (turnstileToken) {
        await doCompile(turnstileToken);
        return;
      }
      isVerifying = true;
      turnstileToken = null;
      if (turnstileRef) {
        turnstileRef.reset();
      } else {
        // Fallback in case of race condition: Svelte will mount it, but we can wait briefly
        setTimeout(() => {
          if (turnstileRef) turnstileRef.reset();
        }, 100);
      }
      return;
    }
    const rawSiteKey = import.meta.env.PUBLIC_TURNSTILE_SITE_KEY;
    const isDummy = !rawSiteKey || rawSiteKey === "1x00000000000000000000AA" || rawSiteKey === "undefined" || rawSiteKey === "null" || rawSiteKey === "";
    const token = isDummy ? "dummy-bypass" : null;
    await doCompile(token);
  }

  function handleTurnstileCallback(e) {
    turnstileToken = e.detail.token;
    isVerifying = false;
    if (isRunning) {
      doCompile(turnstileToken);
    }
  }

  function handleTurnstileError() {
    isVerifying = false;
    output = "Security check failed.";
    isRunning = false;
  }

  async function doCompile(token) {
    let dotCount = 0;
    output = "Compiling";
    
    const intervalId = setInterval(() => {
      dotCount = (dotCount + 1) % 4;
      output = "Compiling" + ".".repeat(dotCount);
    }, 500);

    const compilerLogs = [];
    let compiledSuccess = false;

    await compileViaWebSocket({
      files: $state.snapshot(files),
      entry: "src/main.waso",
      turnstileToken: token,
      onLog(data) {
        compilerLogs.push(stripAnsi(data));
      },
      onWasm(wasmBytes) {
        clearInterval(intervalId);
        compiledSuccess = true;
        output = "";
        executeWasm(wasmBytes);
      },
      onError(message, status) {
        clearInterval(intervalId);
        if (status === 401 || status === 403) {
          hasValidSession = false;
          turnstileToken = null;
          isVerifying = true;
          if (turnstileRef) {
            turnstileRef.reset();
          } else {
            setTimeout(() => {
              if (turnstileRef) turnstileRef.reset();
            }, 100);
          }
          output = "Security check failed. Please try again.\n";
        } else if (status === 429) {
          output = "Too many requests. Please slow down.\n";
        } else {
          output = stripAnsi(message) + "\n";
        }
      },
      onDone(success) {
        clearInterval(intervalId);
        if (success) {
          hasValidSession = true;
        } else {
          if (!compiledSuccess) {
            const cleanLogs = compilerLogs.filter(line => 
              !line.includes("Compiling project at /workspace") && 
              !line.includes("Built /workspace")
            );
            output = cleanLogs.join('\n');
          }
        }
        if (turnstileRef) turnstileRef.reset();
        isRunning = false;
      }
    });

    clearInterval(intervalId);
    isRunning = false;
  }

  async function executeWasm(wasmBytes) {
    output = "";
    const wasi = createWasiShim();
    try {
      const module = await WebAssembly.compile(wasmBytes);
      const instance = await WebAssembly.instantiate(module, wasi.imports);
      if (instance.exports.memory) wasi.setMemory(instance.exports.memory);
      try {
        instance.exports._start();
      } catch (e) {
        if (!(e instanceof WasiExit)) throw e;
      }
      const result = wasi.getOutput();
      if (result.stdout) output += result.stdout;
      if (result.stderr) output += "\n[stderr] " + result.stderr;
      if (result.exitCode !== null && result.exitCode !== 0) {
        output += "\n[Process exited with code " + result.exitCode + "]";
      }
    } catch (e) {
      if (!(e instanceof WasiExit)) {
        output += "[WASM execution error] " + e.message + "\n";
      }
    }
  }

  onMount(() => {
    if (!saved) {
      openFile({ name: "main.waso", path: "src/main.waso" });
    }
    fetch('/api/status')
      .then(r => r.json())
      .then(data => {
        compilationEnabled = data.compilation_enabled;
        maintenanceMode = data.maintenance_mode;
        statusLoaded = true;
      })
      .catch(() => { statusLoaded = true; });
  });
</script>

{#if statusLoaded && !compilationEnabled}
  <div class="disabled-notice page-animate">
    <div class="disabled-inner">
      <img src="/logo.png" alt="Wasome" class="disabled-logo" />
      <h2>Playground Unavailable</h2>
      <p>The playground is currently disabled. Please check back later.</p>
    </div>
  </div>
{:else}
<!-- Modal Overlay -->
{#if modal.open}
  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="modal-overlay" onclick={modalCancel}>
    <div class="modal-box" onclick={(e) => e.stopPropagation()}>
      <div class="modal-header">
        <span class="modal-title">{modal.title}</span>
        <button class="modal-close-btn" onclick={modalCancel}
          ><X size={16} /></button
        >
      </div>

      {#if modal.type === "prompt"}
        <div class="modal-body">
          <input
            bind:this={modalInputEl}
            bind:value={modal.value}
            class="modal-input"
            placeholder={modal.placeholder}
            onkeydown={modalKeydown}
            spellcheck="false"
          />
          {#if modal.error}
            <p class="modal-error">{modal.error}</p>
          {/if}
        </div>
      {:else}
        <div class="modal-body">
          <p class="modal-message">{modal.message}</p>
        </div>
      {/if}

      <div class="modal-actions">
        <button class="modal-btn cancel" onclick={modalCancel}>Cancel</button>
        <button class="modal-btn confirm" onclick={modalConfirm}>
          {modal.confirmLabel}
        </button>
      </div>
    </div>
  </div>
{/if}

<!-- Main Layout -->
<div class="playground-container page-animate">
  <div class="toolbar">
    <div class="title">Wasome Playground</div>
    <div class="controls">
      <button class="reset-btn" onclick={resetProject} title="Reset project">
        <RotateCcw size={14} />
        Reset
      </button>
      <button class="run-btn" onclick={runCode} disabled={isRunning}>
        {#if isRunning}
          <span class="spinner"></span> Running...
        {:else}
          <Play size={14} /> Run
        {/if}
      </button>
    </div>
  </div>

  <div class="editor-layout">
    <!-- Sidebar -->
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
        <span>EXPLORER</span>
        <div class="sidebar-actions">
          <button
            class="sidebar-action-btn"
            onclick={() => createFileAt("", tree)}
            title="New File"
          >
            <FilePlus size={14} />
          </button>
          <button
            class="sidebar-action-btn"
            onclick={() => createFolderAt("", tree)}
            title="New Folder"
          >
            <FolderPlus size={14} />
          </button>
        </div>
      </div>
      <div class="file-tree">
        {#each sortItems(tree) as node}
          {#snippet treeNode(item, depth, parentList)}
            {#if item.type === "dir"}
              <div class="tree-item-row">
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
                      size={16}
                      color="var(--primary)"
                      class="folder-icon"
                    />
                  {:else}
                    <Folder
                      size={16}
                      color="var(--primary)"
                      class="folder-icon"
                    />
                  {/if}
                  <span class="item-name">{item.name}</span>
                </button>
                <div class="row-actions">
                  <button
                    class="row-action-btn"
                    onclick={(e) => {
                      e.stopPropagation();
                      createFileAt(item.path, item.children || []);
                    }}
                    title="New File in {item.name}"
                  >
                    <FilePlus size={12} />
                  </button>
                  <button
                    class="row-action-btn"
                    onclick={(e) => {
                      e.stopPropagation();
                      createFolderAt(item.path, item.children || []);
                    }}
                    title="New Folder in {item.name}"
                  >
                    <FolderPlus size={12} />
                  </button>
                  <button
                    class="row-action-btn delete"
                    onclick={(e) => {
                      e.stopPropagation();
                      confirmDelete(item, parentList);
                    }}
                    title="Delete"
                  >
                    <X size={12} />
                  </button>
                </div>
              </div>
              {#if item.expanded && item.children}
                {#each sortItems(item.children) as child}
                  {@render treeNode(child, depth + 1, item.children)}
                {/each}
              {/if}
            {:else}
              <div class="tree-item-row">
                <button
                  class="tree-item file"
                  class:active={activeTabPath === item.path}
                  style="padding-left: {12 + depth * 16}px"
                  onclick={() => openFile(item)}
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
                <div class="row-actions">
                  <button
                    class="row-action-btn delete"
                    onclick={(e) => {
                      e.stopPropagation();
                      confirmDelete(item, parentList);
                    }}
                    title="Delete"
                  >
                    <X size={12} />
                  </button>
                </div>
              </div>
            {/if}
          {/snippet}
          {@render treeNode(node, 0, tree)}
        {/each}
      </div>
    </aside>

    <!-- Editor + Output -->
    <div class="main-area">
      <div class="editor-pane">
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
                  <img
                    src="/logo.png"
                    alt=""
                    class="tab-icon wasome-tab-icon"
                  />
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
                  aria-label="Close tab"><X size={12} /></button
                >
              </div>
            {/each}
          </div>
          <div class="editor-wrapper">
            {#if activeTabPath && files[activeTabPath] !== undefined}
              {#key activeTabPath}
                <CodeEditor
                  bind:code={files[activeTabPath]}
                  lang={activeLang}
                />
              {/key}
            {/if}
          </div>
        {:else}
          <div class="welcome">
            <div class="welcome-inner">
              <img src="/logo.png" alt="Wasome" class="welcome-logo" />
              <h3>Wasome Playground</h3>
              <p>Select a file from the explorer to start coding.</p>
            </div>
          </div>
        {/if}
      </div>

      <div class="output-pane">
        <div class="pane-header">Console Output</div>
        <pre>{output}</pre>
      </div>
    </div>
  </div>
</div>

<div 
  class="turnstile-overlay" 
  class:visible={isVerifying}
  role="dialog"
  aria-modal="true"
  aria-labelledby="turnstile-title"
  aria-describedby="turnstile-desc"
>
  <div class="turnstile-modal">
    <h3 id="turnstile-title" class="modal-title">Security Check</h3>
    <p id="turnstile-desc" class="modal-desc">Please complete the verification below to build and run your project.</p>
    <div class="turnstile-widget">
      <Turnstile
        siteKey={import.meta.env.PUBLIC_TURNSTILE_SITE_KEY || "1x00000000000000000000AA"}
        theme="dark"
        size="normal"
        appearance="interaction-only"
        on:callback={handleTurnstileCallback}
        on:error={handleTurnstileError}
        bind:this={turnstileRef}
      />
    </div>
    <button class="cancel-button" onclick={() => { isRunning = false; isVerifying = false; }}>
      Cancel
    </button>
  </div>
</div>
{/if}

<style>
  .turnstile-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    height: 100vh;
    background: rgba(0, 0, 0, 0.75);
    backdrop-filter: blur(8px);
    z-index: 10000;
    display: flex;
    justify-content: center;
    align-items: center;
    opacity: 0;
    visibility: hidden;
    pointer-events: none;
    transition: opacity 0.2s ease-out, visibility 0.2s ease-out;
  }

  .turnstile-overlay.visible {
    opacity: 1;
    visibility: visible;
    pointer-events: auto;
  }

  .turnstile-modal {
    box-sizing: border-box;
    background: rgba(18, 18, 18, 0.95);
    border: 1px solid rgba(255, 255, 255, 0.08);
    border-radius: 12px;
    padding: 2rem;
    width: 380px;
    max-width: 90%;
    text-align: center;
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.5);
    transform: scale(0.95);
    transition: transform 0.2s cubic-bezier(0.34, 1.56, 0.64, 1);
  }

  .turnstile-overlay.visible .turnstile-modal {
    transform: scale(1);
  }

  .turnstile-modal * {
    box-sizing: border-box;
  }

  .modal-title {
    color: var(--text-primary, #fff);
    font-size: 1.25rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
  }

  .modal-desc {
    color: var(--text-muted, #999);
    font-size: 0.875rem;
    line-height: 1.4;
    margin-bottom: 1.5rem;
  }

  .turnstile-widget {
    display: flex;
    justify-content: center;
    margin-bottom: 1.5rem;
  }

  .cancel-button {
    background: transparent;
    border: 1px solid rgba(255, 255, 255, 0.15);
    color: var(--text-secondary, #ccc);
    padding: 0.5rem 1.5rem;
    border-radius: 6px;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .cancel-button:hover {
    background: rgba(255, 255, 255, 0.05);
    color: #fff;
    border-color: rgba(255, 255, 255, 0.3);
  }


  @media (max-width: 400px) {
    .turnstile-modal {
      padding: 1.5rem 1rem;
      max-width: 95%;
    }
  }

  .playground-container {
    height: calc(100vh - var(--header-height));
    display: flex;
    flex-direction: column;
    background: #000;
  }

  /* Toolbar */
  .toolbar {
    height: 50px;
    background: #0a0a0a;
    border-bottom: 1px solid var(--border-light);
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 1.5rem;
    flex-shrink: 0;
  }

  .title {
    font-weight: 600;
    color: var(--text-secondary);
    font-size: 0.95rem;
  }

  .controls {
    display: flex;
    gap: 0.75rem;
    align-items: center;
  }

  .run-btn {
    background: var(--primary);
    color: #000;
    font-weight: 600;
    padding: 0.4rem 1.2rem;
    border-radius: 4px;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.85rem;
    transition: all 0.2s;
  }

  .run-btn:hover:not(:disabled) {
    background: var(--primary-dim);
    box-shadow: 0 0 15px rgba(250, 204, 21, 0.2);
  }

  .run-btn:disabled {
    opacity: 0.7;
    cursor: not-allowed;
    background: #333;
    color: #888;
  }

  .reset-btn {
    background: transparent;
    border: 1px solid var(--border-light);
    color: var(--text-secondary);
    padding: 0.4rem 1rem;
    border-radius: 4px;
    display: flex;
    align-items: center;
    gap: 0.4rem;
    font-size: 0.85rem;
    transition: all 0.2s;
  }

  .reset-btn:hover {
    border-color: var(--border-active);
    color: var(--text-main);
    background: rgba(255, 255, 255, 0.03);
  }

  .spinner {
    width: 14px;
    height: 14px;
    border: 2px solid rgba(0, 0, 0, 0.3);
    border-top-color: #fff;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  /* Layout */
  .editor-layout {
    flex: 1;
    display: grid;
    grid-template-columns: 240px 1fr;
    overflow: hidden;
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
    padding: 0.6rem 0.8rem;
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

  .sidebar-actions {
    margin-left: auto;
    display: flex;
    gap: 2px;
  }

  .sidebar-action-btn {
    background: transparent;
    color: var(--text-muted);
    padding: 3px;
    border-radius: 3px;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: all 0.15s;
  }

  .sidebar-action-btn:hover {
    background: rgba(255, 255, 255, 0.08);
    color: var(--text-main);
  }

  .file-tree {
    flex: 1;
    overflow-y: auto;
    overflow-x: hidden;
    padding: 4px 0;
    scrollbar-width: thin;
    scrollbar-color: rgba(255, 255, 255, 0.1) transparent;
  }

  /* Tree items */
  .tree-item-row {
    display: flex;
    align-items: center;
    position: relative;
  }

  .tree-item-row:hover .row-actions {
    opacity: 1;
  }

  .row-actions {
    position: absolute;
    right: 4px;
    display: flex;
    gap: 1px;
    opacity: 0;
    transition: opacity 0.15s;
    z-index: 2;
  }

  .row-action-btn {
    background: transparent;
    color: var(--text-muted);
    padding: 2px;
    border-radius: 3px;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: all 0.15s;
  }

  .row-action-btn:hover {
    background: rgba(255, 255, 255, 0.08);
    color: var(--text-main);
  }

  .row-action-btn.delete:hover {
    background: rgba(239, 68, 68, 0.15);
    color: #ef4444;
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

  /* Main area */
  .main-area {
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .editor-pane {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
    border-bottom: 1px solid var(--border-light);
  }

  .editor-wrapper {
    flex: 1;
    position: relative;
    overflow: hidden;
  }

  /* Tab bar */
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

  .wasome-tab-icon {
    width: 18px;
    height: 18px;
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

  /* Output pane */
  .output-pane {
    min-height: 160px;
    max-height: 220px;
    background: #050505;
    display: flex;
    flex-direction: column;
    flex-shrink: 0;
  }

  .pane-header {
    padding: 0.5rem 1rem;
    background: #111;
    font-size: 0.7rem;
    font-weight: 700;
    letter-spacing: 0.1em;
    color: var(--text-muted);
    text-transform: uppercase;
    border-bottom: 1px solid var(--border-light);
    flex-shrink: 0;
  }

  pre {
    flex: 1;
    padding: 1rem 1.5rem;
    color: var(--text-secondary);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    white-space: pre-wrap;
    overflow-y: auto;
    margin: 0;
    scrollbar-width: thin;
    scrollbar-color: rgba(255, 255, 255, 0.1) transparent;
  }

  /* Welcome */
  .welcome {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    background: #0a0a0a;
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
  }

  /* Modal */
  .modal-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.6);
    backdrop-filter: blur(4px);
    -webkit-backdrop-filter: blur(4px);
    z-index: 9999;
    display: flex;
    align-items: center;
    justify-content: center;
    animation: fadeIn 0.15s ease;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }

  .modal-box {
    background: #141414;
    border: 1px solid var(--border-light);
    border-radius: 10px;
    width: 380px;
    max-width: 90vw;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.6);
    animation: slideUp 0.2s ease;
    overflow: hidden;
  }

  @keyframes slideUp {
    from {
      opacity: 0;
      transform: translateY(12px) scale(0.97);
    }
    to {
      opacity: 1;
      transform: translateY(0) scale(1);
    }
  }

  .modal-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 1rem 1.25rem;
    border-bottom: 1px solid var(--border-light);
  }

  .modal-title {
    font-weight: 600;
    font-size: 0.95rem;
    color: var(--text-main);
  }

  .modal-close-btn {
    background: transparent;
    color: var(--text-muted);
    padding: 4px;
    border-radius: 4px;
    display: flex;
    align-items: center;
    transition: all 0.15s;
  }

  .modal-close-btn:hover {
    background: rgba(255, 255, 255, 0.08);
    color: var(--text-main);
  }

  .modal-body {
    padding: 1.25rem;
  }

  .modal-input {
    width: 100%;
    background: #0a0a0a;
    border: 1px solid var(--border-light);
    color: var(--text-main);
    padding: 0.6rem 0.8rem;
    border-radius: 6px;
    font-family: var(--font-mono);
    font-size: 0.88rem;
    outline: none;
    transition: border-color 0.2s;
  }

  .modal-input:focus {
    border-color: var(--primary);
    box-shadow: 0 0 0 2px rgba(250, 204, 21, 0.1);
  }

  .modal-input::placeholder {
    color: var(--text-muted);
  }

  .modal-error {
    color: #ef4444;
    font-size: 0.8rem;
    margin-top: 0.5rem;
  }

  .modal-message {
    color: var(--text-secondary);
    font-size: 0.9rem;
    line-height: 1.5;
  }

  .modal-actions {
    display: flex;
    justify-content: flex-end;
    gap: 0.5rem;
    padding: 0.75rem 1.25rem;
    border-top: 1px solid var(--border-light);
  }

  .modal-btn {
    padding: 0.45rem 1rem;
    border-radius: 6px;
    font-size: 0.85rem;
    font-weight: 500;
    transition: all 0.15s;
  }

  .modal-btn.cancel {
    background: transparent;
    border: 1px solid var(--border-light);
    color: var(--text-secondary);
  }

  .modal-btn.cancel:hover {
    border-color: var(--border-active);
    color: var(--text-main);
  }

  .modal-btn.confirm {
    background: var(--primary);
    color: #000;
    font-weight: 600;
    border: none;
  }

  .modal-btn.confirm:hover {
    background: var(--primary-dim);
  }

  /* Mobile */
  @media (max-width: 768px) {
    .playground-container {
      height: auto;
      min-height: calc(100vh - var(--header-height));
    }

    .editor-layout {
      grid-template-columns: 1fr;
      display: flex;
      flex-direction: column;
    }

    .sidebar {
      max-height: 200px;
      border-right: none;
      border-bottom: 1px solid var(--border-light);
    }

    .main-area {
      min-height: 500px;
    }

    .editor-pane {
      min-height: 350px;
    }

    .output-pane {
      min-height: 150px;
    }
  }

  /* Disabled notice */
  .disabled-notice {
    height: calc(100vh - var(--header-height));
    display: flex;
    align-items: center;
    justify-content: center;
    background: #000;
  }

  .disabled-inner {
    text-align: center;
    color: var(--text-muted);
  }

  .disabled-logo {
    width: 64px;
    height: 64px;
    opacity: 0.2;
    margin-bottom: 2rem;
  }

  .disabled-notice h2 {
    color: var(--text-secondary);
    font-size: 1.5rem;
    margin-bottom: 0.75rem;
  }

  .disabled-notice p {
    font-size: 1rem;
    color: var(--text-muted);
  }
</style>
