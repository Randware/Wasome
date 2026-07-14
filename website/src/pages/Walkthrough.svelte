<script>
  import { steps } from "../lib/tutorial";
  import Mascot from "../components/Mascot.svelte";
  import Typewriter from "../components/Typewriter.svelte";
  import CodeEditor from "../components/CodeEditor.svelte";
  import { Play } from "lucide-svelte";
  import { Turnstile } from "svelte-turnstile";
  import { onMount } from "svelte";
  import { compileViaWebSocket } from "../lib/ws-compile.js";
  import { createWasiShim, WasiExit } from "../lib/wasi-shim.js";
  function stripAnsi(str) {
    return str.replace(/\x1b\[[0-9;]*[a-zA-Z]/g, '');
  }

  let initialStep = 0;
  if (typeof window !== "undefined") {
    const stored = sessionStorage.getItem("wasome_tour_step");
    if (stored !== null) {
      const idx = parseInt(stored, 10);
      if (idx >= 0 && idx < steps.length) {
        initialStep = idx;
      }
    }
  }

  let currentStepIndex = $state(initialStep);
  let code = $state(steps[initialStep].code);
  let output = $state("Waiting to run...");
  let isTalking = $state(false);
  let isRunning = $state(false);
  let isVerifying = $state(false);
  let hasValidSession = $state(false);
  let turnstileToken = $state(null);
  let turnstileRef = $state(null);
  let compilationEnabled = $state(true);
  let maintenanceMode = $state(false);

  let currentStep = $derived(steps[currentStepIndex]);

  $effect(() => {
    code = currentStep.code;
    output = "Waiting to run...";
  });

  $effect(() => {
    if (typeof window !== "undefined") {
      sessionStorage.setItem("wasome_tour_step", currentStepIndex.toString());
    }
  });

  function handleTyping(typing) {
    isTalking = typing;
  }

  function nextStep() {
    if (currentStepIndex < steps.length - 1) {
      currentStepIndex++;
    }
  }

  function prevStep() {
    if (currentStepIndex > 0) {
      currentStepIndex--;
    }
  }

  onMount(() => {
    fetch('/api/status')
      .then(r => r.json())
      .then(data => {
        compilationEnabled = data.compilation_enabled;
        maintenanceMode = data.maintenance_mode;
      })
      .catch(() => {});
  });

  async function runCode() {
    if (maintenanceMode) {
      output = "Walkthrough is under maintenance. Please try again later.";
      return;
    }
    if (!compilationEnabled) {
      output = "Compilation is currently disabled.";
      return;
    }
    isRunning = true;
    output = "";

    if (!hasValidSession) {
      const siteKey = import.meta.env.PUBLIC_TURNSTILE_SITE_KEY || "1x00000000000000000000AA";
      if (siteKey === "1x00000000000000000000AA") {
        await doCompile("dummy-bypass");
        return;
      }
      isVerifying = true;
      if (turnstileRef) {
        turnstileRef.reset();
      } else {
        setTimeout(() => {
          if (turnstileRef) turnstileRef.reset();
        }, 100);
      }
      return;
    }
    const siteKey = import.meta.env.PUBLIC_TURNSTILE_SITE_KEY || "1x00000000000000000000AA";
    const token = siteKey === "1x00000000000000000000AA" ? "dummy-bypass" : null;
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

    const compileFiles = { 
      "src/main.waso": code,
      "waso.toml": `[project]\nname = "tour"\nversion = "0.1.0"\nentry = "src/main.waso"\n`
    };

    if (code.includes('import "./math"')) {
      compileFiles["src/math/main.waso"] = `pub fn double(s32 x) -> s32 {\n    -> x * 2\n}\n`;
    }

    await compileViaWebSocket({
      files: compileFiles,
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
</script>

<div class="walkthrough-container page-animate">
  <aside class="guide-panel">
    <div class="mascot-area">
      <Mascot talking={isTalking} />
    </div>

    <div class="content-area">
      <h3>{currentStep.title}</h3>
      {#if currentStepIndex === steps.length - 1}
        <div class="completion-message">
          <p>Congratulations! You have completed the Wasome language tour!</p>
          <p>We have covered everything from basic variables and functions to structs, generics, modules, and the standard library. You now have the tools to write and compile code for WebAssembly.</p>
          
          <div class="next-steps-section">
            <span class="next-title">What's next?</span>
            <ul class="next-steps-list">
              <li>Explore the full language details on the <a href="/docs" class="inline-link">Docs page</a>.</li>
              <li>Check out the <a href="/examples" class="inline-link">Examples tab</a> to see larger solvers.</li>
              <li>Open the <a href="/playground" class="inline-link">Playground</a> to start writing your own code.</li>
            </ul>
          </div>
          
          <p>If you find a bug, have a suggestion, or want to contribute to the compiler, come say hello on our <a href="https://github.com/Randware/Wasome" target="_blank" class="inline-link">GitHub repository</a>. We are always happy to collaborate!</p>
          <p class="final-wish">Thank you for exploring with me. Happy coding!</p>
        </div>
      {:else}
        <Typewriter text={currentStep.content} onTyping={handleTyping} />
      {/if}
    </div>

    <div class="controls">
      <button
        class="nav-btn"
        onclick={prevStep}
        disabled={currentStepIndex === 0}
      >
        &larr; Back
      </button>
      <span class="step-indicator">{currentStepIndex + 1} / {steps.length}</span
      >
      <button
        class="nav-btn primary"
        onclick={nextStep}
        disabled={currentStepIndex === steps.length - 1}
      >
        Next &rarr;
      </button>
    </div>
  </aside>

  <main class="editor-panel">
    <div class="editor-header">
      <div class="filename">tour.waso</div>
      {#if compilationEnabled}
        <button class="run-btn" onclick={runCode} disabled={isRunning}>
          {#if isRunning}
            <span class="spinner"></span> Running...
          {:else}
            <Play size={14} /> Run Code
          {/if}
        </button>
      {/if}
    </div>
    <div class="editor-body">
      <CodeEditor bind:code />
    </div>
    <div class="output-panel">
      <div class="output-header">Output</div>
      <pre>{output}</pre>
    </div>
  </main>
</div>

{#if isVerifying}
  <div class="turnstile-overlay">
    <div class="turnstile-modal">
      <h3 class="modal-title">Security Check</h3>
      <p class="modal-desc">Please complete the verification below to build and run your project.</p>
      <div class="turnstile-widget">
        <Turnstile
          siteKey={import.meta.env.PUBLIC_TURNSTILE_SITE_KEY || '1x00000000000000000000AA'}
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
    animation: fadeIn 0.2s ease-out;
  }

  .turnstile-modal {
    background: rgba(18, 18, 18, 0.95);
    border: 1px solid rgba(255, 255, 255, 0.08);
    border-radius: 12px;
    padding: 2rem;
    width: 380px;
    max-width: 90%;
    text-align: center;
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.5);
    animation: scaleIn 0.2s cubic-bezier(0.34, 1.56, 0.64, 1);
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

  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }

  @keyframes scaleIn {
    from { transform: scale(0.95); opacity: 0; }
    to { transform: scale(1); opacity: 1; }
  }

  @media (max-width: 400px) {
    .turnstile-modal {
      padding: 1.5rem 1rem;
      max-width: 95%;
    }
  }

  .walkthrough-container {
    display: flex;
    height: calc(100vh - var(--header-height));
    background: #000;
  }

  .guide-panel {
    width: 400px;
    background: #0a0a0a;
    border-right: 1px solid var(--border-light);
    display: flex;
    flex-direction: column;
    padding: 2rem;
    flex-shrink: 0;
  }

  .mascot-area {
    height: 200px;
    display: flex;
    justify-content: center;
    align-items: center;
    margin-bottom: 2rem;
  }

  .content-area {
    flex: 1;
    overflow-y: auto;
  }

  h3 {
    color: var(--primary);
    margin-bottom: 1rem;
    font-size: 1.5rem;
  }

  .content-area :global(p) {
    color: var(--text-secondary);
    line-height: 1.7;
    white-space: pre-line;
  }

  .controls {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-top: 2rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border-light);
  }

  .step-indicator {
    color: var(--text-muted);
    font-family: var(--font-mono);
    font-size: 0.9rem;
  }

  .nav-btn {
    background: transparent;
    border: 1px solid var(--border-light);
    color: var(--text-main);
    padding: 0.5rem 1rem;
    border-radius: 6px;
    transition: all 0.2s;
  }

  .nav-btn:hover:not(:disabled) {
    border-color: var(--text-main);
  }

  .nav-btn.primary {
    background: var(--primary);
    color: #000;
    border: none;
    font-weight: 600;
  }

  .nav-btn.primary:hover:not(:disabled) {
    background: var(--primary-dim);
  }

  .nav-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .editor-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    background: #050505;
    overflow: hidden;
  }

  .editor-header {
    height: 50px;
    flex-shrink: 0;
    background: #111;
    border-bottom: 1px solid var(--border-light);
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 1rem;
  }

  .filename {
    color: var(--text-muted);
    font-family: var(--font-mono);
    font-size: 0.9rem;
    padding-bottom: 4px;
    border-bottom: 2px solid var(--primary);
  }

  .run-btn {
    background: var(--primary);
    color: #000;
    border: none;
    padding: 0.4rem 1.2rem;
    border-radius: 4px;
    font-size: 0.9rem;
    font-weight: 600;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .run-btn:hover:not(:disabled) {
    background: var(--primary-dim);
    box-shadow: 0 0 12px rgba(250, 204, 21, 0.2);
  }

  .run-btn:disabled {
    opacity: 0.7;
    cursor: not-allowed;
    background: #333;
    color: #888;
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

  .editor-body {
    flex: 2;
    position: relative;
    min-height: 0;
  }

  .output-panel {
    flex: 1;
    border-top: 1px solid var(--border-light);
    background: #0a0a0a;
    display: flex;
    flex-direction: column;
    min-height: 0;
  }

  .output-header {
    padding: 0.5rem 1rem;
    font-size: 0.8rem;
    text-transform: uppercase;
    color: var(--text-muted);
    background: #111;
    border-bottom: 1px solid var(--border-light);
    flex-shrink: 0;
  }

  pre {
    padding: 1rem;
    color: var(--text-secondary);
    font-family: var(--font-mono);
    font-size: 0.9rem;
    margin: 0;
    overflow: auto;
    flex: 1;
  }

  @media (max-width: 900px) {
    .walkthrough-container {
      flex-direction: column;
      height: auto;
      min-height: calc(100vh - var(--header-height));
    }

    .guide-panel {
      width: 100%;
      height: auto;
      border-right: none;
      border-bottom: 1px solid var(--border-light);
      padding: 1.5rem;
      flex-shrink: 0;
    }

    .mascot-area {
      height: 120px;
      margin-bottom: 1rem;
    }

    .mascot-area :global(.mascot-container) {
      width: 160px;
      height: 160px;
    }

    .content-area {
      min-height: 150px;
    }

    .editor-panel {
      flex: none;
      height: auto;
      overflow: visible;
    }

    .editor-body {
      height: 400px;
      min-height: 400px;
      flex: none;
    }

    .output-panel {
      height: 200px;
      min-height: 200px;
      flex: none;
    }
  }

  .completion-message p {
    color: var(--text-secondary);
    line-height: 1.6;
    margin-bottom: 1rem;
    font-size: 0.95rem;
  }

  .next-steps-section {
    margin: 1.5rem 0;
    padding: 1rem;
    background: rgba(255, 255, 255, 0.03);
    border-radius: 8px;
    border: 1px solid var(--border-light);
  }

  .next-title {
    display: block;
    color: var(--text-main);
    font-weight: 600;
    margin-bottom: 0.5rem;
    font-size: 0.95rem;
  }

  .next-steps-list {
    margin: 0;
    padding-left: 1.2rem;
    color: var(--text-secondary);
  }

  .next-steps-list li {
    margin-bottom: 0.4rem;
    line-height: 1.5;
    font-size: 0.9rem;
  }

  .inline-link {
    color: var(--primary);
    text-decoration: none;
    font-weight: 500;
    transition: color 0.2s;
  }

  .inline-link:hover {
    color: var(--primary-dim);
    text-decoration: underline;
  }

  .final-wish {
    margin-top: 1.5rem;
    font-weight: 500;
    color: var(--text-main) !important;
  }
</style>
