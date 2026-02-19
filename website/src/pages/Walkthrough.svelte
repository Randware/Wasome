<script>
  import { steps } from "../lib/tutorial";
  import Mascot from "../components/Mascot.svelte";
  import Typewriter from "../components/Typewriter.svelte";
  import CodeEditor from "../components/CodeEditor.svelte";
  import { Play } from "lucide-svelte";

  let currentStepIndex = $state(0);
  let code = $state(steps[0].code);
  let output = $state("Waiting to run...");
  let isTalking = $state(false);
  let isRunning = $state(false);

  let currentStep = $derived(steps[currentStepIndex]);

  $effect(() => {
    code = currentStep.code;
    output = "Waiting to run...";
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

  async function runCode() {
    isRunning = true;
    output = "Compiling...";
    setTimeout(() => {
      isRunning = false;
      output =
        "Error: Wasome Compiler Backend is offline.\n(This is a placeholder for the tour)";
    }, 1000);
  }
</script>

<div class="walkthrough-container page-animate">
  <aside class="guide-panel">
    <div class="mascot-area">
      <Mascot talking={isTalking} />
    </div>

    <div class="content-area">
      <h3>{currentStep.title}</h3>
      <Typewriter text={currentStep.content} onTyping={handleTyping} />
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
      <button class="run-btn" onclick={runCode} disabled={isRunning}>
        {#if isRunning}
          <span class="spinner"></span> Running...
        {:else}
          <Play size={14} /> Run Code
        {/if}
      </button>
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

<style>
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
</style>
