<script>
  import { steps } from '../lib/tutorial';
  import Mascot from '../components/Mascot.svelte';
  import Typewriter from '../components/Typewriter.svelte';
  import CodeEditor from '../components/CodeEditor.svelte';
  
  let currentStepIndex = 0;
  let code = steps[0].code;
  let output = 'Waiting to run...';
  let isTalking = false;
  let isRunning = false;
  
  // Sync code when step changes
  $: currentStep = steps[currentStepIndex];
  $: {
    code = currentStep.code;
    output = 'Waiting to run...';
  }

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
    output = 'Compiling...';
    setTimeout(() => {
      isRunning = false;
      output = 'Error: Wasome Compiler Backend is offline.\n(This is a placeholder for the tour)';
    }, 1000);
  }
</script>

<div class="walkthrough-container">
  <!-- Left Side: Guide -->
  <aside class="guide-panel">
    <div class="mascot-area">
      <Mascot talking={isTalking} />
    </div>
    
    <div class="content-area">
      <h3>{currentStep.title}</h3>
      <Typewriter text={currentStep.content} onTyping={handleTyping} />
    </div>

    <div class="controls">
      <button class="nav-btn" on:click={prevStep} disabled={currentStepIndex === 0}>
        &larr; Back
      </button>
      <span class="step-indicator">{currentStepIndex + 1} / {steps.length}</span>
      <button class="nav-btn primary" on:click={nextStep} disabled={currentStepIndex === steps.length - 1}>
        Next &rarr;
      </button>
    </div>
  </aside>

  <!-- Right Side: Editor -->
  <main class="editor-panel">
    <div class="editor-header">
      <div class="filename">tour.waso</div>
      <button class="run-btn" on:click={runCode} disabled={isRunning}>
        {#if isRunning}Running...{:else}▶ Run Code{/if}
      </button>
    </div>
    <div class="editor-body">
      <CodeEditor bind:code={code} />
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

  /* Guide Panel */
  .guide-panel {
    width: 400px;
    background: #0A0A0A;
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

  p {
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

  /* Editor Panel */
  .editor-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    background: #050505;
    overflow: hidden; /* Ensure it respects container bounds */
  }

  .editor-header {
    height: 50px;
    flex-shrink: 0; /* Prevent shrinking */
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
  }

  .run-btn {
    background: #222;
    color: #fff;
    border: 1px solid #333;
    padding: 0.3rem 1rem;
    border-radius: 4px;
    font-size: 0.9rem;
    transition: all 0.2s;
  }

  .run-btn:hover:not(:disabled) {
    background: #333;
    border-color: #444;
  }

  .editor-body {
    flex: 2;
    position: relative;
    min-height: 0; /* Important for flex child with overflow */
  }

  .output-panel {
    flex: 1;
    border-top: 1px solid var(--border-light);
    background: #0A0A0A;
    display: flex;
    flex-direction: column;
    min-height: 0; /* Important for flex child with overflow */
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
      height: 150px;
      margin-bottom: 1rem;
    }
    
    .content-area {
        min-height: 150px; /* Prevent layout shift when text is empty */
    }

    .editor-panel {
      flex: none;
      height: auto;
      overflow: visible;
    }

    .editor-body {
      height: 400px; /* Fixed height for editor on mobile */
      min-height: 400px; /* Force minimum */
      flex: none;
    }

    .output-panel {
      height: 200px; /* Fixed height for output */
      min-height: 200px; /* Force minimum */
      flex: none;
    }
  }
</style>
