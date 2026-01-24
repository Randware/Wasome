<script>
  import CodeEditor from '../components/CodeEditor.svelte';

  let code = `fn main() {
    // Write your Wasome code here
    s32 x <- 10
    s32 y <- 20
    -> x + y
}`;
  let output = '// Output will appear here...';
  let isRunning = false;

  async function runCode() {
    isRunning = true;
    output = 'Compiling and executing...';
    
    // Simulate network delay
    setTimeout(async () => {
      try {
        const res = await fetch('https://api.wasome-lang.org/run', {
            method: 'POST',
            body: JSON.stringify({ code })
        });
        // We expect this to fail or 404
        if (!res.ok) throw new Error('API Endpoint not found');
        output = await res.text();
      } catch (e) {
        output = `Error: Wasome Compiler Backend is offline.\n\n(This is a placeholder playground. The backend API is not yet implemented.)`;
      } finally {
        isRunning = false;
      }
    }, 1500);
  }
</script>

<div class="playground-container">
  <div class="toolbar">
    <div class="title">Wasome Playground</div>
    <div class="controls">
      <button class="run-btn" on:click={runCode} disabled={isRunning}>
        {#if isRunning}
          <span class="spinner"></span> Running...
        {:else}
          ▶ Run
        {/if}
      </button>
    </div>
  </div>

  <div class="editor-layout">
    <div class="editor-pane">
      <div class="pane-header">main.waso</div>
      <div class="editor-wrapper">
        <CodeEditor bind:code={code} />
      </div>
    </div>
    <div class="output-pane">
      <div class="pane-header">Console Output</div>
      <pre>{output}</pre>
    </div>
  </div>
</div>

<style>
  .playground-container {
    height: calc(100vh - var(--header-height));
    display: flex;
    flex-direction: column;
    background: #000;
  }

  .toolbar {
    height: 60px;
    background: #0A0A0A;
    border-bottom: 1px solid var(--border-light);
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 1.5rem;
  }

  .title {
    font-weight: 600;
    color: var(--text-secondary);
  }

  .run-btn {
    background: var(--primary);
    color: #000;
    font-weight: 600;
    padding: 0.5rem 1.5rem;
    border-radius: 4px;
    display: flex;
    align-items: center;
    gap: 0.5rem;
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

  .editor-layout {
    flex: 1;
    display: grid;
    grid-template-columns: 1fr 1fr;
    overflow: hidden;
  }

  .editor-pane, .output-pane {
    display: flex;
    flex-direction: column;
    border-right: 1px solid var(--border-light);
  }

  .output-pane {
    background: #050505;
    border-right: none;
  }

  .pane-header {
    padding: 0.5rem 1rem;
    background: #111;
    font-size: 0.8rem;
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.05em;
    border-bottom: 1px solid var(--border-light);
  }

  .editor-wrapper {
    flex: 1;
    position: relative;
    overflow: hidden;
  }

  pre {
    flex: 1;
    padding: 1.5rem;
    color: var(--text-secondary);
    font-family: var(--font-mono);
    font-size: 0.9rem;
    white-space: pre-wrap;
    overflow-y: auto;
  }

  .spinner {
    width: 14px;
    height: 14px;
    border: 2px solid rgba(0,0,0,0.3);
    border-top-color: #fff;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  @media (max-width: 768px) {
    .editor-layout {
      grid-template-columns: 1fr;
      grid-template-rows: 1fr 1fr;
    }
    .editor-pane {
      border-right: none;
      border-bottom: 1px solid var(--border-light);
    }
  }
</style>