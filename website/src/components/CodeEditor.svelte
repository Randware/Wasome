<script>
  import { highlight } from '../lib/highlighter';

  export let code = '';
  export let readonly = false;

  let area;
  let pre;

  function handleScroll() {
    if (pre && area) {
      pre.scrollTop = area.scrollTop;
      pre.scrollLeft = area.scrollLeft;
    }
  }

  function handleKeydown(e) {
    if (e.key === 'Tab') {
      e.preventDefault();
      const start = area.selectionStart;
      const end = area.selectionEnd;
      const value = area.value;
      area.value = value.substring(0, start) + '    ' + value.substring(end);
      area.selectionStart = area.selectionEnd = start + 4;
      code = area.value;
    }
  }
</script>

<div class="code-editor-container">
  <!-- The syntax highlighted layer -->
  <pre aria-hidden="true" bind:this={pre}>{@html highlight(code)}<br/></pre>
  
  <!-- The interactive inputs layer -->
  <textarea
    bind:this={area}
    bind:value={code}
    {readonly}
    spellcheck="false"
    on:scroll={handleScroll}
    on:keydown={handleKeydown}
    on:input
  ></textarea>
</div>

<style>
  .code-editor-container {
    position: relative;
    width: 100%;
    height: 100%;
    background-color: #0A0A0A;
    overflow: hidden;
    border-radius: 0 0 8px 8px; /* Bottom rounded only */
  }

  textarea, pre {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    margin: 0;
    padding: 1.5rem;
    border: none;
    
    /* Font Sync Critical */
    font-family: 'JetBrains Mono', monospace;
    font-size: 14px;
    line-height: 1.5;
    letter-spacing: normal;
    
    white-space: pre;
    overflow: auto;
    box-sizing: border-box;
    text-align: left;
    tab-size: 4;
  }

  pre {
    z-index: 1;
    color: #e4e4e7; /* Default text color for non-highlighted tokens */
    pointer-events: none;
  }

  textarea {
    z-index: 2;
    color: transparent;
    background: transparent;
    caret-color: #FACC15;
    resize: none;
    outline: none;
  }

  textarea::selection {
    background: rgba(250, 204, 21, 0.2);
    color: transparent;
  }
</style>