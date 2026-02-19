<script>
  import { highlight } from "../lib/highlighter";

  export let code = "";
  export let readonly = false;
  export let lang = "wasome";

  let area;
  let pre;

  function handleScroll() {
    if (pre && area) {
      pre.scrollTop = area.scrollTop;
      pre.scrollLeft = area.scrollLeft;
    }
  }

  function handleKeydown(e) {
    if (e.key === "Tab") {
      e.preventDefault();
      const start = area.selectionStart;
      const end = area.selectionEnd;
      const value = area.value;
      area.value = value.substring(0, start) + "    " + value.substring(end);
      area.selectionStart = area.selectionEnd = start + 4;
      code = area.value;
    }
  }
</script>

<div class="code-editor-container">
  <!-- The syntax highlighted layer -->
  <pre aria-hidden="true" bind:this={pre}>{@html highlight(code, lang)}<br
    /></pre>

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
    display: grid;
    width: 100%;
    height: 100%;
    min-height: 200px;
    background-color: #0a0a0a;
    overflow: auto;
    border-radius: 0 0 8px 8px;
    -webkit-overflow-scrolling: touch;
  }

  textarea,
  pre {
    grid-area: 1 / 1 / 2 / 2;
    margin: 0;
    padding: 1.5rem;
    border: none;

    /* Font Sync Critical */
    font-family: "JetBrains Mono", monospace;
    font-size: 14px;
    line-height: 1.5;
    letter-spacing: normal;

    white-space: pre;
    overflow: auto;
    box-sizing: border-box;
    text-align: left;
    tab-size: 4;
    min-height: 100%;
  }

  pre {
    z-index: 1;
    color: #e4e4e7;
    pointer-events: none;
  }

  textarea {
    z-index: 2;
    color: transparent;
    background: transparent;
    caret-color: #facc15;
    resize: none;
    outline: none;
  }

  textarea::selection {
    background: rgba(250, 204, 21, 0.2);
    color: transparent;
  }

  /* Prevent iOS auto-zoom on focus */
  @media (max-width: 768px) {
    textarea,
    pre {
      font-size: 16px;
    }
  }
</style>
