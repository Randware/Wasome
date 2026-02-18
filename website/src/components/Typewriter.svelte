<script>
  import { onMount, afterUpdate } from 'svelte';
  
  export let text = '';
  export let onTyping = (isTyping) => {};
  
  let displayedText = '';
  let index = 0;
  let interval;

  $: if (text) {
    startTyping();
  }

  function startTyping() {
    displayedText = '';
    index = 0;
    if (interval) clearInterval(interval);
    
    onTyping(true);
    
    interval = setInterval(() => {
      if (index < text.length) {
        displayedText += text[index];
        index++;
      } else {
        clearInterval(interval);
        onTyping(false);
      }
    }, 18);
  }
  
  onDestroy(() => {
    if (interval) clearInterval(interval);
  });
  
  import { onDestroy } from 'svelte';
</script>

<p>{displayedText}<span class="cursor">|</span></p>

<style>
  p {
    color: var(--text-secondary);
    line-height: 1.7;
    white-space: pre-line;
    min-height: 100px;
  }
  
  .cursor {
    animation: blink 1s step-end infinite;
    color: var(--primary);
  }
  
  @keyframes blink { 50% { opacity: 0; } }
</style>
