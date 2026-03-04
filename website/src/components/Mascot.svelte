<script>
  export let talking = false;

  let hearts = [];
  let heartIdCounter = 0;

  function handleMascotClick() {
    const count = 3 + Math.floor(Math.random() * 2); 
    for (let i = 0; i < count; i++) {
      spawnHeart();
    }
  }

  function spawnHeart() {
    const id = heartIdCounter++;
    const angle = Math.random() * Math.PI * 2;
    const radius = 80;
    
    // Center relative to container (280x280)
    const startX = 140;
    const startY = 140;
    
    const endX = 140 + Math.cos(angle) * radius;
    const endY = 140 + Math.sin(angle) * radius - 40;

    hearts = [...hearts, { id, startX, startY, endX, endY }];

    setTimeout(() => {
      hearts = hearts.filter(h => h.id !== id);
    }, 1000);
  }
</script>

<div class="mascot-container">
  <!-- svelte-ignore a11y-click-events-have-key-events -->
  <div 
    class="mascot-wrapper" 
    class:talking={talking}
    on:click={handleMascotClick}
    role="button"
    tabindex="0"
  >
    <svg viewBox="0 0 100 100" class="mascot-svg">
      <defs>
        <filter id="glow" x="-50%" y="-50%" width="200%" height="200%">
          <feGaussianBlur stdDeviation="4" result="coloredBlur" />
          <feMerge>
            <feMergeNode in="coloredBlur" />
            <feMergeNode in="SourceGraphic" />
          </feMerge>
        </filter>
      </defs>
      
      <!-- Star Body -->
      <path 
        d="M50 15 L61 38 L86 41 L68 59 L72 84 L50 72 L28 84 L32 59 L14 41 L39 38 Z" 
        fill="#FACC15" 
        stroke="#EAB308" 
        stroke-width="3"
        stroke-linejoin="round"
        filter="url(#glow)"
        class="star-body"
      />
      
      <!-- Eyes -->
      <g class="face">
        <ellipse cx="43" cy="55" rx="3.5" ry="6" fill="#111" class="eye left-eye" />
        <ellipse cx="57" cy="55" rx="3.5" ry="6" fill="#111" class="eye right-eye" />
        
        <!-- Cheeks -->
        <circle cx="35" cy="62" r="3" fill="#FF8888" opacity="0.6" />
        <circle cx="65" cy="62" r="3" fill="#FF8888" opacity="0.6" />
        
        <!-- Mouth (hidden usually, or small smile) -->
        <path d="M47 62 Q50 65 53 62" stroke="#111" stroke-width="1.5" fill="none" stroke-linecap="round" opacity="0.8" />
      </g>
    </svg>
  </div>
  
  {#each hearts as heart (heart.id)}
    <div 
      class="heart" 
      style="--sx: {heart.startX}px; --sy: {heart.startY}px; --ex: {heart.endX}px; --ey: {heart.endY}px; --rot: {Math.random() * 40 - 20}deg;"
    >
      <svg viewBox="0 0 8 8" fill="#ff4d4d">
        <path d="M2 1h1v1H2V1zm3 0h1v1H5V1zM1 2h1v1H1V2zm2 0h1v1H3V2zm1 0h1v1H4V2zm2 0h1v1H6V2zm-6 1h1v1H0V3zm2 0h1v1H2V3zm5 0h1v1H7V3zm-7 1h1v1H0V4zm7 0h1v1H7V4zm-6 1h1v1H1V5zm5 0h1v1H6V5zm-4 1h1v1H2V6zm3 0h1v1H5V6zm-2 1h1v1H3V7zm1 0h1v1H4V7z" />
      </svg>
    </div>
  {/each}
</div>

<style>
  .mascot-container {
    position: relative;
    width: 280px;
    height: 280px;
    margin: 0 auto;
  }

  .mascot-wrapper {
    width: 100%;
    height: 100%;
    cursor: pointer;
    position: relative;
    z-index: 2;
    transition: transform 0.1s;
    
    /* Idle Animation */
    animation: float 3s ease-in-out infinite;
  }

  .mascot-svg {
    width: 100%;
    height: 100%;
    filter: drop-shadow(0 10px 20px rgba(250, 204, 21, 0.2));
  }

  /* Talking Animation State */
  .mascot-wrapper.talking {
    animation: bounce 0.4s ease-in-out infinite alternate;
  }

  @keyframes float {
    0%, 100% { transform: translateY(0); }
    50% { transform: translateY(-10px); }
  }

  @keyframes bounce {
    from { transform: translateY(0) scale(1); }
    to { transform: translateY(-5px) scale(1.05); }
  }

  /* Blinking Eyes Animation */
  .eye {
    animation: blink 4s infinite;
    transform-origin: center;
  }

  @keyframes blink {
    0%, 96%, 100% { transform: scaleY(1); }
    98% { transform: scaleY(0.1); }
  }

  .heart {
    position: absolute;
    left: 0;
    top: 0;
    width: 24px;
    height: 24px;
    pointer-events: none;
    animation: heart-pop 0.8s cubic-bezier(0.175, 0.885, 0.32, 1.275) forwards;
    z-index: 1;
    opacity: 0;
  }

  .heart svg {
    width: 100%;
    height: 100%;
    filter: drop-shadow(0 2px 4px rgba(0,0,0,0.3));
  }

  @keyframes heart-pop {
    0% {
      opacity: 0;
      transform: translate(var(--sx), var(--sy)) scale(0) rotate(0);
    }
    30% {
      opacity: 1;
      transform: translate(var(--ex), var(--ey)) scale(1.2) rotate(var(--rot));
    }
    100% {
      opacity: 0;
      transform: translate(var(--ex), calc(var(--ey) - 40px)) scale(1) rotate(var(--rot));
    }
  }
</style>