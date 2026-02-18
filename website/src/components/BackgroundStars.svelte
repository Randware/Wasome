<script>
  import { onMount, onDestroy } from "svelte";

  let canvas;
  let ctx;
  let animationId;
  let width, height;

  const COLOR_BG = "#050505";
  const COLOR_STAR = "255, 255, 255";
  const COLOR_PRIMARY = "250, 204, 21"; // Wasome Yellow (rgb 250, 204, 21)

  // Configuration
  const STAR_COUNT = 80;
  const CONNECT_DIST = 150;
  const MOUSE_DIST = 200;

  let stars = [];
  let mouse = { x: -1000, y: -1000 };

  class Star {
    constructor() {
      this.x = Math.random() * width;
      this.y = Math.random() * height;
      this.vx = (Math.random() - 0.5) * 0.5; // slow drift
      this.vy = (Math.random() - 0.5) * 0.5;
      this.size = Math.random() * 2;
      this.isSpecial = Math.random() < 0.15; // 15% are yellow/special
    }

    update() {
      this.x += this.vx;
      this.y += this.vy;

      // Wrap around screen edges for continuous flow
      if (this.x < 0) this.x = width;
      if (this.x > width) this.x = 0;
      if (this.y < 0) this.y = height;
      if (this.y > height) this.y = 0;

      // Mouse interaction: gentle attraction
      const dx = mouse.x - this.x;
      const dy = mouse.y - this.y;
      const dist = Math.sqrt(dx * dx + dy * dy);

      if (dist < MOUSE_DIST) {
        const force = (MOUSE_DIST - dist) / MOUSE_DIST;
        // Move slightly towards mouse
        this.x += dx * force * 0.02;
        this.y += dy * force * 0.02;
      }
    }

    draw(ctx) {
      ctx.beginPath();
      ctx.arc(this.x, this.y, this.size, 0, Math.PI * 2);
      ctx.fillStyle = this.isSpecial
        ? `rgba(${COLOR_PRIMARY}, 0.8)`
        : `rgba(${COLOR_STAR}, 0.6)`;
      ctx.fill();
    }
  }

  function resize() {
    if (!canvas) return;
    width = window.innerWidth;
    height = window.innerHeight;
    canvas.width = width;
    canvas.height = height;
    initStars();
  }

  function initStars() {
    stars = [];
    for (let i = 0; i < STAR_COUNT; i++) {
      stars.push(new Star());
    }
  }

  function loop() {
    if (!ctx) return;
    ctx.clearRect(0, 0, width, height);

    // Draw Connections first (behind stars)
    for (let i = 0; i < stars.length; i++) {
      const starA = stars[i];
      for (let j = i + 1; j < stars.length; j++) {
        const starB = stars[j];
        const dx = starA.x - starB.x;
        const dy = starA.y - starB.y;
        const dist = Math.sqrt(dx * dx + dy * dy);

        if (dist < CONNECT_DIST) {
          const opacity = 1 - dist / CONNECT_DIST;
          ctx.beginPath();
          // If either star is special, line is slightly yellow
          if (starA.isSpecial || starB.isSpecial) {
            ctx.strokeStyle = `rgba(${COLOR_PRIMARY}, ${opacity * 0.3})`;
            ctx.lineWidth = 1;
          } else {
            ctx.strokeStyle = `rgba(255, 255, 255, ${opacity * 0.15})`;
            ctx.lineWidth = 0.5;
          }
          ctx.moveTo(starA.x, starA.y);
          ctx.lineTo(starB.x, starB.y);
          ctx.stroke();
        }
      }
    }

    // Draw Stars on top
    stars.forEach((star) => {
      star.update();
      star.draw(ctx);
    });

    animationId = requestAnimationFrame(loop);
  }

  function handleMouseMove(e) {
    if (canvas) {
      const rect = canvas.getBoundingClientRect();
      mouse.x = e.clientX - rect.left;
      mouse.y = e.clientY - rect.top;
    }
  }

  onMount(() => {
    if (typeof window !== "undefined") {
      width = window.innerWidth;
      height = window.innerHeight;
      if (canvas) {
        canvas.width = width;
        canvas.height = height;
        ctx = canvas.getContext("2d");
        initStars();
        loop();
      }
      window.addEventListener("resize", resize);
      window.addEventListener("mousemove", handleMouseMove);
    }
  });

  onDestroy(() => {
    if (typeof window !== "undefined") {
      window.removeEventListener("resize", resize);
      window.removeEventListener("mousemove", handleMouseMove);
      cancelAnimationFrame(animationId);
    }
  });
</script>

<div class="canvas-container">
  <canvas bind:this={canvas}></canvas>
  <div class="fade-overlay"></div>
</div>

<style>
  .canvas-container {
    position: absolute;
    inset: 0;
    z-index: -1;
    overflow: hidden;
    background: #050505;
    will-change: transform;
  }

  canvas {
    display: block;
    width: 100%;
    height: 100%;
  }

  .fade-overlay {
    position: absolute;
    bottom: 0;
    left: 0;
    width: 100%;
    height: 200px;
    background: linear-gradient(to bottom, transparent, #050505);
    pointer-events: none;
  }
</style>
