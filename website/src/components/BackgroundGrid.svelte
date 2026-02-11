<script>
  import { onMount } from 'svelte';

  let canvas;
  let ctx;
  let animationId;
  let width, height;

  const GRID_SIZE = 40;
  const WAVE_SPEED = 2;
  const COLOR_PRIMARY = [250, 204, 21]; // Wasome Yellow (rgb)
  
  let waves = [];
  let mouseX = 0;
  let mouseY = 0;

  class Wave {
    constructor(x, y) {
      this.x = x;
      this.y = y;
      this.radius = 0;
      this.opacity = 1;
      this.maxRadius = 300;
    }

    update() {
      this.radius += WAVE_SPEED;
      this.opacity = 1 - (this.radius / this.maxRadius);
    }

    draw(ctx) {
      if (this.opacity <= 0) return;
      
      // We don't draw the circle itself, we light up the grid points inside it
      // Optimization: Only check grid points within the bounding box of the circle
      const startCol = Math.floor((this.x - this.radius) / GRID_SIZE);
      const endCol = Math.ceil((this.x + this.radius) / GRID_SIZE);
      const startRow = Math.floor((this.y - this.radius) / GRID_SIZE);
      const endRow = Math.ceil((this.y + this.radius) / GRID_SIZE);

      for (let c = startCol; c <= endCol; c++) {
        for (let r = startRow; r <= endRow; r++) {
          const px = c * GRID_SIZE;
          const py = r * GRID_SIZE;
          
          const dx = px - this.x;
          const dy = py - this.y;
          const dist = Math.sqrt(dx * dx + dy * dy);

          // If point is near the wave front (ring effect)
          if (Math.abs(dist - this.radius) < 20) {
            const intensity = (1 - Math.abs(dist - this.radius) / 20) * this.opacity;
            
            ctx.fillStyle = `rgba(${COLOR_PRIMARY.join(',')}, ${intensity})`;
            ctx.beginPath();
            ctx.arc(px, py, 1.5, 0, Math.PI * 2);
            ctx.fill();
            
            // Draw connecting lines if intensity is high enough
            if (intensity > 0.5) {
                ctx.strokeStyle = `rgba(${COLOR_PRIMARY.join(',')}, ${intensity * 0.5})`;
                ctx.beginPath();
                ctx.moveTo(px - 4, py);
                ctx.lineTo(px + 4, py);
                ctx.moveTo(px, py - 4);
                ctx.lineTo(px, py + 4);
                ctx.stroke();
            }
          }
        }
      }
    }
  }

  function resize() {
    width = window.innerWidth;
    height = window.innerHeight;
    canvas.width = width;
    canvas.height = height;
  }

  function loop() {
    ctx.fillStyle = '#09090b'; // Background
    ctx.fillRect(0, 0, width, height);

    // Draw base grid (faint)
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.03)';
    ctx.lineWidth = 1;
    
    // Vertical lines
    for (let x = 0; x <= width; x += GRID_SIZE) {
        ctx.beginPath();
        ctx.moveTo(x, 0);
        ctx.lineTo(x, height);
        ctx.stroke();
    }
    
    // Horizontal lines
    for (let y = 0; y <= height; y += GRID_SIZE) {
        ctx.beginPath();
        ctx.moveTo(0, y);
        ctx.lineTo(width, y);
        ctx.stroke();
    }

    // Update and draw waves
    for (let i = waves.length - 1; i >= 0; i--) {
      waves[i].update();
      waves[i].draw(ctx);
      if (waves[i].opacity <= 0) {
        waves.splice(i, 1);
      }
    }
    
    // Occasional random drop
    if (Math.random() < 0.02) {
        const x = Math.random() * width;
        const y = Math.random() * height;
        waves.push(new Wave(x, y));
    }

    animationId = requestAnimationFrame(loop);
  }

  function handleMouseMove(e) {
      // Chance to spawn wave on mouse move
      if (Math.random() < 0.1) {
        const rect = canvas.getBoundingClientRect();
        waves.push(new Wave(e.clientX - rect.left, e.clientY - rect.top));
      }
  }

  onMount(() => {
    ctx = canvas.getContext('2d');
    resize();
    window.addEventListener('resize', resize);
    window.addEventListener('mousemove', handleMouseMove);
    
    loop();

    return () => {
      window.removeEventListener('resize', resize);
      window.removeEventListener('mousemove', handleMouseMove);
      cancelAnimationFrame(animationId);
    };
  });
</script>

<canvas bind:this={canvas} class="background-canvas"></canvas>

<style>
  .background-canvas {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
  }
</style>