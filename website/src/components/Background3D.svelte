<script>
  import { onMount, onDestroy } from 'svelte';
  import * as THREE from 'three';

  let canvasContainer;
  let animationId;
  let renderer, scene, camera;
  let shape, particles;
  
  // Mouse state
  let mouseX = 0;
  let mouseY = 0;
  let targetX = 0;
  let targetY = 0;

  const handleMouseMove = (event) => {
    // Normalize mouse position from -1 to 1
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
  };

  onMount(() => {
    if (!canvasContainer) return;
    
    window.addEventListener('mousemove', handleMouseMove);

    // Scene setup
    scene = new THREE.Scene();
    
    // Camera
    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.z = 5;

    renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
    renderer.setSize(window.innerWidth, window.innerHeight);
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    canvasContainer.appendChild(renderer.domElement);

    // Main Shape: Icosahedron Wireframe
    const geometry = new THREE.IcosahedronGeometry(2.2, 1);
    const material = new THREE.MeshBasicMaterial({ 
      color: 0xFACC15, // Wasome Yellow
      wireframe: true,
      transparent: true,
      opacity: 0.12
    });
    shape = new THREE.Mesh(geometry, material);
    scene.add(shape);

    // Inner Core (adds depth)
    const coreGeo = new THREE.IcosahedronGeometry(1.0, 0);
    const coreMat = new THREE.MeshBasicMaterial({
      color: 0xFACC15,
      wireframe: true,
      transparent: true,
      opacity: 0.2
    });
    const core = new THREE.Mesh(coreGeo, coreMat);
    scene.add(core);


    // Particles
    const particlesGeometry = new THREE.BufferGeometry();
    const particlesCount = 200; // Reduced count
    const posArray = new Float32Array(particlesCount * 3);

    for(let i = 0; i < particlesCount * 3; i+=3) {
      // Keep particles further away from camera (z > -5 and z < 2)
      posArray[i] = (Math.random() - 0.5) * 25;     // x
      posArray[i+1] = (Math.random() - 0.5) * 25;   // y
      posArray[i+2] = (Math.random() - 0.5) * 10 - 5; // z: push back
    }

    particlesGeometry.setAttribute('position', new THREE.BufferAttribute(posArray, 3));
    const particlesMaterial = new THREE.PointsMaterial({
      size: 0.04,
      color: 0xFFFFFF,
      transparent: true,
      opacity: 0.2, // More subtle
      blending: THREE.AdditiveBlending
    });

    particles = new THREE.Points(particlesGeometry, particlesMaterial);
    scene.add(particles);

    // Animation Loop
    const animate = () => {
      animationId = requestAnimationFrame(animate);
      
      // Smooth mouse follow
      targetX = targetX + (mouseX - targetX) * 0.05;
      targetY = targetY + (mouseY - targetY) * 0.05;

      if (shape) {
        shape.rotation.x += 0.001;
        shape.rotation.y += 0.002;
        
        // Parallax effect on shape
        shape.rotation.x += targetY * 0.02;
        shape.rotation.y += targetX * 0.02;
      }
      
      if (core) {
        core.rotation.x -= 0.002;
        core.rotation.y -= 0.004;
        core.rotation.x -= targetY * 0.03;
        core.rotation.y -= targetX * 0.03;
      }

      if (particles) {
        particles.rotation.y -= 0.0005;
        // Parallax effect on particles
        particles.rotation.x = -targetY * 0.1;
        particles.rotation.y += targetX * 0.05;
      }
      
      // Slight camera sway
      camera.position.x = targetX * 0.2;
      camera.position.y = targetY * 0.2;
      camera.lookAt(scene.position);

      renderer.render(scene, camera);
    };

    animate();

    const handleResize = () => {
      camera.aspect = window.innerWidth / window.innerHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(window.innerWidth, window.innerHeight);
    };

    window.addEventListener('resize', handleResize);

    return () => {
      window.removeEventListener('mousemove', handleMouseMove);
      window.removeEventListener('resize', handleResize);
      cancelAnimationFrame(animationId);
      if (renderer) renderer.dispose();
      if (geometry) geometry.dispose();
      if (material) material.dispose();
    };
  });
</script>

<div class="canvas-container" bind:this={canvasContainer}></div>

<style>
  .canvas-container {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
    opacity: 0.6;
    pointer-events: none;
    overflow: hidden;
  }
</style>