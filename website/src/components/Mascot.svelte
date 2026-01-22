<script>
  import { onMount, onDestroy } from 'svelte';
  import * as THREE from 'three';

  export let talking = false;

  let canvasContainer;
  let renderer, scene, camera;
  let mascotGroup;
  let animationId;
  let time = 0;

  // Blinking state
  let eyesScaleY = 1;

  onMount(() => {
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera(45, 1, 0.1, 100);
    camera.position.z = 7;

    renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
    renderer.setSize(280, 280);
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    canvasContainer.appendChild(renderer.domElement);

    mascotGroup = new THREE.Group();
    scene.add(mascotGroup);

    // --- BODY: CHUNKY LOW POLY STAR ---
    const starShape = new THREE.Shape();
    const points = 5;
    // Classic "Cute Star" ratios: fat arms, rounded valleys
    const outerRadius = 1.5;
    const innerRadius = 0.8; 
    
    for (let i = 0; i < points * 2; i++) {
      const r = (i % 2 === 0) ? outerRadius : innerRadius;
      const a = (i / (points * 2)) * Math.PI * 2 - (Math.PI / 2);
      const x = Math.cos(a) * r;
      const y = Math.sin(a) * r;
      if (i === 0) starShape.moveTo(x, y);
      else starShape.lineTo(x, y);
    }
    starShape.closePath();

    const extrudeSettings = {
      steps: 1,
      depth: 0.8, // Thick and chunky
      bevelEnabled: true,
      bevelThickness: 0.3, 
      bevelSize: 0.3,
      bevelSegments: 0 // Zero segments = Sharp Low Poly Edges
    };

    const geometry = new THREE.ExtrudeGeometry(starShape, extrudeSettings);
    geometry.center();

    // Material: Warm Gold, Soft Matte-ish for "Toy" feel
    const material = new THREE.MeshStandardMaterial({
      color: 0xFACC15,
      emissive: 0xF59E0B,
      emissiveIntensity: 0.2,
      roughness: 0.4,
      metalness: 0.1,
      flatShading: true
    });

    const body = new THREE.Mesh(geometry, material);
    body.castShadow = true;
    body.receiveShadow = true;
    mascotGroup.add(body);

    // --- FACE: CUTE & SIMPLE ---
    const faceGroup = new THREE.Group();
    faceGroup.position.z = 0.72; // Sitting on surface
    mascotGroup.add(faceGroup);

    // Eyes: Vertical Ovals (Capsules) - Classic Cute
    // Low poly style: Cylinder with few segments
    const eyeGeo = new THREE.CylinderGeometry(0.12, 0.12, 0.35, 8); 
    const eyeMat = new THREE.MeshBasicMaterial({ color: 0x111111 });

    const leftEye = new THREE.Mesh(eyeGeo, eyeMat);
    leftEye.rotation.x = Math.PI / 2; // Face forward
    leftEye.position.set(-0.4, 0.1, 0);
    faceGroup.add(leftEye);

    const rightEye = new THREE.Mesh(eyeGeo, eyeMat);
    rightEye.rotation.x = Math.PI / 2;
    rightEye.position.set(0.4, 0.1, 0);
    faceGroup.add(rightEye);

    // Cheeks: Pink Hexagons (Low Poly circles)
    const cheekGeo = new THREE.CylinderGeometry(0.18, 0.18, 0.05, 6);
    const cheekMat = new THREE.MeshBasicMaterial({ color: 0xFF8888, transparent: true, opacity: 0.6 });
    
    const leftCheek = new THREE.Mesh(cheekGeo, cheekMat);
    leftCheek.rotation.x = Math.PI / 2;
    leftCheek.position.set(-0.7, -0.15, -0.05); // Slightly back
    faceGroup.add(leftCheek);

    const rightCheek = new THREE.Mesh(cheekGeo, cheekMat);
    rightCheek.rotation.x = Math.PI / 2;
    rightCheek.position.set(0.7, -0.15, -0.05);
    faceGroup.add(rightCheek);

    // --- LIGHTING ---
    const dirLight = new THREE.DirectionalLight(0xffffff, 1.2);
    dirLight.position.set(3, 5, 8);
    dirLight.castShadow = true;
    scene.add(dirLight);

    const ambient = new THREE.AmbientLight(0xffffff, 0.6);
    scene.add(ambient);
    
    // Warm bottom up light (Bounce light)
    const bounceLight = new THREE.DirectionalLight(0xFFD700, 0.5);
    bounceLight.position.set(0, -5, 2);
    scene.add(bounceLight);

    // --- ANIMATION LOOP ---
    const animate = () => {
      animationId = requestAnimationFrame(animate);
      time += 0.02; // Slower time base

      // 1. Idle Body Motion
      // Gentle float
      const idleY = Math.sin(time * 1.5) * 0.1;
      // Gentle rotation (Looking around slightly)
      const idleRotY = Math.sin(time * 1.0) * 0.15;
      const idleRotZ = Math.sin(time * 0.5) * 0.05;

      let talkY = 0;
      let talkRotZ = 0;
      let talkScale = 1;

      // 2. Talking Animation (Overlays on Idle)
      if (talking) {
        // Slower, rhythmic bob (approx 3 beats per second, not a buzz)
        const talkSpeed = 8; 
        const beat = Math.sin(time * talkSpeed);
        
        // Bob up and down
        talkY = Math.abs(beat) * 0.15; 
        
        // Tilt slightly with the beat
        talkRotZ = beat * 0.05;

        // Subtle squash/stretch
        talkScale = 1 + (beat * 0.03);
      }

      // Apply transforms
      mascotGroup.position.y = idleY + talkY;
      mascotGroup.rotation.y = idleRotY;
      mascotGroup.rotation.z = idleRotZ + talkRotZ;
      mascotGroup.scale.setScalar(talkScale);

      // 3. Random Blinking
      // Simple logic: occasional blink interval
      if (Math.random() < 0.01) {
        // Trigger blink
        leftEye.scale.z = 0.1; // Z is Y because of rotation
        rightEye.scale.z = 0.1;
        setTimeout(() => {
           leftEye.scale.z = 1;
           rightEye.scale.z = 1;
        }, 150);
      }

      renderer.render(scene, camera);
    };

    animate();

    return () => {
      cancelAnimationFrame(animationId);
      if (renderer) renderer.dispose();
      if (geometry) geometry.dispose();
    };
  });

  // Easter Egg Logic
  let hearts = [];
  let heartIdCounter = 0;

  function handleMascotClick() {
    // Spawn exactly 3-4 hearts in a random spread around the mascot
    const count = 3 + Math.floor(Math.random() * 2); 
    for (let i = 0; i < count; i++) {
      spawnHeart();
    }
  }

  function spawnHeart() {
    const id = heartIdCounter++;
    // Random angle for circular distribution
    const angle = Math.random() * Math.PI * 2;
    const radius = 80; // Distance from center
    
    // Starting position (center of 280x280 is 140x140)
    const startX = 140;
    const startY = 140;
    
    // End position (exploding outwards)
    const endX = 140 + Math.cos(angle) * radius;
    const endY = 140 + Math.sin(angle) * radius - 40; // Bias upwards

    hearts = [...hearts, { id, startX, startY, endX, endY }];

    // Cleanup after animation
    setTimeout(() => {
      hearts = hearts.filter(h => h.id !== id);
    }, 1000);
  }
</script>

<div class="mascot-container">
  <!-- svelte-ignore a11y-click-events-have-key-events -->
  <div class="mascot-wrapper" bind:this={canvasContainer} on:click={handleMascotClick}></div>
  
  {#each hearts as heart (heart.id)}
    <div 
      class="heart" 
      style="--sx: {heart.startX}px; --sy: {heart.startY}px; --ex: {heart.endX}px; --ey: {heart.endY}px; --rot: {Math.random() * 40 - 20}deg;"
    >
      <svg viewBox="0 0 8 8" fill="#ff4d4d">
        <!-- 8x8 Pixel Heart -->
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
    z-index: 2; /* In front of hearts */
    /* Soft shadow to ground it */
    filter: drop-shadow(0 15px 30px rgba(250, 204, 21, 0.2));
    
    /* Fly-in Animation */
    opacity: 0;
    transform: translateY(20px) scale(0.9);
    animation: mascot-enter 0.8s cubic-bezier(0.34, 1.56, 0.64, 1) forwards;
    animation-delay: 0.2s;
  }

  @keyframes mascot-enter {
    from {
      opacity: 0;
      transform: translateY(50px) scale(0.8) rotate(-5deg);
    }
    to {
      opacity: 1;
      transform: translateY(0) scale(1) rotate(0);
    }
  }

  .heart {
    position: absolute;
    left: 0;
    top: 0;
    width: 24px;
    height: 24px;
    pointer-events: none;
    animation: heart-pop 0.8s cubic-bezier(0.175, 0.885, 0.32, 1.275) forwards;
    z-index: 1; /* Behind the mascot */
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
