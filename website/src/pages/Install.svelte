<script>
  import { onMount } from "svelte";
  import { fade, slide } from "svelte/transition";
  import { highlight } from "../lib/highlighter";
  import { navigate } from "../lib/router";
  import { Rocket, BookOpen, Sparkles } from "lucide-svelte";

  let os = "unknown";
  let installCommand = "";
  let showCopied = false;

  onMount(() => {
    const platform = navigator.platform.toLowerCase();
    const userAgent = navigator.userAgent.toLowerCase();

    if (platform.includes("win") || userAgent.includes("windows")) {
      os = "windows";
      installCommand = "iwr https://get.wasome.org/install.ps1 | iex";
    } else if (platform.includes("linux") || userAgent.includes("linux")) {
      os = "linux";
      installCommand = "curl -fsSL https://get.wasome.org/install.sh | sh";
    } else if (platform.includes("mac") || userAgent.includes("mac")) {
      os = "mac";
      installCommand = "curl -fsSL https://get.wasome.org/install.sh | sh";
    } else {
      os = "linux"; // Default
      installCommand = "curl -fsSL https://get.wasome.org/install.sh | sh";
    }
  });

  async function copyToClipboard() {
    try {
      await navigator.clipboard.writeText(installCommand);
      showCopied = true;
      setTimeout(() => (showCopied = false), 2000);
    } catch (err) {
      console.error("Failed to copy!", err);
    }
  }

  function setOS(newOS) {
    os = newOS;
    if (os === "windows")
      installCommand = "iwr https://get.wasome.org/install.ps1 | iex";
    else installCommand = "curl -fsSL https://get.wasome.org/install.sh | sh";
  }
</script>

<div class="install-page page-animate" in:fade>
  <div class="container">
    <div class="header-section">
      <h1>Get Started with <span class="gradient-text">Wasome</span></h1>
      <p class="subtitle">Install the next-generation WebAssembly toolchain.</p>
    </div>

    <div class="install-card">
      <div class="os-detector">
        <span class="label">Current Platform:</span>
        <span class="os-badge">
          {#if os === "windows"}
            <svg class="icon" viewBox="0 0 24 24" fill="currentColor"
              ><path
                d="M0 3.449L9.75 2.1v9.451H0V3.449zm10.949-1.676L24 0v11.551H10.949V1.773zm-10.949 10.949H9.75v9.451L0 20.551v-7.829zM10.949 12.722H24V24l-13.051-1.773V12.722z"
              /></svg
            >
            Windows
          {:else if os === "mac"}
            <svg class="icon" viewBox="0 0 24 24" fill="currentColor"
              ><path
                d="M18.71 19.5c-.83 1.24-1.71 2.45-3.05 2.47-1.34.03-1.77-.79-3.29-.79-1.53 0-2 .77-3.27.82-1.31.05-2.3-1.32-3.14-2.53C4.25 17 2.94 12.45 4.7 9.39c.87-1.52 2.43-2.48 4.12-2.51 1.28-.02 2.5.87 3.29.87.78 0 2.26-1.07 3.81-.91.65.03 2.47.26 3.64 1.98-.09.06-2.17 1.28-2.15 3.81.03 3.02 2.65 4.03 2.68 4.04-.03.07-.42 1.44-1.38 2.83M13 3.5c.54-.83.9-1.67.9-2.9-1.07.06-1.97.88-1.97 2.48 0 .8.06 1.15.06 1.15s.81-.04 1.01-.73z"
              /></svg
            >
            macOS
          {:else if os === "linux"}
            <svg class="icon" viewBox="0 0 448 512" fill="currentColor"
              ><path
                d="M220.8 123.3c1 .5 1.8 1.7 3 1.7 1.1 0 2.8-.4 2.9-1.5.2-1.4-1.9-2.3-3.2-2.9-1.7-.7-3.9-1-5.5-.1-.4.2-.8.7-.6 1.1.3 1.3 2.3 1.1 3.4 1.7zm-21.9 1.7c1.2 0 2-1.2 3-1.7 1.1-.6 3.1-.4 3.5-1.6.2-.4-.2-.9-.6-1.1-1.6-.9-3.8-.6-5.5.1-1.3.6-3.4 1.5-3.2 2.9.1 1 1.8 1.5 2.8 1.4zM420 403.8c-3.6-4-5.3-11.6-7.2-19.7-1.8-8.1-3.9-16.8-10.5-22.4-1.3-1.1-2.6-2.1-4-2.9-1.3-.8-2.7-1.5-4.1-2 9.2-27.3 5.6-54.5-3.7-79.1-11.4-30.1-31.3-56.4-46.5-74.4-17.1-21.5-33.7-41.9-33.4-72C311.1 85.4 315.7.1 234.8 0 132.4-.2 158 103.4 156.9 135.2c-1.7 23.4-6.4 41.8-22.5 64.7-18.9 22.5-45.5 58.8-58.1 96.7-6 17.9-8.8 36.1-6.2 53.3-6.5 5.8-11.4 14.7-16.6 20.2-4.2 4.3-10.3 5.9-17 8.3s-14 6-18.5 14.5c-2.1 3.9-2.8 8.1-2.8 12.4 0 3.9.6 7.9 1.2 11.8 1.2 8.1 2.5 15.7.8 20.8-5.2 14.4-5.9 24.4-2.2 31.7 3.8 7.3 11.4 10.5 20.1 12.3 17.3 3.6 40.8 2.7 59.3 12.5 19.8 10.4 39.9 14.1 55.9 10.4 11.6-2.6 21.1-9.6 25.9-20.2 12.5-.1 26.3-5.4 48.3-6.6 14.9-1.2 33.6 5.3 55.1 4.1.6 2.3 1.4 4.6 2.5 6.7v.1c8.3 16.7 23.8 24.3 40.3 23 16.6-1.3 34.1-11 48.3-27.9 13.6-16.4 36-23.2 50.9-32.2 7.4-4.5 13.4-10.1 13.9-18.3.4-8.2-4.4-17.3-15.5-29.7zM223.7 87.3c9.8-22.2 34.2-21.8 44-.4 6.5 14.2 3.6 30.9-4.3 40.4-1.6-.8-5.9-2.6-12.6-4.9 1.1-1.2 3.1-2.7 3.9-4.6 4.8-11.8-.2-27-9.1-27.3-7.3-.5-13.9 10.8-11.8 23-4.1-2-9.4-3.5-13-4.4-1-6.9-.3-14.6 2.9-21.8zM183 75.8c10.1 0 20.8 14.2 19.1 33.5-3.5 1-7.1 2.5-10.2 4.6 1.2-8.9-3.3-20.1-9.6-19.6-8.4.7-9.8 21.2-1.8 28.1 1 .8 1.9-.2-5.9 5.5-15.6-14.6-10.5-52.1 8.4-52.1zm-13.6 60.7c6.2-4.6 13.6-10 14.1-10.5 4.7-4.4 13.5-14.2 27.9-14.2 7.1 0 15.6 2.3 25.9 8.9 6.3 4.1 11.3 4.4 22.6 9.3 8.4 3.5 13.7 9.7 10.5 18.2-2.6 7.1-11 14.4-22.7 18.1-11.1 3.6-19.8 16-38.2 14.9-3.9-.2-7-1-9.6-2.1-8-3.5-12.2-10.4-20-15-8.6-4.8-13.2-10.4-14.7-15.3-1.4-4.9 0-9 4.2-12.3zm3.3 334c-2.7 35.1-43.9 34.4-75.3 18-29.9-15.8-68.6-6.5-76.5-21.9-2.4-4.7-2.4-12.7 2.6-26.4v-.2c2.4-7.6.6-16-.6-23.9-1.2-7.8-1.8-15 .9-20 3.5-6.7 8.5-9.1 14.8-11.3 10.3-3.7 11.8-3.4 19.6-9.9 5.5-5.7 9.5-12.9 14.3-18 5.1-5.5 10-8.1 17.7-6.9 8.1 1.2 15.1 6.8 21.9 16l19.6 35.6c9.5 19.9 43.1 48.4 41 68.9zm-1.4-25.9c-4.1-6.6-9.6-13.6-14.4-19.6 7.1 0 14.2-2.2 16.7-8.9 2.3-6.2 0-14.9-7.4-24.9-13.5-18.2-38.3-32.5-38.3-32.5-13.5-8.4-21.1-18.7-24.6-29.9s-3-23.3-.3-35.2c5.2-22.9 18.6-45.2 27.2-59.2 2.3-1.7.8 3.2-8.7 20.8-8.5 16.1-24.4 53.3-2.6 82.4.6-20.7 5.5-41.8 13.8-61.5 12-27.4 37.3-74.9 39.3-112.7 1.1.8 4.6 3.2 6.2 4.1 4.6 2.7 8.1 6.7 12.6 10.3 12.4 10 28.5 9.2 42.4 1.2 6.2-3.5 11.2-7.5 15.9-9 9.9-3.1 17.8-8.6 22.3-15 7.7 30.4 25.7 74.3 37.2 95.7 6.1 11.4 18.3 35.5 23.6 64.6 3.3-.1 7 .4 10.9 1.4 13.8-35.7-11.7-74.2-23.3-84.9-4.7-4.6-4.9-6.6-2.6-6.5 12.6 11.2 29.2 33.7 35.2 59 2.8 11.6 3.3 23.7.4 35.7 16.4 6.8 35.9 17.9 30.7 34.8-2.2-.1-3.2 0-4.2 0 3.2-10.1-3.9-17.6-22.8-26.1-19.6-8.6-36-8.6-38.3 12.5-12.1 4.2-18.3 14.7-21.4 27.3-2.8 11.2-3.6 24.7-4.4 39.9-.5 7.7-3.6 18-6.8 29-32.1 22.9-76.7 32.9-114.3 7.2zm257.4-11.5c-.9 16.8-41.2 19.9-63.2 46.5-13.2 15.7-29.4 24.4-43.6 25.5s-26.5-4.8-33.7-19.3c-4.7-11.1-2.4-23.1 1.1-36.3 3.7-14.2 9.2-28.8 9.9-40.6.8-15.2 1.7-28.5 4.2-38.7 2.6-10.3 6.6-17.2 13.7-21.1.3-.2.7-.3 1-.5.8 13.2 7.3 26.6 18.8 29.5 12.6 3.3 30.7-7.5 38.4-16.3 9-.3 15.7-.9 22.6 5.1 9.9 8.5 7.1 30.3 17.1 41.6 10.6 11.6 14 19.5 13.7 24.6zM173.3 148.7c2 1.9 4.7 4.5 8 7.1 6.6 5.2 15.8 10.6 27.3 10.6 11.6 0 22.5-5.9 31.8-10.8 4.9-2.6 10.9-7 14.8-10.4s5.9-6.3 3.1-6.6-2.6 2.6-6 5.1c-4.4 3.2-9.7 7.4-13.9 9.8-7.4 4.2-19.5 10.2-29.9 10.2s-18.7-4.8-24.9-9.7c-3.1-2.5-5.7-5-7.7-6.9-1.5-1.4-1.9-4.6-4.3-4.9-1.4-.1-1.8 3.7 1.7 6.5z"
              /></svg
            >
            Linux
          {/if}
        </span>
      </div>

      <div class="command-wrapper">
        <div class="command-box">
          <code
            >{@html highlight(
              installCommand,
              os === "windows" ? "powershell" : "bash",
            )}</code
          >
        </div>
        <button
          class="copy-btn"
          onclick={copyToClipboard}
          class:copied={showCopied}
        >
          {#if showCopied}
            <svg
              width="20"
              height="20"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              ><polyline points="20 6 9 17 4 12"></polyline></svg
            >
            Copied
          {:else}
            <svg
              width="20"
              height="20"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              ><rect x="9" y="9" width="13" height="13" rx="2" ry="2"
              ></rect><path
                d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
              ></path></svg
            >
            Copy
          {/if}
        </button>
      </div>

      <div class="manual-select">
        <p>Or select your platform manually:</p>
        <div class="os-buttons">
          <button class:active={os === "linux"} onclick={() => setOS("linux")}
            >Linux</button
          >
          <button class:active={os === "mac"} onclick={() => setOS("mac")}
            >macOS</button
          >
          <button
            class:active={os === "windows"}
            onclick={() => setOS("windows")}>Windows</button
          >
        </div>
      </div>
    </div>

    <div class="next-steps-grid">
      <div class="step-card">
        <h3><Rocket size={18} color="var(--primary)" /> Next Steps</h3>
        <p>Once installed, verify the installation by running:</p>
        <div class="mini-code">wasome --version</div>
      </div>
      <div class="step-card">
        <h3><BookOpen size={18} color="var(--primary)" /> Documentation</h3>
        <p>
          Ready to build? Check out the <a
            href="/docs"
            onclick={(e) => {
              e.preventDefault();
              navigate("/docs");
            }}
            >official docs
          </a> to start your first project.
        </p>
      </div>
      <div class="step-card">
        <h3><Sparkles size={18} color="var(--primary)" /> Take the Tour</h3>
        <p>
          New to Wasome? Our interactive <a
            href="/walkthrough"
            onclick={(e) => {
              e.preventDefault();
              navigate("/walkthrough");
            }}>guided tour</a
          > walks you through the language basics.
        </p>
      </div>
    </div>
  </div>
</div>

<style>
  .install-page {
    padding: 6rem 1.5rem;
    min-height: 80vh;
    display: flex;
    justify-content: center;
  }

  .container {
    max-width: 900px;
    width: 100%;
    text-align: center;
  }

  .header-section {
    margin-bottom: 3rem;
  }

  h1 {
    font-size: 3.5rem;
    margin-bottom: 1rem;
    color: var(--text-main);
    letter-spacing: -0.03em;
  }

  .gradient-text {
    background: linear-gradient(
      135deg,
      var(--primary) 0%,
      var(--secondary, #f59e0b) 100%
    );
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }

  .subtitle {
    font-size: 1.25rem;
    color: var(--text-secondary);
  }

  .install-card {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    border-radius: 16px;
    padding: 3rem;
    box-shadow: 0 10px 40px rgba(0, 0, 0, 0.3);
    margin-bottom: 4rem;
    position: relative;
    overflow: hidden;
  }

  .install-card::before {
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 2px;
    background: linear-gradient(
      90deg,
      transparent,
      var(--primary),
      transparent
    );
  }

  .os-detector {
    margin-bottom: 2rem;
    font-size: 1.1rem;
    color: var(--text-secondary);
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 0.8rem;
  }

  .os-badge {
    background: rgba(255, 255, 255, 0.05);
    padding: 0.5rem 1rem;
    border-radius: 50px;
    color: var(--text-main);
    font-weight: 600;
    display: flex;
    align-items: center;
    gap: 0.6rem;
    border: 1px solid rgba(255, 255, 255, 0.1);
  }

  .os-badge .icon {
    width: 20px;
    height: 20px;
  }

  .command-wrapper {
    display: flex;
    gap: 1rem;
    margin-bottom: 2.5rem;
  }

  .command-box {
    flex: 1;
    background: #080808;
    border: 1px solid var(--border-light);
    border-radius: 8px;
    padding: 1.2rem 1.5rem;
    font-family: "Fira Code", monospace;
    text-align: left;
    display: flex;
    align-items: center;
    overflow-x: auto;
    scrollbar-width: none;
  }

  .command-box::-webkit-scrollbar {
    display: none;
  }

  code {
    color: var(--primary);
    font-size: 1.1rem;
    white-space: nowrap;
  }

  .copy-btn {
    background: var(--bg-card-hover);
    color: var(--text-main);
    border: 1px solid var(--border-light);
    padding: 0 1.5rem;
    border-radius: 8px;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .copy-btn:hover {
    background: #222;
    border-color: var(--primary);
    color: var(--primary);
  }

  .copy-btn.copied {
    background: rgba(45, 212, 191, 0.1); /* Teal tint */
    color: #2dd4bf;
    border-color: #2dd4bf;
  }

  .manual-select p {
    font-size: 0.9rem;
    color: var(--text-muted);
    margin-bottom: 1rem;
  }

  .os-buttons {
    display: flex;
    justify-content: center;
    gap: 1rem;
    flex-wrap: wrap;
  }

  .os-buttons button {
    background: transparent;
    border: 1px solid var(--border-light);
    color: var(--text-secondary);
    padding: 0.6rem 1.5rem;
    border-radius: 8px;
    cursor: pointer;
    transition: all 0.2s;
    font-size: 0.95rem;
  }

  .os-buttons button:hover {
    border-color: var(--text-secondary);
    color: var(--text-main);
  }

  .os-buttons button.active {
    background: rgba(250, 204, 21, 0.1);
    border-color: var(--primary);
    color: var(--primary);
  }

  /* Grid for extra content */
  .next-steps-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 1.5rem;
    text-align: left;
  }

  .step-card {
    background: var(--bg-card);
    border: 1px solid var(--border-light);
    padding: 1.5rem;
    border-radius: 12px;
  }

  .step-card h3 {
    font-size: 1.1rem;
    color: var(--text-main);
    margin-bottom: 0.8rem;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .step-card p {
    font-size: 0.95rem;
    color: var(--text-secondary);
    line-height: 1.5;
  }

  .step-card a {
    color: var(--primary);
    text-decoration: underline;
    text-decoration-thickness: 1px;
    text-underline-offset: 2px;
  }

  .mini-code {
    background: #000;
    padding: 0.5rem 0.8rem;
    border-radius: 6px;
    font-family: var(--font-mono);
    font-size: 0.85rem;
    color: var(--text-secondary);
    margin-top: 0.8rem;
    display: inline-block;
  }

  @media (max-width: 640px) {
    h1 {
      font-size: 2.5rem;
    }
    .command-wrapper {
      flex-direction: column;
    }
    .copy-btn {
      padding: 1rem;
      justify-content: center;
    }
  }
</style>
