<script>
  import { highlight } from '../lib/highlighter';

  let activeTab = 'enum';

  const examples = {
    enum: `enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday
}

fn main() {
    Weekday weekday <- Weekday::Saturday
}`,
    if: `fn main() {
    char wasome <- showcase_if_conditionals()
}

fn showcase_if_conditionals() -> char {
    bool wasome_is_awesome <- true
 
    if (wasome_is_awesome) {
        -> '✨'
    } else {
        -> '🤥'   
    }
}`,
    loop: `fn main() {
    // while loop
    s32 count1 <- 0

    loop (count1 < 10) {
        count1 = count1 + 1
    }

    // for loop
    s32 sum <- 0

    loop (s32 count2 <- 0; count2 < 100; count2 <- count2 + 1) {
        sum = sum + count2
    }
    
    // infinite loop
    s32 count3 <- 0
    
    loop {
        count3 = count3 + 1
        if (count3 > 10) { break }
    }
}`,
    operator: `fn main() {
    s32 math_showcase <- 10 * 2 + 5 - 3 / 1
    s32 num <- 10
    
    if (num < 10) {}
    if (num <= 10) {}
    if (num == 10) {}
    if (num != 10) {}
    if (num > 10) {}
    if (num >= 10) {}
    
    // Logical
    if (num == 10 && true) {}
    if (num == 10 || false) {}
}`,
    struct: `struct Point {
    s32 x
    s32 y
}

fn main() {
    Point p1 <- new Point { x <- 10, y <- 20 }
    s32 old_x <- p1.x
    p1.x <- 15
}`
  };
</script>

<section class="container page-section">
  <div class="header">
    <h1>Examples</h1>
    <p>Learn by doing. Explore these code snippets to understand Wasome syntax.</p>
  </div>

  <div class="example-viewer">
    <div class="tabs">
      {#each Object.keys(examples) as key}
        <button 
          class:active={activeTab === key} 
          on:click={() => activeTab = key}
        >
          {key}.waso
        </button>
      {/each}
    </div>

    <div class="code-window">
      <div class="window-bar">
        <div class="traffic-lights">
          <span></span><span></span><span></span>
        </div>
        <div class="filename">{activeTab}.waso</div>
      </div>
      <div class="code-content">
        <pre>{@html highlight(examples[activeTab])}</pre>
      </div>
    </div>
  </div>
</section>

<style>
  .page-section {
    padding-top: 4rem;
    padding-bottom: 8rem;
  }

  .header {
    text-align: center;
    margin-bottom: 3rem;
  }

  h1 {
    font-size: 3rem;
    margin-bottom: 1rem;
  }

  p {
    color: var(--text-secondary);
    font-size: 1.2rem;
  }

  .example-viewer {
    max-width: 900px;
    margin: 0 auto;
  }

  .tabs {
    display: flex;
    justify-content: center;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
    flex-wrap: wrap;
  }

  button {
    background: transparent;
    border: 1px solid var(--border-light);
    color: var(--text-secondary);
    padding: 0.6rem 1.2rem;
    border-radius: 8px;
    font-family: var(--font-mono);
    font-size: 0.9rem;
    transition: all 0.2s;
  }

  button:hover {
    border-color: var(--text-muted);
    color: var(--text-main);
  }

  button.active {
    background: var(--bg-card-hover);
    border-color: var(--primary);
    color: var(--primary);
    box-shadow: 0 0 10px rgba(250, 204, 21, 0.1);
  }

  .code-window {
    background: #0A0A0A;
    border: 1px solid var(--border-light);
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 20px 40px rgba(0,0,0,0.4);
  }

  .window-bar {
    background: #111;
    padding: 0.8rem 1rem;
    display: flex;
    align-items: center;
    border-bottom: 1px solid var(--border-light);
    position: relative;
  }

  .traffic-lights {
    display: flex;
    gap: 6px;
  }

  .traffic-lights span {
    width: 10px;
    height: 10px;
    border-radius: 50%;
  }

  .traffic-lights span:nth-child(1) { background: #ff5f56; }
  .traffic-lights span:nth-child(2) { background: #ffbd2e; }
  .traffic-lights span:nth-child(3) { background: #27c93f; }

  .filename {
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
    font-family: var(--font-mono);
    font-size: 0.85rem;
    color: var(--text-muted);
  }

  .code-content {
    padding: 2rem;
    overflow-x: auto;
  }

  pre {
    margin: 0;
    font-family: var(--font-mono);
    color: #e4e4e7;
    font-size: 1rem;
  }

  @media (max-width: 600px) {
    .page-section {
      padding-top: 2rem;
      padding-bottom: 2rem;
    }
    
    h1 {
      font-size: 2.5rem;
    }
    
    .code-content {
      padding: 1rem;
    }
  }
</style>