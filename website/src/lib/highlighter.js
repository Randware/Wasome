export function highlight(code) {
  if (!code) return '';

  // Escape HTML to prevent injection and rendering issues
  let result = code
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");

  // Token patterns
  // We use a specific order. Strings/Comments first to capture content that shouldn't be tokenized as keywords.
  const patterns = [
    // Comments: // ...
    { regex: /(\/\/.*$)/gm, class: 'hl-comment' },
    
    // Strings: "..."
    { regex: /("[^"]*")/g, class: 'hl-string' },
    
    // Keywords
    { regex: /\b(fn|struct|enum|if|else|loop|break|return|import|new|true|false)\b/g, class: 'hl-keyword' },
    
    // Types
    { regex: /\b(s8|s16|s32|s64|u8|u16|u32|u64|f32|f64|bool|char|void)\b/g, class: 'hl-type' },

    // Functions
    { regex: /\b([a-zA-Z_][a-zA-Z0-9_]*)(?=\()/g, class: 'hl-function' },

    // Numbers
    { regex: /\b(\d+(\.\d+)?)\b/g, class: 'hl-number' },
  ];

  // Replacement strategy:
  // 1. Find all matches for all patterns.
  // 2. Sort by position.
  // 3. Apply non-overlapping matches.
  
  // Actually, a simpler strategy for this level of complexity:
  // Replace with temporary placeholders to avoid re-matching inside HTML tags.
  
  let tokens = [];
  let tokenIndex = 0;
  
  const save = (match, cls) => {
    const key = `__TOKEN_${tokenIndex++}__`;
    tokens.push({ key, val: `<span class="${cls}">${match}</span>` });
    return key;
  };

  // 1. Comments (consume entire line)
  result = result.replace(/(\/\/.*$)/gm, m => save(m, 'hl-comment'));
  
  // 2. Strings
  result = result.replace(/("[^"]*")/g, m => save(m, 'hl-string'));

  // 3. Keywords & Types & Funcs & Numbers
  // We can do these safely now that strings/comments are hidden
  result = result.replace(/\b(fn|struct|enum|if|else|loop|break|return|import|new|true|false)\b/g, m => save(m, 'hl-keyword'));
  result = result.replace(/\b(s8|s16|s32|s64|u8|u16|u32|u64|f32|f64|bool|char|void)\b/g, m => save(m, 'hl-type'));
  result = result.replace(/\b([a-zA-Z_][a-zA-Z0-9_]*)(?=\()/g, m => save(m, 'hl-function'));
  result = result.replace(/\b(\d+(\.\d+)?)\b/g, m => save(m, 'hl-number'));

  // Restore
  tokens.forEach(t => {
    result = result.replace(t.key, t.val);
  });

  return result;
}