const grammars = {
  wasome: {
    comments: [ /(\/\/.*$)/gm ],
    strings: [ /("[^"]*")/g ],
    tokens: [
      { regex: /\b(fn|struct|enum|if|else|loop|break|return|import|new|true|false)\b/g, class: 'hl-keyword' },
      { regex: /\b(s8|s16|s32|s64|u8|u16|u32|u64|f32|f64|bool|char|void)\b/g, class: 'hl-type' },
      { regex: /\b([a-zA-Z_][a-zA-Z0-9_]*)(?=\()/g, class: 'hl-function' },
      { regex: /\b(\d+(\.\d+)?)\b/g, class: 'hl-number' }
    ]
  },
  bash: {
    comments: [ /(#.*$)/gm ],
    strings: [ /("[^"]*")/g, /('[^']*')/g ],
    tokens: [
      { regex: /\b(curl|sh|sudo|apt|git|npm|node|echo|cd|ls|mkdir|rm)\b/g, class: 'hl-keyword' },
      { regex: /(https?:\/\/[^\s]+)/g, class: 'hl-string' },
      { regex: /(-[a-zA-Z]+|--[a-zA-Z0-9-]+)/g, class: 'hl-type' },
      { regex: /(\|)/g, class: 'hl-keyword' },
      { regex: /\b(\d+)\b/g, class: 'hl-number' }
    ]
  },
  powershell: {
    comments: [ /(#.*$)/gm ],
    strings: [ /("[^"]*")/g, /('[^']*')/g ],
    tokens: [
      { regex: /\b(iwr|iex|Invoke-WebRequest|Invoke-Expression|Write-Host|Get-Command)\b/gi, class: 'hl-keyword' },
      { regex: /(https?:\/\/[^\s]+)/g, class: 'hl-string' },
      { regex: /(\|)/g, class: 'hl-keyword' },
      { regex: /(-[a-zA-Z]+)\b/g, class: 'hl-type' }
    ]
  }
};

export function highlight(code, lang = 'wasome') {
  if (!code) return '';

  // Escape HTML to prevent injection and rendering issues
  let result = code
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");

  const grammar = grammars[lang] || grammars.wasome;

  let tokens = [];
  let tokenIndex = 0;
  
  const save = (match, cls) => {
    const key = `__TOKEN_${tokenIndex++}__`;
    tokens.push({ key, val: `<span class="${cls}">${match}</span>` });
    return key;
  };

  // 1. Comments
  if (grammar.comments) {
    grammar.comments.forEach(regex => {
      result = result.replace(regex, m => save(m, 'hl-comment'));
    });
  }
  
  // 2. Strings
  if (grammar.strings) {
    grammar.strings.forEach(regex => {
      result = result.replace(regex, m => save(m, 'hl-string'));
    });
  }

  // 3. Tokens (Keywords, Types, etc.)
  if (grammar.tokens) {
    grammar.tokens.forEach(token => {
      result = result.replace(token.regex, m => save(m, token.class));
    });
  }

  // Restore
  tokens.forEach(t => {
    result = result.replace(t.key, t.val);
  });

  return result;
}