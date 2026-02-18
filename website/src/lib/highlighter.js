const grammars = {
  wasome: {
    comments: [/(\/{2}.*$)/gm],
    strings: [/("[^"]*")/g],
    tokens: [
      { regex: /\b(fn|struct|enum|if|else|loop|break|return|import|new|true|false)\b/g, class: 'hl-keyword' },
      { regex: /\b(s8|s16|s32|s64|u8|u16|u32|u64|f32|f64|bool|char|void)\b/g, class: 'hl-type' },
      { regex: /\b([a-zA-Z_][a-zA-Z0-9_]*)(?=\()/g, class: 'hl-function' },
      { regex: /\b(\d+(\.\d+)?)\b/g, class: 'hl-number' }
    ]
  },
  bash: {
    comments: [/(#.*$)/gm],
    strings: [/("[^"]*")/g, /('[^']*')/g],
    tokens: [
      { regex: /\b(curl|sh|sudo|apt|git|npm|node|echo|cd|ls|mkdir|rm)\b/g, class: 'hl-keyword' },
      { regex: /(https?:\/\/[^\s]+)/g, class: 'hl-string' },
      { regex: /(-[a-zA-Z]+|--[a-zA-Z0-9-]+)/g, class: 'hl-type' },
      { regex: /(\|)/g, class: 'hl-keyword' },
      { regex: /\b(\d+)\b/g, class: 'hl-number' }
    ]
  },
  powershell: {
    comments: [/(#.*$)/gm],
    strings: [/("[^"]*")/g, /('[^']*')/g],
    tokens: [
      { regex: /\b(iwr|iex|Invoke-WebRequest|Invoke-Expression|Write-Host|Get-Command)\b/gi, class: 'hl-keyword' },
      { regex: /(https?:\/\/[^\s]+)/g, class: 'hl-string' },
      { regex: /(\|)/g, class: 'hl-keyword' },
      { regex: /(-[a-zA-Z]+)\b/g, class: 'hl-type' }
    ]
  },
  toml: {
    comments: [/(#.*$)/gm],
    strings: [/("""[\s\S]*?""")/g, /("(?:[^"\\]|\\.)*")/g, /('(?:[^'\\]|\\.)*')/g],
    tokens: [
      { regex: /(\[[\w.\-]+\])/g, class: 'hl-keyword' },
      { regex: /\b(true|false)\b/g, class: 'hl-keyword' },
      { regex: /^(\s*[\w.\-]+)\s*(?==)/gm, class: 'hl-type' },
      { regex: /\b(\d+(\.\d+)?)\b/g, class: 'hl-number' }
    ]
  },
  json: {
    comments: [],
    strings: [/("(?:[^"\\]|\\.)*")\s*(?=:)/g, /:\s*("(?:[^"\\]|\\.)*")/g],
    tokens: [
      { regex: /\b(true|false|null)\b/g, class: 'hl-keyword' },
      { regex: /\b(-?\d+(\.\d+)?([eE][+-]?\d+)?)\b/g, class: 'hl-number' }
    ]
  }
};

const extMap = {
  '.waso': 'wasome',
  '.toml': 'toml',
  '.json': 'json',
  '.sh': 'bash',
  '.ps1': 'powershell',
  '.md': 'wasome',
  '.txt': 'wasome'
};

export function detectLang(filename) {
  if (!filename) return 'wasome';
  const dot = filename.lastIndexOf('.');
  if (dot === -1) return 'wasome';
  return extMap[filename.substring(dot)] || 'wasome';
}

export function highlight(code, lang = 'wasome') {
  if (!code) return '';

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

  if (grammar.comments) {
    grammar.comments.forEach(regex => {
      result = result.replace(regex, m => save(m, 'hl-comment'));
    });
  }

  if (grammar.strings) {
    grammar.strings.forEach(regex => {
      result = result.replace(regex, m => save(m, 'hl-string'));
    });
  }

  if (grammar.tokens) {
    grammar.tokens.forEach(token => {
      result = result.replace(token.regex, m => save(m, token.class));
    });
  }

  tokens.forEach(t => {
    result = result.replace(t.key, t.val);
  });

  return result;
}