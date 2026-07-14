export function compileViaWebSocket({ files, entry, turnstileToken, onLog, onWasm, onError, onDone }) {
  return new Promise((resolve) => {
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const ws = new WebSocket(`${protocol}//${location.host}/ws/compile`);
    let wasmBytes = null;

    ws.binaryType = 'arraybuffer';

    ws.onopen = () => {
      ws.send(JSON.stringify({
        files,
        entry,
        turnstile_token: turnstileToken
      }));
    };

    ws.onmessage = (event) => {
      if (event.data instanceof ArrayBuffer) {
        wasmBytes = new Uint8Array(event.data);
        onWasm?.(wasmBytes);
      } else {
        try {
          const msg = JSON.parse(event.data);
          if (msg.type === 'log') onLog?.(msg.data);
          else if (msg.type === 'status') onLog?.(msg.message);
          else if (msg.type === 'error') onError?.(msg.message, msg.status);
          else if (msg.type === 'session' && msg.cookie) {
            document.cookie = msg.cookie.replace(/HttpOnly;?/i, '');
          }
          else if (msg.type === 'done') onDone?.(msg.success, wasmBytes);
        } catch {
          onLog?.(event.data);
        }
      }
    };

    ws.onerror = () => {
      onError?.('Connection failed');
      resolve();
    };

    ws.onclose = () => {
      resolve();
    };
  });
}
