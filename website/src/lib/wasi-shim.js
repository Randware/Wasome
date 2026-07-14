export class WasiExit {
  constructor(code) {
    this.code = code;
  }
}

export function createWasiShim() {
  let stdout = '';
  let stderr = '';
  let memory = null;
  let exitCode = null;

  function setMemory(mem) {
    memory = mem;
  }

  function getOutput() {
    return { stdout, stderr, exitCode };
  }

  function reset() {
    stdout = '';
    stderr = '';
    exitCode = null;
  }

  const imports = {
    wasi_snapshot_preview1: {
      fd_write(fd, iovs_ptr, iovs_len, nwritten_ptr) {
        const view = new DataView(memory.buffer);
        let totalWritten = 0;
        for (let i = 0; i < iovs_len; i++) {
          const ptr = view.getUint32(iovs_ptr + i * 8, true);
          const len = view.getUint32(iovs_ptr + i * 8 + 4, true);
          const bytes = new Uint8Array(memory.buffer, ptr, len);
          const text = new TextDecoder().decode(bytes);
          if (fd === 1) stdout += text;
          else if (fd === 2) stderr += text;
          totalWritten += len;
        }
        view.setUint32(nwritten_ptr, totalWritten, true);
        return 0;
      },
      fd_read(fd, iovs_ptr, iovs_len, nread_ptr) {
        const view = new DataView(memory.buffer);
        view.setUint32(nread_ptr, 0, true);
        return 0;
      },
      proc_exit(code) {
        exitCode = code;
        throw new WasiExit(code);
      }
    },
    __wbindgen_placeholder__: new Proxy({}, {
      get(target, prop) {
        if (typeof prop === 'string' && prop.startsWith('__wbg_print_str')) {
          return (ptr, len) => {
            if (!memory) return;
            const bytes = new Uint8Array(memory.buffer, ptr, len);
            const text = new TextDecoder().decode(bytes);
            stdout += text;
          };
        }
        return () => {};
      }
    }),
    __wbindgen_externref_xform__: new Proxy({}, {
      get(target, prop) {
        return () => {};
      }
    })
  };

  return { imports, setMemory, getOutput, reset };
}
