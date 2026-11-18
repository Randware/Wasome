#!/bin/sh
set -eu

tar --extract --file=- --directory=/workspace --no-same-owner --no-same-permissions
timeout --kill-after=1s 18s /opt/wasome/bin/waso build --target web --profile size /workspace >&2

for artifact in /workspace/out/*.wasm; do
    if [ -f "$artifact" ]; then
        cat -- "$artifact"
        exit
    fi
done

printf '%s\n' 'Compiler did not produce a WebAssembly module' >&2
exit 1
