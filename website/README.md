# Wasome Website Architecture & Deployment

This directory contains the Wasome frontend website (SvelteKit/Vite) and the highly secure Rust backend compiler API (`/backend`).

## Security Architecture

The backend compiler service is hardened using a three-tier security model:
1. **Frontend DoS Prevention:** Invisible Cloudflare Turnstile token flow.
2. **Backend Gateway:** Payload size limits (100KB), strict path validation, file limit boundaries.
3. **Execution Sandbox:** The backend spawns sibling Docker containers via the mounted Docker socket (`/var/run/docker.sock`). These sandbox containers use **gVisor (`runsc`)** to intercept all syscalls and prevent kernel exploits. They have zero network access, read-only filesystems, and strict CPU/Memory quotas.

---

## 1. Prerequisites (gVisor Host Installation)

Because the backend service mounts the host's Docker socket to spawn sandboxes, **gVisor must be installed on your Host OS.** It cannot be installed *inside* the backend container.

### Install `runsc` on Ubuntu/Debian Host:
```bash
sudo apt-get update
sudo apt-get install -y apt-transport-https ca-certificates curl gnupg

curl -fsSL https://gvisor.dev/archive.key | sudo gpg --dearmor -o /usr/share/keyrings/gvisor-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/gvisor-archive-keyring.gpg] https://storage.googleapis.com/gvisor/releases release main" | sudo tee /etc/apt/sources.list.d/gvisor.list > /dev/null

sudo apt-get update && sudo apt-get install -y runsc

# Configure Docker daemon to recognize 'runsc' and restart
sudo runsc install
sudo systemctl restart docker
```

If you do not install `runsc` on your host machine, the backend will fail to start the compilation containers because the `--runtime=runsc` flag will be missing from your Docker daemon.

---

## 2. Environment Variables & Cloudflare Turnstile

The website requires environment variables to configure Cloudflare Turnstile bot protection.

1. Copy the `.env.example` file to `.env`:
   ```bash
   cp .env.example .env
   ```
2. For **local testing**, leave the default dummy keys (they are guaranteed to pass validation without user interaction).
3. For **production**, sign up at [Cloudflare Turnstile](https://dash.cloudflare.com/?to=/:account/turnstile), register your domain, and replace the keys:
   - `PUBLIC_TURNSTILE_SITE_KEY` goes in the `.env` (loaded by Vite for the browser).
   - `TURNSTILE_SECRET_KEY` goes in the `.env` (loaded by the backend Rust API).

---

## 3. How to Deploy Locally

To test the frontend and backend locally with Docker Compose:

1. Make sure your host has `runsc` installed (see step 1) and your `.env` file exists.
2. We need the actual `wasome/compiler` image. If it isn't built yet, ensure your local Docker registry has an image tagged `wasome/compiler:latest`. Since the backend runs Debian Bookworm, it requires an image with compatible glibc. If you don't have it, create a dummy tag for testing:
   ```bash
   docker pull debian:bookworm-slim
   docker tag debian:bookworm-slim wasome/compiler:latest
   ```
3. Run `docker-compose`:
   ```bash
   docker-compose up --build
   ```

**What happens?**
* The `website` container runs Nginx, serving the Svelte frontend on port `8080`.
* The `backend` container runs the Rust API on port `3000`. Nginx automatically reverse-proxies `/api/` traffic to the backend.
* The backend spawns background jobs every 2 hours to update the compiler locally via `/app/install.sh`.
* When a user runs code, the `backend` mounts the `waso` binaries directly into an ephemeral `runsc` container and executes it safely.

You can now visit `http://localhost:8080/playground` to interact with the system safely!
