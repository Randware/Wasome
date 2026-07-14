# Wasome Website Architecture & Deployment

This directory contains the Wasome frontend website (Svelte/Vite) and the highly secure Rust backend compiler API (`/backend`).

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

If you do not install `runsc` on your host machine, you must set `DOCKER_RUNTIME=runc` in your `.env` for local testing (this runs sandboxes with standard Docker runtimes, which lacks syscall interception and should not be used in production).

---

## 2. Environment Variables Reference

Copy `.env.example` to `.env` to configure the application:
```bash
cp .env.example .env
```

Below is the complete configuration guide for every environment variable supported by the system.

### Cloudflare Turnstile Config (Bot Protection)

Cloudflare Turnstile provides frictionless bot protection for playground runs.

* **`PUBLIC_TURNSTILE_SITE_KEY`**
  * **Description**: The public key loaded by Svelte for rendering the Turnstile widget in the browser.
  * **Where to find**:
    1. Log in to your [Cloudflare Dashboard](https://dash.cloudflare.com/).
    2. Click **Turnstile** in the left-hand sidebar menu.
    3. Click **Add Site**.
    4. Fill in the Site Name, enter your Domain (e.g. `localhost` for local development, or your production domain), and select **Widget Type** (e.g. *Managed* or *Invisible*).
    5. Click **Create** to receive your credentials. Use the **Site Key** here.
  * **Default/Local Testing**: `1x0000000000000000000001` (dummy bypass key).

* **`TURNSTILE_SECRET_KEY`**
  * **Description**: The secret key loaded by the Rust backend to verify Turnstile challenge tokens server-side.
  * **Where to find**: Retrieved from the same Turnstile Site settings page in the Cloudflare Dashboard (step 5 above). Use the **Secret Key** here.
  * **Default/Local Testing**: `1x0000000000000000000000000000000AA` (dummy bypass key).

---

### Rate Limiting

The backend uses a Token Bucket rate limiter per IP address to prevent brute-force compilation attempts.

* **`RATE_LIMIT_MAX_TOKENS`**
  * **Description**: Maximum number of compilation requests allowed in a burst before the IP gets rate-limited.
  * **Default**: `10`
* **`RATE_LIMIT_REFILL_PERIOD`**
  * **Description**: The duration (in seconds) it takes to refill the bucket back to full capacity.
  * **Default**: `900` (15 minutes)

---

### Security Alerts & Webhooks (Discord Integration)

The Rust backend has an attack detection engine that tracks malicious events (rate limit hits, path traversal attempts, payload overflows). When an IP's threat score exceeds thresholds, it automatically triggers a ban and fires notifications.

* **`DISCORD_WEBHOOK_ALERT`**
  * **Description**: Discord webhook URL for Warning-level events (IP threat score >= 50).
* **`DISCORD_WEBHOOK_CRITICAL`**
  * **Description**: Discord webhook URL for Critical-level events and 1-hour IP bans (threat score >= 100).
* **`DISCORD_WEBHOOK_EMERGENCY`**
  * **Description**: Discord webhook URL for Emergency-level events and 24-hour IP bans (threat score >= 200).
* **Where to find Webhook URLs**:
  1. Open your Discord server.
  2. Right-click the target text channel and select **Edit Channel**.
  3. Go to **Integrations** -> **Webhooks** -> **New Webhook**.
  4. Copy the Webhook URL and paste it into the respective `.env` variable. Leave blank to disable alerts.

---

### Admin & Compiler Paths

* **`ADMIN_SECRET`**
  * **Description**: Token header secret used to authenticate administrative requests.
  * **How to use**: Pass this key as an `x-admin-secret` HTTP header when making requests to endpoints like `POST /api/admin/toggle` to toggle compiler activity.
* **`HOST_WASOME_DIR`**
  * **Description**: The name of the Docker volume (or host path) containing the pre-compiled waso toolchain. This is mounted read-only into sandbox containers to allow compilation.
  * **Default**: `website_wasome-compiler-cache`
* **`DOCKER_RUNTIME`**
  * **Description**: The container runtime to spawn sandboxes with.
  * **Default**: `runsc` (gVisor). Change to `runc` ONLY for local debugging if gVisor is not installed on the host.
* **`BIND_ADDR`**
  * **Description**: Port and network interface host mapping of the backend server.
  * **Default**: `0.0.0.0:3000`

---

## 3. How to Deploy Locally

To test the frontend and backend locally with Docker Compose:

1. Make sure your host has `runsc` installed (see step 1) and your `.env` file exists.
2. Run `docker-compose`:
   ```bash
   docker-compose up --build
   ```

**What happens?**
* The `website` container runs Nginx, serving the Svelte frontend on port `8080`.
* The `backend` container runs the Rust API on port `3000`. Nginx automatically reverse-proxies `/api/` traffic to the backend.
* The backend downloads and installs the toolchain locally via `/app/install.sh` on startup, storing it inside the shared volume.
* When a user runs code, the `backend` mounts the shared volume directly into an ephemeral `runsc` sandbox container and executes it safely.

You can now visit `http://localhost:8080/playground` to interact with the system safely!
