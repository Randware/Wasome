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
  * **Default/Local Testing**: `1x00000000000000000000AA` (dummy bypass key).

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
    * *Production Note*: Docker Compose prefixes volume names with the directory/project name (e.g. `<project-name>_wasome-compiler-cache`). If your production folder name or Compose project name is not `website`, update this variable to match the exact volume name (e.g. `prod-deployment_wasome-compiler-cache`). Run `docker volume ls` on your server to verify the volume name.
  * **Default**: `website_wasome-compiler-cache`
* **`DOCKER_RUNTIME`**
  * **Description**: The container runtime to spawn sandboxes with.
  * **Default**: `runsc` (gVisor). Change to `runc` ONLY for local debugging if gVisor is not installed on the host.
* **`BIND_ADDR`**
  * **Description**: Port and network interface host mapping of the backend server.
  * **Default**: `0.0.0.0:3000`
* **`PORT`**
  * **Description**: The port number exposed on the host machine mapping to the Svelte frontend container's HTTP server (port 80 inside the container). Change this if the default port `8080` is already in use by another service on your system.
  * **Default**: `8080`

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

---

## 4. Production Deployment Guide

Follow these steps to deploy the playground securely on your production server:

### Step 1: Install gVisor on the Host OS
Since sandboxes are spawned as sibling containers on the host, the **Host OS** must have gVisor installed and configured in Docker.

#### Option A: Debian / Ubuntu (APT)
Run the following commands if your host system uses the APT package manager:
```bash
sudo apt-get update
sudo apt-get install -y apt-transport-https ca-certificates curl gnupg

# Add gVisor key and repository
curl -fsSL https://gvisor.dev/archive.key | sudo gpg --dearmor -o /usr/share/keyrings/gvisor-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/gvisor-archive-keyring.gpg] https://storage.googleapis.com/gvisor/releases release main" | sudo tee /etc/apt/sources.list.d/gvisor.list > /dev/null

# Install gVisor
sudo apt-get update && sudo apt-get install -y runsc

# Register runsc runtime in Docker daemon and restart
sudo runsc install
sudo systemctl restart docker
```

#### Option B: Arch Linux (AUR / Pacman)
On Arch Linux, gVisor's `runsc` can be built and installed via the AUR package `gvisor-bin`. Alternatively, download the static binaries directly and register them:
```bash
# Clone the AUR repository and build
git clone https://aur.archlinux.org/gvisor-bin.git
cd gvisor-bin
makepkg -si

# Register the runsc runtime inside Docker and restart
sudo runsc install
sudo systemctl restart docker
```

#### Option C: Alpine Linux (APK)
Since Alpine runs with `musl` libc, gVisor's statically compiled binaries are used since they are fully self-contained and run on any Linux kernel.
```bash
# Download the self-contained static binary
sudo wget -O /usr/local/bin/runsc https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc
sudo chmod a+rx /usr/local/bin/runsc

# Register the runsc runtime inside Docker and restart
sudo /usr/local/bin/runsc install
sudo service docker restart
```

#### Option D: Universal Manual Binary (RedHat, CentOS, Rocky, Alma, SUSE, NixOS, etc.)
If your host system does not use `apt` or you want a package manager independent installation, install from static binaries directly:
```bash
# Download the runsc binary and its checksum
curl -LO https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc
curl -LO https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc.sha256

# Verify the download checksum
sha256sum -c runsc.sha256

# Move runsc to your bin path and make it executable
chmod a+rx runsc
sudo mv runsc /usr/local/bin/

# Register the runsc runtime inside Docker daemon and restart
sudo /usr/local/bin/runsc install
sudo systemctl restart docker
```

*(Note for NixOS: Add `virtualisation.docker.extraOptions = "--add-runtime runsc=${pkgs.gvisor}/bin/runsc";` to your `/etc/nixos/configuration.nix` and run `sudo nixos-rebuild switch` instead of running commands manually).*

### Step 2: Configure Production Environment Variables
Create a production `.env` file and customize the following settings:
1. **Security & Secrets**:
   * Generate a secure admin token for `ADMIN_SECRET`.
   * Create Discord webhooks for real-time security alerts and paste them into `DISCORD_WEBHOOK_ALERT`, `DISCORD_WEBHOOK_CRITICAL`, and `DISCORD_WEBHOOK_EMERGENCY`.
2. **Cloudflare Turnstile site & secret keys**:
   * Create site credentials at the [Cloudflare Dashboard](https://dash.cloudflare.com/) and replace the dummy site/secret keys.
3. **Sandbox Runtime**:
   * Ensure `DOCKER_RUNTIME=runsc` is set to activate gVisor syscall interception.
   * *Fallback*: If you cannot install gVisor on the host (e.g. for testing/dev environments), set `DOCKER_RUNTIME=runc`. **WARNING: Using `runc` in production is insecure** because sandbox containers will share the host kernel directly without kernel isolation, leaving the server vulnerable to kernel exploits.
4. **Volume Name Alignment**:
   * Run `docker volume ls` to check the exact volume name prefix generated by Docker Compose. By default, it prefixes volumes with the directory name. If your production folder is not named `website` (e.g., you cloned into `wasome-prod`), update `HOST_WASOME_DIR` in `.env` to match the exact volume name (e.g. `wasome-prod_wasome-compiler-cache`).

### Step 3: Run the Application
Start the deployment using Docker Compose:
```bash
docker compose up -d
```
Verify the backend container logs to ensure the compiler installs successfully:
```bash
docker compose logs -f backend
```

---

### Admin API Management

You can toggle `compilation_enabled` and `maintenance_mode` at runtime without restarting any services by calling the `POST /api/admin/toggle` endpoint. This endpoint requires you to pass your configured `ADMIN_SECRET` in the `X-Admin-Secret` header.

#### cURL Example
To turn off compilation and activate maintenance mode:
```bash
curl -X POST http://localhost:8080/api/admin/toggle \
  -H "Content-Type: application/json" \
  -H "X-Admin-Secret: your_secret_here" \
  -d '{"compilation_enabled": false, "maintenance_mode": true}'
```

