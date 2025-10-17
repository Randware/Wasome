# We use Ubuntu 25.04 as our base, since we need more recent packages for LLVM 21
FROM ubuntu:25.04

# LLVM major version that will be installed
# We specify the MAJOR version only, so we can receive backwards compatible updates
# For details, see here: https://semver.org/
ARG LLVM_VERSION="21"

# Rust MINOR version that will be installed
# We specify the MINOR version only, so we can receive backwards compatible bug fixes
# For details, see here: https://semver.org/
ARG RUST_VERSION="1.90"

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Set bash as the default shell
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Update and install system packages
RUN <<EOF
  "Updating and installing system packages"
  set -eux 
  
  apt-get update
  apt-get upgrade -y

  apt-get install wget curl lsb-release build-essential software-properties-common gnupg -y
EOF

# Install LLVM using debian install script
# Check https://apt.llvm.org/ for details
RUN <<EOF 
  "Installing LLVM ${LLVM_VERSION}"
  set -eux

  wget -O /tmp/llvm.sh https://apt.llvm.org/llvm.sh
  chmod +x /tmp/llvm.sh
  /tmp/llvm.sh "${LLVM_VERSION}" all
  rm -f /tmp/llvm.sh

  # Verify installation
  llvm-config-${LLVM_VERSION} --version # Show installed LLVM version
  dpkg -l | grep llvm || true # Show installed LLVM components
EOF

# Export Rust installation location environment variables
ENV RUSTUP_HOME=/usr/local/rustup 
ENV CARGO_HOME=/usr/local/cargo
ENV PATH=/usr/local/cargo/bin:/root/.cargo/bin:$PATH

# Install rustup and use that to install the Rust toolchain
RUN <<EOF
  "Installing Rust ${RUST_VERSION}"

  # Install rustup
  # We install inside a superuser shell, so it is installed system-wide
  # We do not modify the path, since we did already
  # Additionally, we also provide the default toolchain, so it is installed
  # and configured as the default right away
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
  --no-modify-path \
  --default-toolchain ${RUST_VERSION}

  # Verify installation
  rustc --version || true
  cargo --version || true
EOF

# Clean up system to minimize container size
RUN <<EOF
  "Performing clean up"

  # Remove unnecessary packages
  apt-get autoremove

  # Remove package file cache
  apt-get clean

  # Clean apt lists, since the containers packages will not be updated
  rm -rf /var/lib/apt/lists/*
EOF
