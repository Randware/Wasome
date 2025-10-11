# Version of the Rust image that will be used
# Check https://github.com/devcontainers/images/tree/main/src/rust for details
# We also use a semantic versioning which will not specify the PATCH version, allowing us to receive backwards compatible bug fixes
# Check https://github.com/devcontainers/images/blob/main/src/rust/history/2.0.2.md for version details
ARG IMAGE_VERSION="2.0-1-trixie"

FROM mcr.microsoft.com/devcontainers/rust:${IMAGE_VERSION}

# LLVM major version that will be installed
ARG LLVM_VERSION="21"

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Set bash as the default shell
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

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

  # Cleanup
  apt-get clean
  rm -rf /var/lib/apt/lists/*
EOF
