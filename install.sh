#!/bin/sh
# Wasome Installer
set -e

# Configuration
GITHUB_REPO="${WASOME_REPO:-Dari-OS/Wasome}"
WASOME_HOME="${WASOME_HOME:-$HOME/.wasome}"
WASOME_BIN="$WASOME_HOME/bin"

# Detect OS and Architecture
OS="$(uname -s)"
ARCH="$(uname -m)"

if [ "$OS" = "Linux" ]; then
    if [ "$ARCH" = "x86_64" ]; then
        TARGET="x86_64-unknown-linux-gnu"
    elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
        TARGET="aarch64-unknown-linux-gnu"
    elif [ "$ARCH" = "armv7l" ]; then
        TARGET="armv7-unknown-linux-gnueabihf"
    elif [ "$ARCH" = "i686" ]; then
        TARGET="i686-unknown-linux-gnu"
    else
        echo "Unsupported architecture: $ARCH"
        exit 1
    fi
elif [ "$OS" = "Darwin" ]; then
    if [ "$ARCH" = "x86_64" ]; then
        TARGET="x86_64-apple-darwin"
    elif [ "$ARCH" = "arm64" ] || [ "$ARCH" = "aarch64" ]; then
        TARGET="aarch64-apple-darwin"
    else
        echo "Unsupported architecture: $ARCH"
        exit 1
    fi
elif [ "$OS" = "FreeBSD" ]; then
    if [ "$ARCH" = "x86_64" ] || [ "$ARCH" = "amd64" ]; then
        TARGET="x86_64-unknown-freebsd"
    else
        echo "Unsupported FreeBSD architecture: $ARCH"
        exit 1
    fi
else
    echo "Unsupported OS: $OS"
    exit 1
fi

echo "Detected Platform: $OS ($ARCH) -> $TARGET"

# Fetch latest version
echo "Fetching latest Wasome release from $GITHUB_REPO..."
LATEST_RELEASE=$(curl -s "https://api.github.com/repos/${GITHUB_REPO}/releases/latest" | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/')
if [ -z "$LATEST_RELEASE" ]; then
    echo "Failed to fetch latest release. Check your internet connection or GitHub API limits."
    exit 1
fi

DOWNLOAD_URL="https://github.com/${GITHUB_REPO}/releases/download/${LATEST_RELEASE}/wasome-${TARGET}.tar.gz"
DOWNLOAD_ARCHIVE="wasome.tar.gz"

echo "Downloading Wasome ${LATEST_RELEASE} from $DOWNLOAD_URL..."
curl -L -o "$DOWNLOAD_ARCHIVE" "$DOWNLOAD_URL"

mkdir -p "$WASOME_HOME"
if [ -d "$WASOME_HOME/bin" ]; then
    echo "Updating existing installation at $WASOME_HOME..."
else
    echo "Installing to $WASOME_HOME..."
fi

# Extract to a temporary directory to perform a smart merge
TEMP_DIR=$(mktemp -d)
tar -xzf "$DOWNLOAD_ARCHIVE" -C "$TEMP_DIR"
rm "$DOWNLOAD_ARCHIVE"

# Safely replace bin
rm -rf "$WASOME_HOME/bin"
mv "$TEMP_DIR/bin" "$WASOME_HOME/"

# Dynamically replace std contents (e.g. std/runtime) without removing user's custom stds
if [ -d "$TEMP_DIR/std" ]; then
    mkdir -p "$WASOME_HOME/std"
    for item in "$TEMP_DIR/std"/*; do
        if [ -e "$item" ]; then
            basename_item=$(basename "$item")
            rm -rf "$WASOME_HOME/std/$basename_item"
            mv "$item" "$WASOME_HOME/std/"
        fi
    done
fi

# Safely merge lib (preserving existing user-downloaded LLVM binaries)
if [ -d "$TEMP_DIR/lib" ]; then
    mkdir -p "$WASOME_HOME/lib"
    for item in "$TEMP_DIR/lib"/*; do
        if [ -e "$item" ]; then
            basename_item=$(basename "$item")
            rm -rf "$WASOME_HOME/lib/$basename_item"
            mv "$item" "$WASOME_HOME/lib/"
        fi
    done
fi

rm -rf "$TEMP_DIR"

# ---------------------------------------------------------
# TODO: Install precompiled LLVM/LLD binaries
# ---------------------------------------------------------
# The CD pipeline creates an empty `lib` folder.
# Download your platform-specific LLVM/LLD binaries here 
# and extract them into "$WASOME_HOME/lib".
# 
# Example:
# echo "Downloading LLVM binaries..."
# curl -L -o llvm.tar.gz "https://your-llvm-source.com/llvm-${TARGET}.tar.gz"
# tar -xzf llvm.tar.gz -C "$WASOME_HOME/lib"
# rm llvm.tar.gz
# ---------------------------------------------------------

# Setup PATH
echo "Configuring PATH..."
SHELL_NAME=$(basename "$SHELL")
PROFILE=""

if [ "$SHELL_NAME" = "zsh" ]; then
    PROFILE="$HOME/.zshrc"
elif [ "$SHELL_NAME" = "bash" ]; then
    if [ "$OS" = "Darwin" ]; then
        PROFILE="$HOME/.bash_profile"
    else
        PROFILE="$HOME/.bashrc"
    fi
elif [ "$SHELL_NAME" = "fish" ]; then
    PROFILE="$HOME/.config/fish/config.fish"
else
    PROFILE="$HOME/.profile"
fi

if [ "$SHELL_NAME" = "fish" ]; then
    if ! grep -q "WASOME_HOME" "$PROFILE" 2>/dev/null; then
        mkdir -p "$(dirname "$PROFILE")"
        echo "" >> "$PROFILE"
        echo "set -gx WASOME_HOME \"$WASOME_HOME\"" >> "$PROFILE"
        echo "set -gx PATH \"\$WASOME_HOME/bin\" \$PATH" >> "$PROFILE"
        echo "Added WASOME_HOME to $PROFILE"
    else
        echo "WASOME_HOME already exists in $PROFILE"
    fi
else
    if ! grep -q "WASOME_HOME" "$PROFILE" 2>/dev/null; then
        echo "" >> "$PROFILE"
        echo "export WASOME_HOME=\"$WASOME_HOME\"" >> "$PROFILE"
        echo 'export PATH="$WASOME_HOME/bin:$PATH"' >> "$PROFILE"
        echo "Added WASOME_HOME to $PROFILE"
    else
        echo "WASOME_HOME already exists in $PROFILE"
    fi
fi

echo ""
echo "Wasome installed/updated successfully!"
echo "Please restart your terminal or run the following command to start using Wasome:"
if [ "$SHELL_NAME" = "fish" ]; then
    echo "source $PROFILE"
else
    echo "source $PROFILE"
fi