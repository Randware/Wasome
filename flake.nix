{
  description = "latest rust + llvm 21 env";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, rust-overlay, ... }:
    let
      system = "x86_64-linux";
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };

      # auto-updating latest stable rust & lsp support     
      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ 
          "rust-src"
          "rust-analyzer"
        ];
        targets = [ "wasm32-wasip1" ];
      };
      
      llvmPkg = pkgs.llvmPackages_21;

      # Build the compiler for a given pkgs set (native or cross).
      # The key insight for cross builds: nativeBuildInputs run on the HOST,
      # so we always use the host pkgs' llvm-config. buildInputs are for the
      # TARGET, so we use crossPkgs there.
      makePackage = crossPkgs:
        let
          crossLlvm = crossPkgs.llvmPackages_21;
        in
        crossPkgs.rustPlatform.buildRustPackage {
          pname = "wasome-compiler";
          version = "0.1.0";
          src = ./.;

          postUnpack = "sourceRoot+=/cli";

          cargoLock = {
            lockFile = ./cli/Cargo.lock;
          };

          nativeBuildInputs = with pkgs; [
            pkg-config
            cmake
            llvmPkg.llvm
          ];

          buildInputs = with crossPkgs; [
            zlib
            zstd
            openssl
            libffi
            libxml2
            ncurses
            llvmPackages_21.libllvm
          ];

          LLVM_SYS_211_PREFIX = "${llvmPkg.libllvm.dev}";
          LIBCLANG_PATH = "${llvmPkg.libclang.lib}/lib";
          NIX_LDFLAGS = "-L${crossLlvm.libllvm}/lib";
        };

      crossTargets = {
        "aarch64-linux" = import nixpkgs {
          localSystem = system;
          crossSystem = nixpkgs.lib.systems.examples.aarch64-multiplatform;
          overlays = [ (import rust-overlay) ];
        };
        "armv7l-linux" = import nixpkgs {
          localSystem = system;
          crossSystem = nixpkgs.lib.systems.examples.armv7l-hf-multiplatform;
          overlays = [ (import rust-overlay) ];
        };
        "i686-linux" = import nixpkgs {
          localSystem = system;
          crossSystem = nixpkgs.lib.systems.examples.gnu32;
          overlays = [ (import rust-overlay) ];
        };
        "riscv64-linux" = import nixpkgs {
          localSystem = system;
          crossSystem = nixpkgs.lib.systems.examples.riscv64;
          overlays = [ (import rust-overlay) ];
        };
        "x86_64-freebsd" = import nixpkgs {
          localSystem = system;
          crossSystem = { config = "x86_64-unknown-freebsd"; };
          overlays = [ (import rust-overlay) ];
        };
      };
    in
    {
      packages.${system}.default = makePackage pkgs;
      packages."aarch64-linux".default = makePackage crossTargets."aarch64-linux";
      packages."armv7l-linux".default  = makePackage crossTargets."armv7l-linux";
      packages."i686-linux".default    = makePackage crossTargets."i686-linux";
      packages."riscv64-linux".default = makePackage crossTargets."riscv64-linux";
      packages."x86_64-freebsd".default = makePackage crossTargets."x86_64-freebsd";

      devShells.${system}.default = pkgs.mkShell {
        name = "llvm21-rust-latest";
        nativeBuildInputs = with pkgs; [
          pkg-config
          cmake
          rustToolchain
          llvmPkg.lld
        ];
        buildInputs = with pkgs; [
          stdenv.cc.cc.lib
          zlib
          zstd
          openssl
          libffi
          libxml2
          ncurses
          llvmPkg.libllvm
          llvmPkg.llvm
          llvmPkg.clang
        ];
        shellHook = ''
          export LLVM_SYS_211_PREFIX="${llvmPkg.libllvm.dev}"
          export LIBCLANG_PATH="${llvmPkg.libclang.lib}/lib"
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
            pkgs.zlib
            pkgs.zstd
            pkgs.openssl
            llvmPkg.libllvm
          ]}:$LD_LIBRARY_PATH"
        '';
      };
    };
}
