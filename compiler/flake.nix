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
      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" "rust-analyzer" ];
      };
      # llvm 21
      llvmPkg = pkgs.llvmPackages_21;

      # Build the compiler package for a given cross pkgs set
      makePackage = crossPkgs:
        let
          crossRust = crossPkgs.rust-bin.stable.latest.default;
          crossLlvm = crossPkgs.llvmPackages_21;
        in
        crossPkgs.rustPlatform.buildRustPackage {
          pname = "wasome-compiler";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;

          nativeBuildInputs = with crossPkgs; [
            pkg-config
            cmake
            llvmPackages_21.llvm
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

          LLVM_SYS_211_PREFIX = "${crossLlvm.libllvm.dev}";
          LIBCLANG_PATH = "${crossLlvm.libclang.lib}/lib";
        };

      # Cross pkgs for each target
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
        "riscv64-linux" = import nixpkgs {
          localSystem = system;
          crossSystem = nixpkgs.lib.systems.examples.riscv64;
          overlays = [ (import rust-overlay) ];
        };
      };
    in
    {
      # Native package
      packages.${system}.default = makePackage pkgs;

      # Cross-compiled packages
      packages."aarch64-linux".default = makePackage crossTargets."aarch64-linux";
      packages."armv7l-linux".default  = makePackage crossTargets."armv7l-linux";
      packages."riscv64-linux".default = makePackage crossTargets."riscv64-linux";

      # Dev shell (unchanged)
      devShells.${system}.default = pkgs.mkShell {
        name = "llvm21-rust-latest";
        nativeBuildInputs = with pkgs; [
          pkg-config
          cmake
          rustToolchain
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
