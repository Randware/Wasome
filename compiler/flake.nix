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

      # auto-updating latest stable rust w/ lsp support
      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" "rust-analyzer" ];
      };

      # llvm 21
      llvmPkg = pkgs.llvmPackages_21; 

    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "llvm21-rust-latest";

        nativeBuildInputs = with pkgs; [
          pkg-config
          cmake
          rustToolchain
        ];

        # sys deps matching dockerfile (zlib, zstd, etc)
        buildInputs = with pkgs; [
          stdenv.cc.cc.lib
          zlib
          zstd
          openssl
          llvmPkg.libllvm
          llvmPkg.llvm
          llvmPkg.clang
        ];

        shellHook = ''
          # fix llvm-sys crate finding nix paths
          export LLVM_SYS_211_PREFIX="${llvmPkg.libllvm.dev}"
          export LIBCLANG_PATH="${llvmPkg.libclang.lib}/lib"
          
          # fix dynamic linking
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
