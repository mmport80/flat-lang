{
  description = "A Haskell project with Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [];
      };
      myHaskellEnv = pkgs.callPackage ./default.nix {};
    in {
      devShell.${system} = myHaskellEnv;
    };
}
  
