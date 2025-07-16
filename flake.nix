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

    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ];
      
    in {
    devShells = forAllSystems (system:
      let pkgs = import nixpkgs { inherit system; }; in {
        default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell-language-server 
            pkgs.ormolu 
            pkgs.ghcid 
            pkgs.git
            pkgs.starship
            (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
              ad 
              normaldistribution 
              lens 
              random-fu 
              safe 
              QuickCheck
              megaparsec
              parser-combinators
            ]))
        ];
        
        shellHook = ''
          eval "$(starship init bash)"
        '';
      };
    }
  );
};
}
