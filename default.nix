{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell-language-server pkgs.ormolu pkgs.ghcid pkgs.git
    (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
      ad normaldistribution lens random-fu safe QuickCheck 
      
      # Add other Haskell dependencies here
      megaparsec       # Add this for parsing
      parser-combinators
    ]))
  ];
}
  
