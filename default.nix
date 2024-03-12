{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell-language-server pkgs.ormolu pkgs.ghcid
    (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
      ad normaldistribution lens random-fu safe QuickCheck
      
      # Add other Haskell dependencies here
    ]))
  ];
}
  
