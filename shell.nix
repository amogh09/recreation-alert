{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/982816eaeefe3b7c5d51afc2ba777e627a98c4ac.tar.gz";
    sha256 = "sha256:17g48gx9ya72d5gq1imzk94k0m1vzf5ghwzawh0craqjil7yc0m7";
}) {} }:
pkgs.mkShell {
  packages = with pkgs; [
    (haskell.packages.ghc927.ghcWithPackages
      (pkgs: with pkgs;
      [ cabal-install haskell-language-server ormolu ]))
  ];
  shellHook = ''
    export PATH="$HOME/.cabal/bin:$PATH"
  '';
}
