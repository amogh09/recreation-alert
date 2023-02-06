{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
        haskell.compiler.ghc924
        haskellPackages.haskell-language-server
        cabal-install
    ];
}
