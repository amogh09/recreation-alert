{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages
      (pkgs: with pkgs;
      [ cabal-install haskell-language-server ormolu ]))
    zlib
  ];
  shellHook = ''
    export PATH="$HOME/.cabal/bin:$PATH"
  '';
}
